########################################
#                                      #
#         Sources of Strength          #
#   Data Preparation and Exploration   #
#                                      #
########################################


# ---- Importing Necessities --------------------

## Loading packages and functions
source("Scripts/PnF.R")

## creating codebooks
# dsclean <- read_sav("SoS Data and Codes/w1-w4 SoS_Cleaned_05-17-22.sav")
# imp <- read_sav("../02 Implementation Data W1-W4 cleaned/Quantitative_Implementation_SoS_ALL_WAVES_1-25-2022.sav")
# clim <- read_sav("../00 School Climate W1-W4 cleaned/CLEANED_school_climate_by_school_w1_w4.sav")
# 
# Create_Codebook(dsclean, export_type = "excel", export_name = "Student Survey Codebook.xlsx")
# Create_Codebook(imp, export_type = "excel", export_name = "Implementation Codebook.xlsx")
# Create_Codebook(clim, export_type = "excel", export_name = "School Climate Survey Codebook.xlsx")

## Importing data
# dsclean data fixed W2 out-of-range value issue
# dscombo <- read_sav("SoS Data and Codes/w1-w4 SoS_Combined.sav")
dsclean <- read_sav("SoS Data and Codes/w1-w4 SoS_Cleaned_05-17-22.sav")

# # comparing combo and clean
# incombo <- names(dscombo)[!names(dscombo) %in% names(dsclean)] #none
# inclean <- names(dsclean)[!names(dsclean) %in% names(dscombo)] # original id variable (SUBJECT_ID) and additional network variables
# table(dscombo$GEN_WELL_BEING_2_W2)
# table(dsclean$GEN_WELL_BEING_2_W2)


## renaming variables so they are included scale validation
dsclean <- dsclean %>%
  rename_with(.fn = ~str_remove(.x, "_NOTIN_SCALE"), .cols = contains("_NOTIN_SCALE")) %>% #starts_with("HELP_SEEK_GEN_BELIEF_NOTIN_SCALE_8")) %>%
  rename_with(.fn = ~str_remove(.x, "_NOT_IN_SCALE"), .cols = contains("_NOT_IN_SCALE")) %>% #starts_with("SEX_HAR_HELP_ATTITUDE_NOT_IN_SCALE_2"))
  rename(EXPOSURE_OWN_STRENGTHS_W2 = "EXPOSURE_OWN_SGRENGTHS_W2")

## question from Dorothy for meta analysis
# dorth <- dsclean %>%
#   filter(SampleInd_NetworkAnalyses_W1 == 1)
# table(dorth$TotOutW1, useNA = "always")
# table(dorth$outdegree_W1, useNA = "always")
# dorthout <- dorth %>%
#   filter(outdegree_W1 != 0)
# skimr::skim(dorthout$outdegree_W1)
# skimr::skim(dorthout$TotOutW1)
# summary(dorthout$outdegree_W1)
# IQR(dorthout$outdegree_W1)


## Keeping variables for analysis
ds1 <- dsclean %>%
  filter_at(vars(starts_with("Inaccurate_Response")),              # removing inaccurate response cases
            all_vars(is.na(.) | . == 2)) %>%  
  filter(!Grade %in% c(8, 12)) %>%                                 # removing the few students in grade 8 and 12 (keeping those with NA for grade)
  select(StudentID, School = SCHOOL_NAME_ALL,                      # Identifiers
         HasASurvey_W1:HasASurvey_W4,                              # Has Survey - Created by Kelly based on whether missing on Age, Friend1, or scale scores for general well-being, intent help others & help seeking general belief
         starts_with("CONDITION_"), starts_with("EXPOSURE"),       # Intervention indicator and exposure
         starts_with("AGE_"), Grade, one_of(DemoVars), PLIndEver,           # Demographics; GRADE_W1 = RGRADE_R1 (using Kelly's Grade variable instead)
         # Male, Female, OtherG, White, Hispanic, OtherR,          # Kelly's demo indicators do not include any missing responses in the original DemoVars
         # Straight, GayLes, Bisexual, OtherO,
         starts_with(c(NoContactPerpVars, ContactPerpVars,         # Primary Outcomes
                       NoContactVictVars, ContactVictVars,
                       CyberVars, DismisssvVars,
                       HNCPerpVars, HNCVictVars)),
         starts_with(c(WellBeingVars, HelpAttVars,                 # Secondary Outcomes (Protective Factors)
                       SHHelpAttitudeVars, SHHelpIntentVars,
                       HelpOthersVars, StaffIntentVars)),
         starts_with(c(AODVars, BullyPerpVars, CyberBullyPerpVars, # Other scales
                       DepAnxVars, PeerVictVars)),
         starts_with("TotAdultNoms_"), starts_with("SUICID")) %>%  # adult nominations and suicide variables
  filter(rowSums(select(., HasASurvey_W1:HasASurvey_W4)) != 0)     # these students (n = 1200) were only in data for network analysis (a named friend by someone with a survey)

## comparing how to best remove cases included just for network analysis, but did not take a survey
# networkcheck <- dscombo %>%
#   mutate(surveys = rowSums(select(., HasASurvey_W1:HasASurvey_W4)),
#          txs = rowSums(is.na(select(., starts_with("CONDITION_")))))
# table(networkcheck$surveys, networkcheck$txs, useNA = "always")
# # below illustrates that txs include people who did not respond to any item
# networkcheck %>% filter(surveys == 0 & txs == 3) %>% select(StudentID, SCHOOLSW1:VAPING_9_W4, surveys, txs) %>% View()

##########################################################

# ---- Recoding Time-Invariant Variables ------------------------

#### Investigating Treatment Condition Switching ####
## And creating final Tx indicator based on Intent-to-Treat framework

## Tx condition
TxTransfers <- ds1 %>%
  select(StudentID, starts_with("CONDITION")) %>%
  gather(Wave, CONDITION, -StudentID) %>%
  filter(!is.na(CONDITION)) %>%
  group_by(StudentID, CONDITION) %>%
  summarize(n_waves = n(), .groups = "drop_last") %>%
  mutate(n_cond = n())

## IDs of those who switched
TxTransferIDs <- TxTransfers[TxTransfers$n_cond == 2, ]$StudentID

lapply(paste0("CONDITION_W", 1:4), function(x) table(ds1$School, ds1[[x]], useNA = "always"))
# School 5 switched to SoS in W4

## Recoding Tx by school based on Intent-To-Treat framework
# consult exposure variables and implementation data to double-check if needed
WaitlistSchools <- c(1, 3, 4, 5, 9, 11, 17, 19, 20)

ds1 <- ds1 %>%
  mutate(Tx = case_when(School %in% WaitlistSchools ~ 0,
                        TRUE ~ 1)) %>%
  select(StudentID, School, Tx, starts_with("HasASurvey_"),
         Grade:SexOr, PLIndEver, starts_with("AGE_"), everything(), -starts_with("CONDITION"))


####       Demographic Variables       #####
## Initial frequencies
lapply(DemoVars, function(x) table(as_factor(ds1[[x]]), useNA = "always"))

## Collapsing demographic categories and creating dichotomous indicators for regression
ds1 <- ds1 %>%
  mutate(Gender = as_factor(Gender) %>% fct_recode(Other = "Multiple"),
         Female = ifelse(Gender == "Female", 1, 0),        # Male is the reference group
         Male = ifelse(Gender == "Male", 1, 0),
         OtherG = ifelse(Gender == "Other", 1, 0),
         Race = as_factor(Race) %>% fct_recode(Multiracial = "Multiple", Indigenous = "Islander"),
         White = ifelse(Race == "White", 1, 0),
         Latinx = ifelse(Race == "Hispanic", 1, 0),
         Black = ifelse(Race == "Black", 1, 0),
         Asian = ifelse(Race == "Asian", 1, 0),
         Multiracial = ifelse(Race == "Multiracial", 1, 0),
         Indigenous = ifelse(Race == "Indigenous", 1, 0),
         POC = case_when(is.na(Race) ~ NA_real_,
                          Race %in% c("White", "Hispanic") ~ 0,
                          TRUE ~ 1),
         SexOr = as_factor(SexOr) %>% fct_drop(),
         Straight = ifelse(SexOr == "Straight", 1, 0),
         Bisexual = ifelse(SexOr == "Bisexual", 1, 0),
         GL = ifelse(SexOr == "Gay/Lesbian", 1, 0),
         Questioning = ifelse(SexOr == "Questioning", 1, 0),
         OtherSO = ifelse(SexOr == "Other", 1, 0),
         LGBQ = ifelse(SexOr != "Straight", 1, 0)) %>%
  mutate(across(.cols = c(Female:LGBQ), ~Tx*., .names = "{col}xTx")) %>%
  mutate(across(.cols = starts_with("TotAdultNoms_"), ~ifelse(. > 0, 1, 0), .names = "Trusted_Adult_{col}")) %>%
  rename_with(.fn = ~str_remove(., "_TotAdultNoms"), .cols = starts_with("Trusted_Adult")) %>%
  select(StudentID:SexOr, PLIndEver, Female:LGBQxTx, everything())


####       Long format version       ####
ds1long <- LongWave(ds1, 1, StudentID:Tx, Grade:LGBQxTx) %>%
  bind_rows(LongWave(ds1, 2, StudentID:Tx, Grade:LGBQxTx),
            LongWave(ds1, 3, StudentID:Tx, Grade:LGBQxTx),
            LongWave(ds1, 4, StudentID:Tx, Grade:LGBQxTx)) %>%
  select(StudentID, Wave, everything())


#### Investigating HasASurvey Variable ####
## retaining original survey items
HAScheck <- ds1long %>% select(StudentID, Wave, HasASurvey, AGE:EXPOSURE_LOGO, -EXPOSURE_OTHER_TEXT)

## retaining cases with NA on all variables 
empties <- HAScheck[rowSums(is.na(HAScheck[,-c(1:3)])) == ncol(HAScheck[,-c(1:3)]),]
table(empties$HasASurvey, useNA = "always") # all were listed as not having a survey

## students not in empties, but listed as not having a survey
# View(ds1long[ds1long$HasASurvey==0 & !(ds1long$StudentID %in% empties$StudentID),])
# 3 cases have responses only to AOD items; not sure why

# CONCLUSION: HasASurvey by and large appears to be accurate; can be used for attrition analysis
#             When investigating missingness, use long version and remove HasASurvey == 0

###########################################################


# ------ Demographics Overall and by Condition -------------

gtdemos <- ds1 %>% select(all_of(DemoVars), Grade, AGE_W1, Tx) %>%
  mutate(Tx = ifelse(Tx == 1, "SoS", "Waitlist"),
         Transgender = as_factor(Transgender) %>% fct_drop()) %>%
  mutate(across(all_of(c(DemoVars, "Grade")), ~as_factor(.x) %>% fct_explicit_na(na_level = "Unknown"))) %>%
  tbl_summary(by = Tx,
              label = list(Transgender ~ "Transgender (recoded across waves)",
                           Grade ~ "Grade (at Wave 1)"),
              # type = list(all_categorical() ~ "continuous"),
              # statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = "ifany") %>%
  add_overall() %>%
  as_flex_table()

as_factor(ds1$AGE_W1) %>% as.character() %>% str_remove(" years") %>%
  str_remove(" or older") %>% str_remove(" or younger") %>%
  as.numeric() %>% psych::describe()

# Sample in Wave 1
gtdemosw1 <- ds1 %>% 
  select(all_of(DemoVars), Grade, Tx) %>%
  mutate(Tx = ifelse(Tx == 1, "SoS", "Waitlist"),
         Transgender = as_factor(Transgender) %>% fct_drop()) %>%
  mutate(across(all_of(c(DemoVars, "Grade")), ~as_factor(.x) %>% fct_explicit_na(na_level = "Unknown"))) %>%
  tbl_summary(by = Tx,
              label = list(Transgender ~ "Transgender (recoded across waves)",
                           Grade ~ "Grade (at Wave 1)"),
              # type = list(all_categorical() ~ "continuous"),
              # statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = "ifany") %>%
  add_overall() %>%
  as_flex_table()

gtdemos.wave <- map(paste0("HasASurvey_W", 1:4),
                    ~ds1[ds1[[.x]] == 1,] %>%
                      select(all_of(DemoVars), Grade, Tx) %>%
                      mutate(Tx = ifelse(Tx == 1, "SoS", "Waitlist"),
                             Transgender = as_factor(Transgender) %>% fct_drop()) %>%
                      mutate(across(all_of(c(DemoVars, "Grade")), ~as_factor(.x) %>% fct_explicit_na(na_level = "Unknown"))) %>%
                      tbl_summary(by = Tx,
                                  label = list(Transgender ~ "Transgender (recoded across waves)",
                                               Grade ~ "Grade (at Wave 1)"),
                                  missing = "ifany") %>%
                      as_flex_table())

#####################################################################

# ---- Raw Scores - Mean of Item Scores ---------------------------

## Uses ds1long because collapsing categories is not necessary for raw score and actually
# provides more intuitive interpretation of the mean (items all have the same number of categories)
# Note: The scales and variables included here are based on the measurement results in 1-EFA_CFA.R and 2-LongMI.R
raw.scores <- map_dfc(.x = c(OutcomeVars, ProtectiveVars, OtherScaleVars),
                      ~scale_score(ds1long, .x, type = "mean", min.valid = length(.x)-1)) %>% # only 1 missing response allowed
  bind_cols(map_dfc(.x = list(Active_Exposure = ActiveExpVars, Passive_Exposure = PassiveExpVars[-4]), # item 4 removed based on measurement results
                    ~scale_score(ds1long, .x, type = "sum", min.valid = length(.x)))) # must have answered all items

## Adding identifiers and recoding exposure to account NAs and missing surveys
raw.scores <- bind_cols(select(ds1long, StudentID, Tx, Wave, HasASurvey), raw.scores) %>%
  mutate(across(.cols = c(Active_Exposure, Passive_Exposure),
                .fns = ~case_when(Wave == 1 ~ NA_real_,
                                  HasASurvey == 0 ~ NA_real_,
                                  Tx == 0 ~ 0,           # Follows Intent-To-Treat Framework
                                  TRUE ~ .x)))

# 92 cases have a survey, but no scale scores - some are all NAs, others have sporadic responding
no.score.cases <- raw.scores[rowSums(is.na(raw.scores[,-c(1:4)])) == ncol(raw.scores[,-c(1:4)]) & raw.scores$Wave != 1 & raw.scores$HasASurvey == 1,] #%>% View()
# ds1long %>% inner_join(no.score.cases, by = c("StudentID", "Wave")) %>% View()


#### Descriptive Statistics ####
VariableDescrips <- raw.scores %>%
  gather(Variable, Value, -StudentID, -Tx, -Wave, - HasASurvey) %>%
  group_by(Tx, Wave, Variable) %>%
  skimr::skim(Value)
# In wave 4 > 200 control students were exposed - consider Tx == 0 ~ 0
# the school switched to treatment, but students are still coded as control under intent to treat framework

## Creating output for Supplemental Materials
VariableDescrips.wide <- VariableDescrips %>%
  filter(Variable %in% OutcomeNames[-7]) %>%
  mutate(Tx = case_when(Tx == 0 ~ "Waitlist",
                        Tx == 1 ~ "SoS"),
         Txwave = paste(Tx, Wave, sep = "_W"),
         est = paste0(format(round(numeric.mean, 2), nsmall = 2),
                      " (", format(round(numeric.sd, 2), nsmall = 2), ")")) %>%
  select(Variable, Txwave, est) %>%
  spread(Txwave, est) %>%
  mutate(Variable = str_replace_all(Variable, "_", " "))

## Outcome Distribution Plots by TX
OutcomeDensities <- map(.x = c(OutcomeNames, ProtectiveNames),
                        ~raw.scores %>%
                          mutate(Tx = factor(Tx) %>%
                                   fct_recode(Waitlist = "0", SoS = "1")) %>%
                          # mutate(across(one_of(.x), log)) %>%
                          ggplot(aes_string(x = .x, fill = "Tx", color = "Tx")) +
                          geom_density(alpha = .4) +
                          xlab(gsub("_", " ", .x)) +
                          theme_bw() +
                          theme(legend.title = element_blank()) +
                          facet_wrap(~Wave)) %>%
  set_names(c(OutcomeNames, ProtectiveNames))


#### Adding scores to wide data ####
raw.scores.wide <- raw.scores %>% select(-HasASurvey) %>%
  gather(Variable, Value, -StudentID, -Tx, -Wave) %>%
  mutate(Variable = paste0(Variable, "_Score_W", Wave)) %>%
  select(-Wave) %>%
  spread(Variable, Value) %>%
  mutate(ActivexPassive_W2 = Active_Exposure_Score_W2 * Passive_Exposure_Score_W2, # Interactions for LGM regressions
         ActivexPassive_W3 = Active_Exposure_Score_W3 * Passive_Exposure_Score_W3,
         ActivexPassive_W4 = Active_Exposure_Score_W4 * Passive_Exposure_Score_W4)

ds2 <- ds1 %>%
  left_join(raw.scores.wide, by = c("StudentID", "Tx")) %>%
  mutate(Active_Exposure_Score_Avg = scale_score(., paste0("Active_Exposure_Score_W", 2:4), type = "mean", min.valid = NULL),
         Passive_Exposure_Score_Avg = scale_score(., paste0("Passive_Exposure_Score_W", 2:4), type = "mean", min.valid = NULL))


## correlations
# ds2 %>% select(contains("_Exposure_")) %>% cor(use = "pairwise.complete.obs") %>% round(2)

#################################################################


# ----- Frequencies --------------------------

#### Total Frequencies ####
TotalFrequencies <- map(c(OutcomeVars, ProtectiveVars, list(Exposure = ExposureVars), OtherScaleVars),
                     ~ds1long %>%
                       select(Wave, all_of(.x)) %>%
                       Get_ItemFreqs(Wave, NAto0 = TRUE) %>%
                       mutate(Item = factor(Item, levels = .x))) %>%
  set_names(c(OutcomeNames, ProtectiveNames, "Exposure"), OtherScaleNames)


#### Frequencies by Demographic Group ####
## Exposure
ExposureFrequencies <- map(.x = DemoVars,
                           ~ds1long %>%
                             filter(!is.na((!!as.name(.x)))) %>%
                             select(Wave, all_of(.x), all_of(ExposureVars)) %>%
                             Get_ItemFreqs(Wave, (!!as.name(.x)), NAto0 = TRUE) %>%
                             mutate(Item = factor(Item, levels = ExposureVars))) %>%
  set_names(DemoVars)


## Protective Factor Scales
ProtectiveFrequencies <- lapply(ProtectiveVars,
                                function(i){
                                  map(.x = DemoVars,
                                      ~ds1long %>%
                                        filter(!is.na((!!as.name(.x)))) %>%
                                        select(Wave, all_of(.x), all_of(i)) %>%
                                        Get_ItemFreqs(Wave, (!!as.name(.x)), NAto0 = TRUE) %>%
                                        mutate(Item = factor(Item, levels = i))) %>%
                                    set_names(DemoVars)
                                }) %>%
  set_names(ProtectiveNames)

## Primary Outcome Scales
OutcomeFrequencies <- lapply(OutcomeVars,
                                function(i){
                                  map(.x = DemoVars,
                                      ~ds1long %>%
                                        filter(!is.na((!!as.name(.x)))) %>%
                                        select(Wave, all_of(.x), all_of(i)) %>%
                                        Get_ItemFreqs(Wave, (!!as.name(.x)), NAto0 = TRUE) %>%
                                        mutate(Item = factor(Item, levels = i))) %>%
                                    set_names(DemoVars)
                                }) %>%
  set_names(OutcomeNames)


#### Collapsing Categories ####
## This is to run psychometric checks; scores are still based on full response scale
# OutcomeFrequencies$HNC_Perpetration$Gender %>% View()

## collapsing categories 3 and 4 for selected items (category collapsed at each wave)
Collapse3 <- c(HNCPerpVars, HNCVictVars, CyberVars)

Collapse2 <- c(BullyPerpVars)

## Need to collapse code 1 and higher (i.e., dichotomize)
# When looking by group SH (no contact) and SV (contact) need to be dichotomized
Collapse1 <- c(NoContactPerpVars, NoContactVictVars, ContactPerpVars, ContactVictVars)

## Earlier notes; Not sure if still relevant
# Low frequencies only primarily for category 3
# However, high correlations between items persist because of 0s in the off-diagonal cells
# This leads to Heywood cases (negative residual covariances)
# there are also a few instances of negative residual variances, but I'm less concerned about
# these since the strict model constrains these to be equal anyway


## Recoding for low frequencies
ds2 <- ds2 %>%
  mutate(across(all_of(
    paste(Collapse3, rep(1:4, each = length(Collapse3)), sep = "_W")),
    ~case_when(as.numeric(.) >= 3 ~ 3,
               TRUE ~ as.numeric(.)))) %>%
  mutate(across(all_of(
    paste(Collapse2, rep(1:4, each = length(Collapse3)), sep = "_W")),
    ~case_when(as.numeric(.) >= 2 ~ 2,
               TRUE ~ as.numeric(.)))) %>%
  mutate(across(all_of(
    paste(Collapse1, rep(1:4, each = length(Collapse1)), sep = "_W")),
    ~case_when(as.numeric(.) >= 1 ~ 1,
               TRUE ~ as.numeric(.))))

# lapply(paste(HNCPerpVars, rep(1:4, each = length(HNCPerpVars)), sep = "_W"), function(x) table(ds2[[x]], useNA = "always"))


#### Updating Long Version with Recodes and Scale Scores ####
ds2long <- LongWave(ds2, 1, StudentID:Tx, Grade:LGBQxTx, Active_Exposure_Score_Avg, Passive_Exposure_Score_Avg) %>%
  bind_rows(LongWave(ds2, 2, StudentID:Tx, Grade:LGBQxTx, Active_Exposure_Score_Avg, Passive_Exposure_Score_Avg),
            LongWave(ds2, 3, StudentID:Tx, Grade:LGBQxTx, Active_Exposure_Score_Avg, Passive_Exposure_Score_Avg),
            LongWave(ds2, 4, StudentID:Tx, Grade:LGBQxTx, Active_Exposure_Score_Avg, Passive_Exposure_Score_Avg)) %>%
  select(StudentID, Wave, everything())

###################################################


# ------------ Missing Data ------------------------

#### Attrition Patterns ####

## Survey responding by wave
table(ds2long$HasASurvey, ds2long$Wave) %>% #margin.table(margin = 1) %>%
  prop.table(margin = 2)

## Each students' pattern
attritionpattern <- ds2 %>%
  select(School, StudentID, Tx, all_of(c(DemoVars, "Grade")), HasASurvey_W1:HasASurvey_W4) %>%
  unite(col = "Surveyed", HasASurvey_W1:HasASurvey_W4, sep = ", ", remove = FALSE)

## Attrition Pattern by Tx Summary Table
attritiontable <- attritionpattern %>%
  select(Tx, Surveyed) %>%
  mutate(Tx = ifelse(Tx == 1, "SoS", "Waitlist")) %>%
  tbl_summary(by = "Tx") %>%
  add_overall() %>%
  as_flex_table()

## Participation Rate
gt.participation <- ds2 %>%
  select(Tx, HasASurvey_W1:HasASurvey_W4) %>%
  mutate(Tx = ifelse(Tx == 1, "SoS", "Waitlist")) %>%
  mutate(across(HasASurvey_W1:HasASurvey_W4, .fn = as.numeric)) %>%
  tbl_summary(by = "Tx") %>%
  add_overall() %>%
  as_flex_table()

# colSums(attritiontable[8:15, -1]) # count of students with W1 survey
# table(attritionpattern$Tx, attritionpattern$HasASurvey_W1, useNA = "always") %>% prop.table(1) %>% round(3) # by condition
# colSums(attritiontable[c(1, 2, 4, 8), -1]) # took 1 survey
# colSums(attritiontable[c(3, 5, 6, 9, 10, 12), -1]) # took 2 surveys
# colSums(attritiontable[c(7, 11, 13, 14), -1]) # took 3 surveys

#### Predicting Attrition ####
## predicting later attrition with W1 variables
# gathering wave 1 predictors
attrition.preds <- ds2 %>%
  select(Tx, Female, OtherG, White, POC, LGBQ, AGE_W1, ends_with("Score_W1"),
         -Active_Exposure_Score_W1, -Passive_Exposure_Score_W1, -Staff_Help_Intent_Score_W1) %>%
  names()


# predicts whether they took the survey
predict.attrition <- map(.x = c("HasASurvey_W2", "HasASurvey_W3", "HasASurvey_W4"),
                         ~lme4::glmer(formula(paste(.x, "~ 1 +", paste(attrition.preds, collapse = " + "), " + (1|School)")), data = ds2,
                              family = binomial(link = "logit"))) %>%
  set_names(c("Wave 2", "Wave 3", "Wave 4"))
lapply(predict.attrition, summary)

## Odds ratio - (reverse coefficients to get odds of attrition)
# lapply(predict.attrition, function(x) round(exp(coef(x)$School[1,]), 3))

ORs <- map(.x = predict.attrition,
             ~parameters::model_parameters(.x) %>%
               select(Parameter, Coefficient, CI_low, CI_high, p) %>%
               mutate(OR = exp(Coefficient)))

ORs.sig <- map(ORs, ~filter(.x, p < .05))


## Missingness by case and variable
# missoverview <- dsconscales %>%
#   select(all_of(predictornames)) %>%
#   miss_summary()

#### Missing cases by outcome and wave ####

# Note: Does NOT remove students without a survey; thus, also illustrates attrition

## Sexual Harassment - No Contact
NoContactMissCase <- missing_data_wrapper(data = ds2long,
                                          scale = list(NoContactPerpVars, NoContactVictVars),
                                          scale.name = c("Perp", "Victim"))


## Sexual Violence - Contact
ContactMissCase <- missing_data_wrapper(data = ds2long,
                                        scale = list(ContactPerpVars, ContactVictVars),
                                        scale.name = c("Perp", "Victim"))


## Homophobic Name Calling
HNCMissCase <- missing_data_wrapper(data = ds2long,
                                    scale = list(HNCPerpVars, HNCVictVars),
                                    scale.name = c("Perp", "Victim"))


## Cybersex perpetration
CyberMissCase <- missing_data_wrapper(data = ds2long,
                                      scale = CyberVars)

## Dismissive attitudes toward sexual harassment
DismissMissCase <-missing_data_wrapper(data = ds2long,
                                       scale = DismisssvVars)


#############################################################

# ------  Subgroup Descriptive statistics (Exposure and Trusted Adult Nominations) ----------
## Groups to examine
subby <- c("School", "Grade", "Transgender", "Gender", "Race", "SexOr", "Nominated_Trusted_Adult",
           "Sexual_Harassment", "Forced_Sexual_Contact", "Sexual_Violence","HNC", "Dismissive", "Peer_Leader")

## subsetting data
sos.expta <- ds2 %>%
  mutate(Nominated_Trusted_Adult = ifelse(Trusted_Adult_W1 == 0, "No", "Yes"),
         Sexual_Harassment = ifelse(No_Contact_Perpetration_Score_W1 == 0, "Non-Perpetrator", "Perpetrator") %>%
           factor(levels = c("Non-Perpetrator", "Perpetrator")),
         Forced_Sexual_Contact = ifelse(Contact_Perpetration_Score_W1 == 0, "Non-Perpetrator", "Perpetrator") %>%
           factor(levels = c("Non-Perpetrator", "Perpetrator")),
         Sexual_Violence = case_when(Contact_Perpetration_Score_W1 > 0 ~ "Forced Contact Perpetrator",
                                     No_Contact_Perpetration_Score_W1 > 0 ~ "SH Only Perpetrator",
                                     is.na(Contact_Perpetration_Score_W1) & is.na(No_Contact_Perpetration_Score_W1) ~ NA_character_,
                                     TRUE ~ "Non-Perpetrator"),
         Sexual_Violence = factor(Sexual_Violence, levels = rev(c("Forced Contact Perpetrator", "SH Only Perpetrator", "Non-Perpetrator"))),
         HNC = ifelse(HNC_Perpetration_Score_W1 == 0, "Non-Perpetrator", "Perpetrator") %>%
           factor(levels = c("Non-Perpetrator", "Perpetrator")),
         Dismissive = ifelse(SV_Dismissiveness_Score_W1 == 0, "Not Dismissive", "Dismissive") %>%
           factor(levels = c("Not Dismissive", "Dismissive")),
         Peer_Leader = as_factor(PLIndEver)) %>%
  filter(Grade %in% c(9:11) | is.na(Grade)) %>%
  mutate(School = as_factor(School) %>% fct_drop(),
         Transgender = as_factor(Transgender) %>% fct_drop()) %>%
  select(StudentID, Tx, all_of(subby), Passive_Exposure_Score_W2:Passive_Exposure_Score_W4, Passive_Exposure_Score_Avg,
         Active_Exposure_Score_W2:Active_Exposure_Score_W4, Active_Exposure_Score_Avg,
         starts_with("Trusted_Adult")) %>%
  rename_with(~str_remove(.x, "_Exposure_Score"), .cols = dplyr::contains("Exposure_Score"))

# saveRDS(sos.expta, file = "SoS_Exposure_TANom.rds") # why does this need to be saved?

#######     Exposure by Group     ########

sos.exp <- sos.expta %>%
  filter(Tx == 1) %>%
  select(-starts_with("Trusted_Adult"), -Tx)

## Mean and SD of Exposure by Wave and Group
exp.tables.wave <- map(subby, ~sos.exp %>%
                    select(all_of(.x), dplyr::contains("_W")) %>%
                    tbl_summary(by = all_of(.x),
                                type = list(all_categorical() ~ "continuous"),
                                statistic = list(all_continuous() ~ "{mean} ({sd})"),
                                missing = "no") %>% 
                    add_p(test = list(all_continuous() ~ "kruskal.test")) %>% # kruskal is the non-parametric equivalent to anova which is more suitable given the distributions
                    # add_q(method = "fdr") %>%  # false discovery rate is the same as Benjamini & Hochberg; only corrects for the 6 tests within each table which doesn't make much difference
                    as_flex_table() %>%
                      flextable::font(fontname = "Times New Roman", part = "all") %>%
                      flextable::padding(padding = 1, part = "all") %>%
                      flextable::autofit()) %>%
  set_names(subby)

exp.tables.avg <- map(subby, ~sos.exp %>%
                        select(all_of(.x), dplyr::contains("_Avg")) %>%
                        tbl_summary(by = all_of(.x),
                                    type = list(all_categorical() ~ "continuous"),
                                    statistic = list(all_continuous() ~ "{mean} ({sd})"),
                                    missing = "no") %>% 
                        add_p(test = list(all_continuous() ~ "kruskal.test")) %>% # kruskal is the non-parametric equivalent to anova which is more suitable given the distributions
                        # add_q(method = "fdr") %>%  # false discovery rate is the same as Benjamini & Hochberg; only corrects for the 6 tests within each table which doesn't make much difference
                        as_flex_table() %>%
                        flextable::font(fontname = "Times New Roman", part = "all") %>%
                        flextable::padding(padding = 1, part = "all") %>%
                        flextable::autofit()) %>%
  set_names(subby)

exp.overall <- sos.exp %>%
  select(contains("_W")) %>%
  tbl_summary(type = list(all_categorical() ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              missing = "no") %>% 
  as_flex_table() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::padding(padding = 1, part = "all") %>%
  flextable::autofit()

## kruskal-wallis - if we need test statistic and df
exp.names <- select(sos.exp, contains("_W")) %>% names()

sh.exp.test <- map(exp.names, ~kruskal.test(sos.exp[[.x]], sos.exp$Sexual_Harassment)) %>%
  map_dfr(.x = ., ~data.frame(statistic = .x$statistic,
                              df = .x$parameter,
                              p = .x$p.value)) %>%
  cbind(data.frame(Exposure = exp.names), .)
rownames(sh.exp.test) <- NULL

fsc.exp.test <- map(exp.names, ~kruskal.test(sos.exp[[.x]], sos.exp$Forced_Sexual_Contact)) %>%
  map_dfr(.x = ., ~data.frame(statistic = .x$statistic,
                              df = .x$parameter,
                              p = .x$p.value)) %>%
  cbind(data.frame(Exposure = exp.names), .)
rownames(fsc.exp.test) <- NULL

hnc.exp.test <- map(exp.names, ~kruskal.test(sos.exp[[.x]], sos.exp$HNC)) %>%
  map_dfr(.x = ., ~data.frame(statistic = .x$statistic,
                              df = .x$parameter,
                              p = .x$p.value)) %>%
  cbind(data.frame(Exposure = exp.names), .)
rownames(hnc.exp.test) <- NULL

# kruskal.test(sos.exp$Active_Avg, sos.exp$HNC)

prop.table(table(sos.exp$Peer_Leader, sos.exp$Sexual_Harassment), 2)
prop.table(table(sos.exp$Peer_Leader, sos.exp$Forced_Sexual_Contact), 2)
prop.table(table(sos.exp$Peer_Leader, sos.exp$HNC), 2)

tbl_summary(sos.exp, by = "Peer_Leader", include = c("Sexual_Harassment", "Forced_Sexual_Contact", "HNC"))

map(c("Sexual_Harassment", "Forced_Sexual_Contact", "HNC"),
    ~FreqProp(sos.exp$Peer_Leader, sos.exp[[.x]], margin = 2))


## Converting data to longer format for plotting
exp.long <- sos.exp %>%
  gather(Temp, Score, Passive_W2:Passive_W4, Active_W2:Active_W4) %>%
  separate(Temp, into = c("Exposure", "Wave"), sep = "_")


## Exposure distribution by Wave and Group
library(ggridges)
library(rlang)
exp.ridges <- map(subby, 
                  ~exp.long %>%
                    filter(!is.na(!!sym(.x))) %>%
                    mutate(Wave = factor(Wave, levels = c("W4", "W3", "W2"))) %>%
                    ggplot(aes(x = Score, y = Wave, fill = Wave, height = ..density..)) +
                    geom_density_ridges(stat = "density", trim = TRUE) +
                    scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
                    scale_fill_brewer(palette = "Set2") +
                    theme_ridges() + 
                    theme(legend.position = "none") +
                    facet_grid(reformulate("Exposure", .x))) %>%
  set_names(subby)

# produced just in case
exp.ridges.avg <- map(subby, 
                  ~sos.exp %>%
                    select(StudentID:Peer_Leader, Active_Avg, Passive_Avg) %>%
                    gather("Exposure", "Score", Active_Avg, Passive_Avg) %>%
                    mutate(Exposure = str_remove(Exposure, "_Avg")) %>%
                    filter(!is.na(!!sym(.x))) %>%
                    ggplot(aes(x = Score, y = !!sym(.x), fill = !!sym(.x), height = ..density..)) +
                    geom_density_ridges(stat = "density", trim = TRUE) +
                    scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
                    scale_fill_brewer(name = str_replace_all(.x, "_", " "), palette = "Set2") +
                    theme_ridges() + 
                    # theme(legend.position = "top") +
                    theme(axis.title.y = element_blank(), axis.text.y = element_blank()) +
                    guides(fill = guide_legend(reverse=T)) +
                    facet_wrap(~Exposure)) %>%
  set_names(subby)

# stest <- exp.long %>%
#   group_by(Gender, Exposure, Wave) %>%
#   skimr::skim(Score)

## Predicting Exposure
library(lme4)
exp.unc <- map(c("Active", "Passive"),
               ~exp.long %>% filter(Exposure == .x) %>%
                 lmer(Score ~ 1 + (1|School/StudentID), data = .)) %>%
  set_names(c("Active", "Passive"))

exp.con <- map(c("Active", "Passive"), ~exp.long %>%
                 mutate(Wave = str_remove(Wave, "W") %>% as.numeric(),
                        SexOr = fct_relevel(SexOr, "Straight"),
                        Race = fct_relevel(Race, "Hispanic")) %>%
                 filter(Exposure == .x) %>%
                 lmer(Score ~ 1 + Wave + Grade + Transgender + Gender + Race + SexOr + 
                        Sexual_Harassment + Forced_Sexual_Contact + HNC + Dismissive + (1|School/StudentID), data = .)) %>%
  set_names(c("Active", "Passive"))

exp.icc <- map(exp.unc, ~performance::icc(.x, by_group = TRUE))
exp.perform <- map(exp.con, ~performance::model_performance(.x))
exp.modcheck <- map(exp.con, ~performance::check_model(.x, check = c("vif", "normality", "qq", "reqq", "linearity", "homogeneity"))) # # qq worked until i installed qqplotr; outliers does not work
exp.params <- map(exp.con, ~parameters::model_parameters(.x))


######    Trusted Adult Nomination by Group   ######

sos.ta <- sos.expta %>%
  select(-starts_with("Active"), -starts_with("Passive")) %>%
  mutate(Tx = factor(Tx) %>% fct_recode(Waitlist = "0", Sources = "1")) %>%
  mutate(across(Transgender:Dismissive, ~ifelse(is.na(.x), NA, paste(.x, Tx, sep = "-")), .names = "{col}xTx")) %>%
  mutate(Wave_Count = scale_score(., paste0("Trusted_Adult_W",1:4), type = "sum")) %>%
  unite(Pattern, starts_with("Trusted_Adult"), sep = ", ", remove = FALSE)

## Mean and SD of Exposure by Wave and Group
ta.tables.wave <- map(c("Tx", subby[-c(8:9)], "TransgenderxTx", "GenderxTx", "RacexTx", "SexOrxTx", "Nominated_Trusted_AdultxTx",
                        "Sexual_ViolencexTx", "HNCxTx", "DismissivexTx"),
                      ~sos.ta %>%
                         select(all_of(.x), dplyr::contains("_W")) %>%
                         tbl_summary(by = all_of(.x),
                                     statistic = list(all_categorical() ~ "{n} ({p}%)"),
                                     missing = "no") %>% 
                         add_p(test = list(all_categorical() ~ "chisq.test.no.correct")) %>% 
                         # add_q(method = "fdr") %>%  # false discovery rate is the same as Benjamini & Hochberg; only corrects for the 4 tests within each table which doesn't make much difference
                         as_flex_table() %>%
                        flextable::font(fontname = "Times New Roman", part = "all") %>%
                        flextable::padding(padding = 1, part = "all") %>%
                        flextable::autofit()) %>%
  set_names(c("Intervention_Condition", subby[-c(8:9)],
              "Transgender_by_Tx", "Gender_by_Tx", "Race_by_Tx", "SexOr_by_Tx", "TA_Nom_by_Tx",
              "Sexual_Violence_by_Tx", "HNC_by_Tx", "Dismissive_by_Tx"))

ta.tables.wave$TA_Nom_by_Tx

sos.ta.long <- sos.ta %>%
  gather(Wave, TA, starts_with("Trusted")) %>%
  mutate(Wave = str_remove(Wave, "Trusted_Adult_W"),
         Grade = as.character(Grade)) %>%
  rename(Condition = "Tx")

ta.plots <- map(subby[-c(1,8:9)],
                ~sos.ta.long %>%
                  group_by(Condition, !!sym(.x), Wave) %>%
                  summarize(n = sum(!is.na(TA)),
                            Proportion = mean(TA, na.rm = TRUE)) %>%
                  filter(!is.na(!!sym(.x))) %>%
                  mutate(Group = paste(Condition, !!sym(.x))) %>%
                  ggplot(aes(x = Wave, y = Proportion, group = Group)) +
                  geom_line(aes(linetype = Condition, colour = !!sym(.x)), size = 1.5) +
                  geom_point(aes(shape = !!sym(.x), colour = !!sym(.x)), size = 4) +
                  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .10)) +
                  scale_colour_brewer(palette = "Set2") +
                  theme_bw(base_size = 18)) %>%
  set_names(subby[-c(1,8:9)])
ta.plots$Nominated_Trusted_Adult

ta.pattern <- sos.ta %>%
  select(Wave_Count, Pattern) %>%
  tbl_summary() %>%
  as_flex_table() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::padding(padding = 1, part = "all") %>%
  flextable::autofit()


##################################################################################

#  ------ Saving ---------------

## Demographics and Missing Data
save(gtdemos, gtdemosw1, gtdemos.wave,
     VariableDescrips, VariableDescrips.wide, OutcomeDensities,
     TotalFrequencies, ExposureFrequencies,
     OutcomeFrequencies, ProtectiveFrequencies,
     attritiontable, gt.participation,
     predict.attrition, ORs, ORs.sig,
     NoContactMissCase, ContactMissCase,
     HNCMissCase, CyberMissCase, DismissMissCase,
     exp.tables.wave, exp.tables.avg, exp.overall,
     exp.ridges, exp.ridges.avg, exp.unc, exp.con,
     exp.icc, exp.params, exp.perform, exp.modcheck, hnc.exp.test,
     ta.tables.wave, ta.plots, ta.pattern, sos.expta,
     file = "Output/SoS_Miss_Demos.RData")

## datasets
save(dsclean, ds1, ds1long, ds2, ds2long,
     raw.scores, raw.scores.wide,
     file = "Output/SoS_Data.RData")

# load("Output/SoS_Miss_Demos.RData")
# load("Output/SoS_Data.RData")



