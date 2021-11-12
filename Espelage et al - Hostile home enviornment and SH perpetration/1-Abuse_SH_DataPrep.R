##########################################
#                                        #
#       Paper 1 - Abuse and SH           #
#   Data Exploration and Preparation     #
#                                        #
##########################################


# ---- Importing information and preliminary data prep ------------------

## Packages and functions
source("Scripts/0-PnF.R")

## Data
sswide.orig <- read_sav("Data and Codes/Second Step RCT Waves 1-4_WiderForm.sav")
sslong.orig <- read_sav("Data and Codes/Second Step RCT Waves 1-4_LongerForm.sav")

## Keeping only the variables we need for analysis
# and only control students (based on W1 indicator)
sswide <- sswide.orig %>%
  select(subjno:AGE_W4, starts_with("GRADE"), MOM_EDU_W1, DAD_EDU_W1,
         starts_with("ABUSE"), starts_with("FAM_VIOL"), starts_with("SIB_AGG"),
         starts_with("DEPRESSION"), starts_with("EMPATHY"), contains(su_items),
         starts_with("DELINQ"), starts_with("SCH_BEL"), contains("SH_PERP"),
         -starts_with("School_W"), -starts_with("CYBER")) %>%
  filter(TX_COND_W1 == 0)

## Transforming variables for regression and/or multigroup analysis
sswide <- sswide %>%
  mutate(Race4 = as_factor(Race) %>% fct_other(drop = c("Am Indian", "Asian", "Pac Island", "Multiple"), other_level = "Other"), # frequencies too small for multigroup analysis
         Female = ifelse(Gender == 1, 1, 0),
         Black = ifelse(Race4 == "Black", 1, 0),
         Hispanic = ifelse(Race4 == "Hispanic", 1, 0),
         White = ifelse(Race4 == "White", 1, 0),
         OtherR = ifelse(Race4 == "Other", 1, 0)) %>%
  mutate(across(contains("GRADES"), ~na_if(.x, 8))) %>% # "Not sure" to NA
  mutate(across(contains("GRADES"), ~max(., na.rm = TRUE) - .)) # reverse scoring so Mostly D's and F's = 0, Mostly A's = 6

# table(sswide$GRADES_W1, useNA = "always")
# table(sswide.orig$GRADES_W1, useNA = "always")
# ## Two approaches to getting the data to long format
# widecheck <- LongWave(sswide, subjno, Gender, Race, wave  = 1) %>%
#   bind_rows(LongWave(sswide, subjno, Gender, Race, wave  = 2)) %>%
#   bind_rows(LongWave(sswide, subjno, Gender, Race, wave  = 3)) %>%
#   bind_rows(LongWave(sswide, subjno, Gender, Race, wave  = 4)) %>%
#   select(Wave, subjno, everything()) %>%
#   filter(!is.na(NUMBER_ID)) # 5329 matches sslong


sslong <- sslong.orig %>%
  select(Wave:Race, SCHOOL_ID, TX_COND:AGE, GRADE, MOM_EDU, DAD_EDU, GRADES,
         all_of(c(ab_items, fv_items, sib_items, dep_items, emp_items, su_items,
                del_items, schb_items, w1sh_items, sh_items))) %>%
  filter(subjno %in% sswide$subjno) %>%
  left_join(select(sswide, subjno, Race4:OtherR), by = "subjno") %>% # adding the time-invariant demographic dummy variables
  mutate(GRADES = na_if(GRADES, 8),                    # "Not sure" to NA
         GRADES = max(GRADES, na.rm = TRUE) - GRADES)  # reverse scoring so Mostly D's and F's = 0, Mostly A's = 6
  

#####################################################

# ---- Approaches to define SH perp --------

#### Item frequencies ####
## Wave 1 contains separate at school and outside of school items
w1sh.itemfreq <- sslong %>%
  filter(Wave == 1) %>%
  select(all_of(w1sh_items)) %>%
  Get_ItemFreqs(NAto0 = TRUE) %>%
  mutate(Item = factor(Item, levels = w1sh_items))

## Waves 2-4 does not distinguish where harassment occurred
w24sh.itemfreq <- map_dfr(2:4,
                  ~sslong %>%
                    filter(Wave == .x) %>%
                    select(all_of(sh_items)) %>%
                    Get_ItemFreqs(NAto0 = TRUE), .id = "Wave") %>%
  mutate(Item = factor(Item, levels = sh_items))


#### Combining W1 SH items and joining to main datasets ####
## From wide data, gather W1 SH responses in long format and create dichotomized combined variable
combinew1sh <- sswide %>%
  select(subjno, starts_with("COMM_SH")) %>%
  gather(Item, Comm, -subjno) %>%
  mutate(Item = str_remove(Item, "COMM_")) %>%
  full_join(sswide %>%
              select(subjno, starts_with("SCH_SH")) %>%
              gather(Item, Sch, -subjno) %>%
              mutate(Item = str_remove(Item, "SCH_")),
            by = c("subjno", "Item")) %>%
  mutate(di = case_when(is.na(Comm) & is.na(Sch) ~ NA_integer_,  # coding scheme for combining dichotomized W1 SH variables
                           Comm == 0 & Sch == 0 ~  0L,
                           is.na(Comm) & Sch == 0 ~ 0L,
                           Comm == 0 & is.na(Sch) ~ 0L,
                           Comm > 0 | Sch > 0 ~ 1L,
                           TRUE ~ 2L), # no cases should have a 2; this is used for validity check
         Wave = 1,
         Item = str_remove(Item, "_W."))
# table(combinew1sh$Combo, useNA = "always")
# length(unique(combinew1sh$subjno))

## Join W1 SH responses with dichotomized W2-4
allshdi <- sswide %>%
  select(subjno, starts_with(sh_items)) %>%
  gather(Item, Response, -subjno) %>%
  mutate(di = ifelse(Response == 0, 0, 1),        # dichotomizing W2-4 responses
         Wave = as.numeric(str_sub(Item, -1, -1)),
         Item = str_remove(Item, "_W.")) %>%
  bind_rows(combinew1sh) %>%                      # adding W1 responses
  select(Wave, subjno, Item, di)

# table(allshdi$Response, allshdi$di)

## joining new dichtomized SH to main datasets
# long
sslong <- sslong %>%
  left_join(allshdi %>%
              spread(Item, di) %>%
              select(Wave, subjno, all_of(sh_items)) %>%
              rename_with(~paste0(.x, "_di"), .cols = all_of(sh_items)),
            by = c("Wave", "subjno"))
# wide
sswide <- sswide %>%
  left_join(allshdi %>%
              mutate(Item = paste0(Item, "_W", Wave, "_di")) %>%
              select(-Wave) %>%
              spread(Item, di),
            by = "subjno")


##########################################################


# ---- Evaluating Item Characteristics ---------------------

#### Item Frequencies ####
## All constructs across all subjects
allconstruct.itemfreq <- lapply(list(c(ab_items, fv_items, sib_items),
                                     dep_items, emp_items, del_items,
                                     su_items, schb_items, paste0(sh_items, "_di")),
                                function(i){
                                  sslong %>%
                                    select(Wave, all_of(i)) %>%
                                    Get_ItemFreqs(data = ., Wave, NAto0 = TRUE) %>%
                                    mutate(Item = factor(Item, levels = i)) %>%
                                    arrange(Wave, Item)
                                }) %>%
  set_names(c("Hostile_Home",
              "Depression", "Lack_of_Empathy",
              "Delinquency","Substance_Use", 
              "School_Belonging", "Sexual_Harassment"))
# lapply(allconstruct.itemfreq, View)

## By Gender - Only primary outcomes
gen.itemfreq <- lapply(list(dep_items, del_items, paste0(sh_items, "_di")),
                       function(i){
                         sslong %>%
                           select(Wave, Gender, all_of(i)) %>%
                           mutate(Gender = as_factor(Gender)) %>%
                           Get_ItemFreqs(data = ., Gender, Wave, NAto0 = TRUE) %>%
                           mutate(Item = factor(Item, levels = i)) %>%
                           filter(!is.na(Gender))
                       }) %>%
  set_names(c("Depression", "Delinquency",
              "Sexual_Harassment"))

## By Race - Only primary outcomes
race.itemfreq <- lapply(list(dep_items, del_items, paste0(sh_items, "_di")),
                        function(i){
                          sslong %>%
                            select(Wave, Race4, all_of(i)) %>%
                            Get_ItemFreqs(data = ., Race4, Wave, NAto0 = TRUE) %>%
                            mutate(Item = factor(Item, levels = i)) %>%
                            filter(!is.na(Race4))
                        }) %>%
  set_names(c("Depression", "Delinquency",
              "Sexual_Harassment"))

## By School Belongingness - Only primary outcomes (Belongingness not defined until 3-Prelim_Analysis.R)
# bel.itemfreq <- lapply(list(dep_items, del_items, paste0(sh_items, "_di")),
#                        function(i){
#                          sslong %>%
#                            select(Wave, High_Belong, all_of(i)) %>%
#                            mutate(Belong = as_factor(High_Belong)) %>%
#                            Get_ItemFreqs(data = ., High_Belong, Wave, NAto0 = TRUE) %>%
#                            mutate(Item = factor(Item, levels = i)) %>%
#                            filter(!is.na(High_Belong))
#                        }) %>%
#   set_names(c("Depression", "Delinquency",
#               "Sexual_Harassment"))


##### collapsing low frequency categories ####
# - Substance Use (low frequencies overall) collapsed to Never, 1-2 times, 3+ 
# - Delinquency (low frequencies by group and attempt to improve model fit) dichotomized 
# - Combining the dichotomized SH_PERP items 8-12 into single dichotomized variable (low frequencies by group)
#   - intended for use in latent growth and multigroup models, not necessarily raw scores or descriptives

## long
sslong <- sslong %>%
  mutate(across(all_of(c(su_items)),
                ~case_when(as.numeric(.) >= 2 ~ 2,
                           TRUE ~ as.numeric(.)))) %>%
  mutate(across(all_of(c(del_items)),
                ~case_when(as.numeric(.) >= 1 ~ 1,
                           TRUE ~ as.numeric(.)))) %>%
  mutate(SH_PERP812_di = ifelse(scale_score(., items = paste0(sh_items[8:12], "_di"), type = "sum") > 0, 1, 0))

## wide
sswide <- sswide %>%
  mutate(across(all_of(c(paste(su_items, rep(1:4, each = length(su_items)), sep = "_W"))),
                ~case_when(as.numeric(.) >= 2 ~ 2,
                           TRUE ~ as.numeric(.)))) %>%
  mutate(across(all_of(c(paste(del_items, rep(1:4, each = length(del_items)), sep = "_W"))),
                ~case_when(as.numeric(.) >= 1 ~ 1,
                           TRUE ~ as.numeric(.)))) %>%
  left_join(select(sslong, Wave, subjno, SH_PERP812_di) %>%
              spread(Wave, SH_PERP812_di) %>%
              rename_with(~paste0("SH_PERP812_W", .x, "_di"), .cols = -c(subjno)),
            by = "subjno")


# # checking join
# table(sslong[sslong$Wave ==1, ]$Abuse, useNA = "always")
# table(sswide$Abuse, useNA = "always")


########################################################################

# --------   Creating Raw Scale Scores   ------------------------

# Note: Student must respond to at least 3 items in the scale to receive a score
#       Initially, scale scores were calculated before collapsing response categories to maximize variability,
#       but the LGM for delinquency has poor fit so now investigating collapsing first, similar to SH. This does
#       not affect measurement invariance in 2-Measurement.R

## Reverse scoring - Empathy to Lack of Empathy and "happy" depression item (gets dropped due to poor fit anyway)
# long
sslong <- sslong %>%
  mutate(across(all_of(c(emp_items, "DEPRESSION6")), ~max(., na.rm = TRUE) - .))
# wide
sswide <- sswide %>%
  mutate(across(contains(c(emp_items, "DEPRESSION6")), ~max(., na.rm = TRUE) - .))


## Home Hostility Scales are only in W1
sslong <- sslong %>%
  mutate(Family_Conflict = scale_score(., items = fv_items, type = "mean", min.valid = 3),
         Abuse = scale_score(., items = ab_items, type = "sum", min.valid = 3),
         Sibling_Aggression = scale_score(., items = sib_items, type = "mean", min.valid = 2),
         dep_scale = scale_score(., items = dep_items[-6], type = "mean", min.valid = 3),
         emp_scale = scale_score(., items = emp_items, type = "mean", min.valid = 3),
         del_scale = scale_score(., items = del_items, type = "sum", min.valid = 3),
         su_scale = scale_score(., items = su_items, type = "mean", min.valid = 3),
         schb_scale = scale_score(., items = schb_items, type = "mean", min.valid = 3),
         sh_scale = scale_score(., items = paste0(sh_items[c(1:4, 6, 7)], "_di"), type = "sum", min.valid = 3), # final scale used for analysis
         sh_full_scale = scale_score(., items = paste0(sh_items[-5], "_di"), type = "sum", min.valid = 3)) # Note: final scale for analysis only includes items c(1:4, 6, 7)


# # checking that scores were calculated correctly
#   select(subjno, Wave, all_of(fv_items), Family_Conflict,
#          all_of(ab_items), Abuse,
#          all_of(sib_items), Sibling_Aggression) %>%
#   filter(Wave == 1)
# sslong %>% select(subjno, all_of(dep_items[-6]), dep_scale) %>% View()

## Converting scales to wide format and joining to sswide
scaleswide <- sslong %>%
  select(subjno, Wave, ends_with("_scale")) %>%
  gather(Scale, Score, -subjno, -Wave) %>%
  unite(Temp, Scale, Wave, sep = "_W") %>%
  spread(Temp, Score) %>%
  relocate(starts_with("sh_full"), .after = last_col())


sswide <- sswide %>%
  left_join(sslong %>%
              filter(Wave == 1) %>%
              select(subjno, Family_Conflict, Abuse, Sibling_Aggression),
            by = "subjno") %>%
  left_join(scaleswide, by = "subjno")


##############################################################

# ---- Missing Data Analysis ---------------------

###################
#### Attrition ####

## Number of surveys taken and in which wave for each student
studentwaves <- sslong %>%
  group_by(subjno, Wave) %>%
  summarize(ignore = n(), .groups = "drop_last") %>%
  mutate(waves = sum(ignore)) %>% ungroup()

## Number of students by wave
wavecount <- studentwaves %>%
  count(Wave) %>%
  mutate(Retention = n / nrow(sswide))

## Number of surveys taken by student
studentcount <- studentwaves %>%
  select(subjno, waves) %>% unique()
surveycount <- FreqProp(studentcount$waves, varnames = "Surveys")

## Dichotomous variable indicated whether student took survey at each wave
wavedummies <- studentwaves %>%
  select(-waves) %>%
  spread(Wave, ignore) %>%
  mutate(across(-subjno, ~replace_na(.x, 0), .names = "Took_Survey{col}"))

# Adding survey indicator to wide data
# Don't need to add it to long b/c the row would not exist in the data
sswide <- sswide %>%
  left_join(select(wavedummies, subjno, starts_with("Took")), by = "subjno")

#### Investigating attrition patterns ####
attritiondata <- sswide %>%
  select(subjno, Gender, Race, Race4:OtherR, Family_Conflict, Abuse, Sibling_Aggression,
         starts_with("Took"), ends_with("_W1")) %>%
  nabular(only_miss = TRUE) # creates dichotomous indicators of missingness for all variables with missingness

## Overall patterns
surveypattern <- attritiondata %>%
  select(subjno, starts_with("Took")) %>%
  unite(col = "Pattern", -subjno, sep = ", ", remove = FALSE) %>%
  mutate(Num_Surveys = rowSums(.[c(paste0("Took_Survey", 1:4))]))
sp.summary <- FreqProp(surveypattern$Pattern, varnames = "Pattern") %>%
  arrange(desc(Percent))

# students with only 1 wave of data
onewaveIDs <- surveypattern[surveypattern$Num_Surveys < 2,]$subjno

## predicting later attrition with W1 variables
# gathering wave 1 predictors
attrition.preds <- attritiondata %>%
  select(Female, Black:OtherR, AGE_W1, GRADES_W1:DAD_EDU_W1,                               # Demographics
         Family_Conflict, Abuse, Sibling_Aggression, del_scale_W1:su_scale_W1,  # scale scores
         -White) %>%                                                            # reference group
  names()


# Technically predicts whether they took the survey, so coefficients need to be reversed
# excludes students who only participated in one wave
predict.attrition <- map(.x = c("Took_Survey2", "Took_Survey3", "Took_Survey4"),
            ~glm(formula(paste(.x, "~ 1 +", paste(attrition.preds, collapse = " + "))), data = attritiondata[!attritiondata$subjno %in% onewaveIDs, ],
                 family = binomial(link = "logit"))) %>%
  set_names(c("Wave 2", "Wave 3", "Wave 4"))
lapply(predict.attrition, summary)

## Odds ratio - in the correct direction
lapply(predict.attrition, function(x) round(exp(coef(x)*-1), 3))



###################################
#### Non-attrition missingness ####
## Summarizes the missingness at each wave after removing attrition cases
missing.summary <- map(.x = 1:4,
                       ~get_miss_summary(sswide, .x)) %>%
  set_names(paste0("Wave_", 1:4))

missing.summary$Wave_1$miss_var_summary
lapply(missing.summary, function(x) x[["miss_var_summary"]][[1]] %>% filter(str_detect(variable, "GRADES")))


###################################################

# ----  Finalizing dataset for analysis ---------------------

## Removing students with only 1 wave of data & rearranging variable order 
# wide
sswide <- sswide %>%
  select(subjno, starts_with("Took"), Gender, Female,
         Race, Race4:OtherR, everything()) %>%
  filter(!subjno %in% onewaveIDs)
  
# long
sslong <- sslong %>%
  select(Wave, subjno, Gender, Female, Race, Race4:OtherR, everything()) %>%
  filter(!subjno %in% onewaveIDs)

#### Demographics Summary ####
w1demos <- map_dfr(c("Gender", "Race", "AGE_W1", "GRADE_W1", "GRADES_W1", "MOM_EDU_W1", "DAD_EDU_W1"),
                   ~bind_rows(data.frame(x = .x, n = NA, Percent = NA),
                              FreqProp(as_factor(sswide[[.x]]), useNA = "ifany")) %>%
                     mutate(x = ifelse(is.na(x), paste("Missing", .x), x))) %>%
  filter(is.na(n) | n!=0)


################################################


#### Saving output ####
save(sswide, sslong,
     w1sh.itemfreq, w24sh.itemfreq, allshdi, w1demos, 
     allconstruct.itemfreq, gen.itemfreq, race.itemfreq, #bel.itemfreq,
     studentwaves, wavecount, surveycount,
     surveypattern, sp.summary,
     attritiondata, predict.attrition, missing.summary,
     file = "Output/SS_Data_Prep.RData")

save(sswide, sslong,
     file = "Output/SS_Data_For_Analysis.RData")
