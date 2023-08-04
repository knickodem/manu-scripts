###########################################
#                                         #
#           Sources of Strength           #
#    Preparing Data for Mplus Analysis    #
#                                         #
###########################################

## Loading packages and functions
source("Scripts/PnF.R")
load("Output/SoS_Data.RData")
# load("Output/Longitudinal_MI.RData") # only needed if ds3 contains factor scores and we need the factor scores (currently ds3 == ds2)


# ------  Adding School-level Variables -----------------------------

## School demographics from 2018-2019 (Year 2)
schdemos <- readxl::read_excel("SoS Data and Codes/RSVP2 School Stats.xlsx", sheet = "RReady") %>%
  mutate(Rural = ifelse(Rural_Urban == "Urban", 0, 1),
         SchN100 = School_N/100,
         SchN = scale(SchN100, center = TRUE, scale = FALSE)[,1]) # centered

#### Investigating School Climate Indicators ####
# Note: See 4-Analysis_ML.R for checks
schclimate.orig <- read_sav("../00 School Climate W1-W4 cleaned/SPSS/sources_school_climate_w1_2021-1-27.sav")

## Dichotomizing responses and re-checking baseline differences
schclimate <- schclimate.orig %>%
  mutate(School = as.character(as_factor(RECODED_SCHOOLS_W1)),
         Teasing = case_when(is.na(AGGRESSION_PROBLEM_AT_SCHOOL_3_W1) ~ NA_real_,
                             AGGRESSION_PROBLEM_AT_SCHOOL_3_W1 == 3 ~ NA_real_,    # Don't know
                             AGGRESSION_PROBLEM_AT_SCHOOL_3_W1 %in% c(4, 5) ~ 1,   # pretty big or huge problem
                             TRUE ~ 0),
         MH_Training = case_when(is.na(SCHOOL_COMMITMENT_MT_HEALTH_6_W1) ~ NA_real_,
                                 SCHOOL_COMMITMENT_MT_HEALTH_6_W1 == 3 ~ NA_real_,  # Don't know
                                 SCHOOL_COMMITMENT_MT_HEALTH_6_W1 %in% c(4, 5) ~ 1, # fair amount or a lot of training
                                 TRUE ~ 0))

#### Joining school-level variables to student-level data ####

ds3 <- ds2 %>%
  mutate(School = as.character(as_factor(School))) %>%
  left_join(schdemos, by = "School") %>%
  left_join(schclimate %>%
              group_by(School) %>%
              summarize(Teasing_Per = mean(Teasing, na.rm = TRUE),           # % of school staff reporting teasing was pretty big or huge problem
                        MH_Training_Per = mean(MH_Training, na.rm = TRUE)),  # % of school staff reporting fair amount or a lot of mental health training
            by = "School")

# saveRDS(ds3, file = "SoS Data and Codes/ds3.rds") # used in Imp4-Mplus.R

#############################################################################

## Selecting variables for Mplus dataset and renaming to 8 or fewer characters
mpluswide <- ds3 %>%
  select(ID = StudentID, School:Grade, TransG = Transgender, Gender:Asian, MultiR = Multiracial, Indigen = Indigenous,
         POC:GL, Quest = Questioning, OtherSO, LGBQ,
         FGxTx = FemalexTx, MGxTx = MalexTx, OGxTx = OtherGxTx,
         WRxTx = WhitexTx, LRxTx = LatinxxTx, BRxTx = BlackxTx, ARxTx = AsianxTx, MRxTx = MultiracialxTx, IRxTx = IndigenousxTx, POCxTx,
         SSxTx = StraightxTx, BSxTx = BisexualxTx, GLxTx, QSxTx = QuestioningxTx, OSxTx = OtherSOxTx, LGBQxTx,
         AGE_W1:AGE_W4,
         starts_with(c(NoContactPerpVars, ContactPerpVars,         # Primary Outcome Items
                       NoContactVictVars, ContactVictVars,
                       CyberVars, DismisssvVars,
                       HNCPerpVars, HNCVictVars)),
         starts_with(c(WellBeingVars, SHHelpAttitudeVars, SHHelpIntentVars,
                       DepAnxVars)), # Secondary Outcome (Protective Factor) Items
         starts_with(c("TotAdultNoms", "Trusted_Adult", "SUICIDAL_IDEATION",      # dropping other suicide variables; can add them back in as needed
                       "Active_Exposure_Score", "Passive_Exposure_Score",  # Raw scale scores
                       "No_Contact_Perpetration_Score", "No_Contact_Victimization_Score",
                       "Contact_Perpetration_Score", "Contact_Victimization_Score",
                       "HNC_Perpetration_Score", "HNC_Victimization_Score",
                       "Cybersex_Perpetration_Score", "SV_Dismissiveness_Score",
                       "General_Well.being_Score", "SH_Help_Attitudes_Score", "SH_Help_Seeking_Score",
                       "Bullying_Score", "Cyberbullying_Score", "Peer_Victimization_Score", "Depression.Anxiety_Score")), -ends_with("_Avg"), # excluding Exposure Averages
                  # NCPerp1:Passive4,                                               # Factor Scores
         Funding = Funding_Per_Pupil, Rural, SchN, Teas_Per = Teasing_Per, MHT_Per = MH_Training_Per)  # School-level variables

# Renaming variables to <= 8 characters per Mplus requirement
mpluswide <- mpluswide %>%
  rename_with(~str_replace(.x, "HasASurvey", "srvy"), .cols = starts_with("HasASurvey")) %>%          
  rename_with(~str_replace(.x, "SEX_VIOL_PERP_", "SVP"), .cols = starts_with("SEX_VIOL_PERP_")) %>%
  rename_with(~str_replace(.x, "SEX_VIOL_VICT_", "SVV"), .cols = starts_with("SEX_VIOL_VICT_")) %>%
  rename_with(~str_replace(.x, "HOM_PERP_", "HOP"), .cols = starts_with("HOM_PERP_")) %>%
  rename_with(~str_replace(.x, "HOM_VICT_", "HOV"), .cols = starts_with("HOM_VICT_")) %>%
  rename_with(~str_replace(.x, "CYBER_SEX_PERP_", "CYP"), .cols = starts_with("CYBER_SEX_PERP_")) %>%
  rename_with(~str_replace(.x, "DISMISS_SEX_VIOL_", "DSV"), .cols = starts_with("DISMISS_SEX_VIOL_")) %>%
  rename_with(~str_replace(.x, "GEN_WELL_BEING_", "GWB"), .cols = starts_with("GEN_WELL_BEING_")) %>%
  rename_with(~str_replace(.x, "SEX_HAR_HELP_ATTITUDE_", "SHA"), .cols = starts_with("SEX_HAR_HELP_ATTITUDE_")) %>%
  rename_with(~str_replace(.x, "SEX_HAR_HELP_INTENT_", "SHI"), .cols = starts_with("SEX_HAR_HELP_INTENT_")) %>%
  rename_with(~str_replace(.x, "DEP_ANX_", "DEP"), .cols = starts_with("DEP_ANX_")) %>%
  rename_with(~str_replace(.x, "TotAdultNoms_W", "TotAN_"), .cols = starts_with("TotAdultNoms_W")) %>%
  rename_with(~str_replace(.x, "Trusted_Adult_W", "TA_"), .cols = starts_with("Trusted_Adult_W")) %>%
  rename_with(~str_replace(.x, "SUICIDAL_IDEATION_PLAN_1_W", "SIP_"), .cols = starts_with("SUICIDAL_IDEATION_PLAN_1_W")) %>%
  rename_with(~str_replace(.x, "SUICIDAL_IDEATION_1_W", "SI_"), .cols = starts_with("SUICIDAL_IDEATION_1_W")) %>%
  rename_with(~str_replace(.x, "Active_Exposure_Score_W", "AEXS_"), .cols = starts_with("Active_Exposure_Score_W")) %>%
  rename_with(~str_replace(.x, "Passive_Exposure_Score_W", "PEXS_"), .cols = starts_with("Passive_Exposure_Score_W")) %>%
  rename_with(~str_replace(.x, "No_Contact_Perpetration_Score_W", "NCPS_"), .cols = starts_with("No_Contact_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "No_Contact_Victimization_Score_W", "NCVS_"), .cols = starts_with("No_Contact_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "Contact_Perpetration_Score_W", "CPS_"), .cols = starts_with("Contact_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "Contact_Victimization_Score_W", "CVS_"), .cols = starts_with("Contact_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "HNC_Perpetration_Score_W", "HOPS_"), .cols = starts_with("HNC_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "HNC_Victimization_Score_W", "HOVS_"), .cols = starts_with("HNC_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "Cybersex_Perpetration_Score_W", "CYPS_"), .cols = starts_with("Cybersex_Perpetration_Score_W")) %>%
  rename_with(~str_replace(.x, "SV_Dismissiveness_Score_W", "DSVS_"), .cols = starts_with("SV_Dismissiveness_Score_W")) %>%
  rename_with(~str_replace(.x, "General_Well.being_Score_W", "GWBS_"), .cols = starts_with("General_Well.being_Score_W")) %>%
  rename_with(~str_replace(.x, "SH_Help_Attitudes_Score_", "SHAS_"), .cols = starts_with("SH_Help_Attitudes_Score_")) %>%
  rename_with(~str_replace(.x, "SH_Help_Seeking_Score_W", "SHIS_"), .cols = starts_with("SH_Help_Seeking_Score_W")) %>%
  rename_with(~str_replace(.x, "Bullying_Score_W", "BLLY_"), .cols = starts_with("Bullying_Score_W")) %>%
  rename_with(~str_replace(.x, "Cyberbullying_Score_W", "CBLY_"), .cols = starts_with("Cyberbullying_Score_W")) %>%
  rename_with(~str_replace(.x, "Peer_Victimization_Score_W", "PEER_"), .cols = starts_with("Peer_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "Depression.Anxiety_Score_W", "DEPS_"), .cols = starts_with("Depression.Anxiety_Score_W")) %>%
  rename_with(~str_replace(.x, "Cybersex", "CSPerp"), .cols = starts_with("Cybersex")) %>%        # lines 110-113 are not relevant unless factor scores are included
  rename_with(~str_replace(.x, "GenWellBeing", "GenWB"), .cols = starts_with("GenWellBeing")) %>%
  rename_with(~str_replace(.x, "SHHelpAtt", "SHHAtt"), .cols = starts_with("SHHelpAtt")) %>%
  rename_with(~str_replace(.x, "SHHelpSeek", "SHHSeek"), .cols = starts_with("SHHelpSeek")) %>%
  rename_with(~str_replace(.x, "_W", "_"), .cols = matches("_W[0-9]$")) %>%
  mutate(School = factor(School, levels = SchoolNames)) %>%
  mutate(across(where(is.factor), as.numeric)) %>%                    # Converting factors to numeric per Mplus requirements
  mutate(across(where(is.labelled), as.numeric))

table(sapply(names(mpluswide), function(x) class(mpluswide[[x]])))
# double-checking length of names
table(nchar(names(mpluswide)))


## distribution of teacher nominations; use a negative binomial model b/c sds are consistently higher than the mean (overdispersion)
nomstats <- mpluswide %>%
  select(ID, School, Tx, starts_with("Tot")) %>%
  gather(Wave, Noms, starts_with("Tot")) %>%
  group_by(Wave) %>%
  skimr::skim(Noms)

lapply(paste0("TotAN_", 1:4), function(x) table(mpluswide[[x]]))

## checking if frequencies match between original data and renamed mplus data
# No contact perpetration
map2(.x = paste0(rep(paste0(OutcomeVars$No_Contact_Perpetration, "_W"), each = 4), 1:4),
     .y = paste0(rep(c("SVP1_", "SVP2_", "SVP3_", "SVP4_"), each = 4), 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))

# SH Help Seeking
map2(.x = paste0(rep(paste0(ProtectiveVars$SH_Help_Seeking, "_W"), each = 4), 1:4),
     .y = paste0(rep(c("SHI1_", "SHI2_", "SHI3_", "SHI4_"), each = 4), 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))

# Trusted Adult
map2(.x = paste0("Trusted_Adult_W", 1:4),
     .y = paste0("TA_", 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))

# Suicidal Ideation
map2(.x = paste0("SUICIDAL_IDEATION_1_W", 1:4),
     .y = paste0("SI_", 1:4),
     ~table(ds3[[.x]], mpluswide[[.y]]))


## Saving csv data file for mplus models 
# write.csv(mpluswide,   file = "mpluswide_wbully_colnames.csv", row.names = FALSE, na = "-999")
# write.table(mpluswide, file = "mpluswide_wbully.csv", row.names = FALSE, col.names = FALSE,  na = "-999", sep = ",")
saveRDS(mpluswide, file = "SoS Data and Codes/mpluswide_wbully.rds")

## File with only Wave 1 participants
w1only <- mpluswide %>% filter(srvy_1 == 1)
# saveRDS(w1only, file = "SoS Data and Codes/mpluswide_w1only.rds")


#########################################################################################################



# -------- Using a multilevel modeling approach -----------------
# library(lme4)
# library(performance)
# 
# # Note 1: MLMod residual variances are assumed to be equal across time
# #         whereas they are estimated in LGM
# #         the nlme package can estimate unequal variances, but lme4 cannot
# # Note 2: MLMod will listwise delete missingness at Level2 which is ~200 students,
# #         whereas in LGM the variables can be brought into the model as outcomes
# 
# ## Defining predictors and outcome variables
# int.preds <- c("Tx", "Female", "OtherG", "Latinx", "POC", "LGBQ",
#                "FGxTx", "OGxTx", "LRxTx", "POCxTx", "LGBQxTx",
#                "Rural", "Teas_Per", "MHT_Per")
# mlm.preds <- c(int.preds, paste0(int.preds, "xW"))
# 
# mlm.outcomes <- mplusoutcomes
# names(mlm.outcomes <- c("NCPS", "NCVS", "CPS", "CVS", "HOPS", "HOVS",
#                         "CYPS", "DSVS", "GWBS", "SHAS", "SHIS", "TA"))
# 
# 
# ## Pivoting to long format
# mpluslong <- mpluswide %>%
#   select(ID:LGBQxTx, ends_with("Avg"), Funding:MHT_Per,
#          NCPS_1:SHIS_4, starts_with("TotAN"), starts_with("TA")) %>%
#   # rename_with(~str_remove(., "_"), .cols = starts_with("srvy")|starts_with("TotAN")|starts_with("TA")) %>%
#   gather(Temp, Score, dplyr::matches("[0-9]$")) %>%
#   separate(Temp, into = c("Scale", "Wave"), sep = "_") %>%
#   spread(Scale, Score) %>%
#   mutate(Wave = as.numeric(Wave) - 1) %>%
#   mutate(across(.cols = all_of(mlm.preds), ~. * Wave, .names = "{col}xW"))
# 
# 
# unc <- lmer(as.formula(paste0(mlm.outcomes[[1]], " ~ 1 + Wave + factor(School) + (1 + Wave|ID)")), data = mpluslong, REML = FALSE)
# summary(unc)
# model_performance(unc)
# 
# ## Produces singular model when random effect for school is included; could include fixed effects for school and remove school-level predictors
# cond <- lmerTest::lmer(as.formula(paste0(mlm.outcomes[[1]], " ~ 1 + Wave + ", paste(mlm.preds[-c(12:14, 26:28)], collapse = " + "),
#                                          " + factor(School) + (1 + Wave|ID)")),
#              data = mpluslong, REML = FALSE)
# summary(cond)
# 
# ## including random effect for wave produced singular model
# ta0 <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + (1|ID)")),
#              data = mpluslong, family = binomial(link = "logit"))
# 
# ta.tx <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + Tx + TxxW + (1|ID)")),
#               data = mpluslong, family = binomial(link = "logit"))
# 
# # failed to converge with all predictors
# tictoc::tic()
# ta1 <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + ", paste(mlm.preds[-c(12:14, 26:28)], collapse = " + "), " + (1|ID)")),
#              data = mpluslong, family = binomial(link = "logit"))
# tictoc::toc()
# 
# ta2 <- glmer(as.formula(paste0(mlm.outcomes[[12]], " ~ 1 + Wave + ", paste(mlm.preds[-c(12:14, 26:28)], collapse = " + "),
#                                         " + factor(School) + (1 +|ID)")),
#                       data = mpluslong, family = binomial(link = "logit"))
# summary(ta.tx)
# model_performance(ta0)
# r2(ta0)


#######################################################
