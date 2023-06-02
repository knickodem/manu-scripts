###########################################
#                                         #
#           Sources of Strength           #
#    Preparing Data for Mplus Analysis    #
#                                         #
###########################################

## Loading packages, functions, and data
library(MplusAutomation)
source("Scripts/PnF.R")
# load("Output/SoS_Data.RData") # do I need this one?
load("Output/Implmentation_Exposure_Year_Assoc.RData") # implementation and exposure data
ds3 <- readRDS("SoS Data and Codes/ds3.rds") # created in 3-MplusPrep.R

# ------  Adding Implementation Variables to Student Data -----------------------------

#### Joining school-level variables to student-level data ####
ds4 <- ds3 %>%
  mutate(School = as_factor(School)) %>%
  left_join(impexpyear.wide, by = c("School" = "school_name", "Rural")) %>%
  mutate(across(.cols = any_of(names(impexpyear.wide)), ~ifelse(is.na(.) & Tx == 0, 0, .))) %>%  # Changing Waitlist implementation variables from NA to 0
  select(ID = StudentID, School:Grade, TransG = Transgender, Gender:Asian, MultiR = Multiracial, Indigen = Indigenous,
         POC:GL, Quest = Questioning, OtherSO, LGBQ, AGE_W1:AGE_W4,
         starts_with(c("TotAdultNoms", "Trusted_Adult", "SUICIDAL_IDEATION",      # dropping other suicide variables; can add them back in as needed
                       "Active_Exposure_Score", "Passive_Exposure_Score",  # Raw scale scores
                       "No_Contact_Perpetration_Score", "No_Contact_Victimization_Score",
                       "Contact_Perpetration_Score", "Contact_Victimization_Score",
                       "HNC_Perpetration_Score", "HNC_Victimization_Score",
                       "Cybersex_Perpetration_Score", "SV_Dismissiveness_Score",
                       "General_Well.being_Score", "SH_Help_Attitudes_Score", "SH_Help_Seeking_Score",
                       "Bullying_Score", "Cyberbullying_Score", "Peer_Victimization_Score", "Depression.Anxiety_Score")),
         any_of(names(impexpyear.wide)),  -school_n, -school_n100,    # school variables from impexyear; exclude W1 school n's b/c we only have them for Tx schools whereas SchN is for all
         Rural, SchN100, SchN, TeasingP = Teasing_Per, MHTP = MH_Training_Per,  # school variables from 2018-2019 (Year 2) Colorado or school climate survey; SchN is standardized version of SchN100
         -ends_with("_Avg"), -starts_with("std_"), ) # dropping school-level exposure (std_) variables for now. May reconsider

## Renaming variables to <= 8 characters per Mplus requirement
ds4 <- ds4 %>%
  rename_with(~str_replace(.x, "HasASurvey", "srvy"), .cols = starts_with("HasASurvey")) %>%          
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
  rename_with(~str_replace(.x, "SH_Help_Attitudes_Score_W", "SHAS_"), .cols = starts_with("SH_Help_Attitudes_Score_W")) %>%
  rename_with(~str_replace(.x, "SH_Help_Seeking_Score_W", "SHIS_"), .cols = starts_with("SH_Help_Seeking_Score_W")) %>%
  rename_with(~str_replace(.x, "Bullying_Score_W", "BLLY_"), .cols = starts_with("Bullying_Score_W")) %>%
  rename_with(~str_replace(.x, "Cyberbullying_Score_W", "CBLY_"), .cols = starts_with("Cyberbullying_Score_W")) %>%
  rename_with(~str_replace(.x, "Peer_Victimization_Score_W", "PEER_"), .cols = starts_with("Peer_Victimization_Score_W")) %>%
  rename_with(~str_replace(.x, "Depression.Anxiety_Score_W", "DEPS_"), .cols = starts_with("Depression.Anxiety_Score_W")) %>%
  # rename_with(~str_replace_all(.x, "std", "s"), .cols = starts_with("std")) %>%
  # rename_with(~str_replace_all(.x, "active_exp", "aex"), .cols = contains("active_exp")) %>%
  # rename_with(~str_replace_all(.x, "passive_exp", "pex"), .cols = contains("passive_exp")) %>%
  rename_with(~str_replace_all(.x, "meet", "m"), .cols = contains("meet")) %>%
  rename_with(~str_replace_all(.x, "camp", "c"), .cols = contains("camp")) %>%
  rename_with(~str_replace_all(.x, "trained", "t"), .cols = contains("trained")) %>%
  rename_with(~str_replace_all(.x, "att", "a"), .cols = contains("att")) %>%
  rename_with(~str_replace_all(.x, "perc", "pc"), .cols = contains("perc")) %>%
  rename_with(~str_replace_all(.x, "active_prop", "acp"), .cols = contains("active_prop")) %>%
  rename_with(~str_remove_all(.x, "aa_pl_"), .cols = starts_with("aa_pl_")) %>%
  rename_with(~str_remove_all(.x, "avg"), .cols = contains("avg")) %>%
  rename_with(~str_remove_all(.x, "inv"), .cols = contains("inv")) %>%
  rename_with(~str_remove_all(.x, "max"), .cols = contains("max")) %>%
  rename_with(~str_remove_all(.x, "_it"), .cols = contains("_it")) %>%
  rename_with(~str_remove_all(.x, "_"), .cols = contains("_"))


  
# double-checking length of names
table(nchar(names(ds4)))
# ratio = PL:AA ratio; ratioz = standardized version
# plpc = %PL
# plmapc = Avg PL meeting attendance (expressed as %)
# SchN = centered(school n / 100)

## exporting mplus data
# write.csv(ds4,   file = "mplus_impexp_colnames.csv", row.names = FALSE, na = "-999")
# write.table(ds4, file = "mplus_impexp.csv", row.names = FALSE, col.names = FALSE,  na = "-999", sep = ",")
saveRDS(ds4, file = "SoS Data and Codes/mplus_impexp.rds")

##########################################################################

# ------     Running Models    -------------------------
## Defining outcome names and shorthands
# Note: readModels organizes them in alphabetical order
impexpout = c(NCPS = "SH Perpetration", NCVS = "SH Victimization", CPS = "FSC Perpetration", CVS = "FCS Victimization", 
                  HOPS = "HNC Perpetration", HOVS = "HNC Victimization", CYPS = "Cybersex Perpetration", DSVS = "SV Dismissiveness",
                  GWBS = "General Well-Being", SHAS = "SH Attitudes", SHIS = "SH Help Intent", BLLY = "Bully Perpetration",
                  CBLY = "Cyberbully Perpetration", PEER = "Peer Victimization", DEPS = "Depression/Anxiety")
impexpbi = c(TA = "Trusted Adult", SI = "Suicide Ideation")

####################################################

# Part A are the models run in 4b-RunRead.R

# ------   Exposure With Implementation (Part B)   ------------

# Just doing Tx sample produces error that chi-square correction factor is negative
# adding ratioz (PL:AA ratio) yields terrible fit
impexp.ratio.sntx <- mplusObject(
  rdata = ds4, #[ds4$Tx==1,],
  usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
                   paste0("plpcy", 1:2), paste0("plmapcy", 1:2), paste0("ratiozy", 1:2),
                   "SchN", "Rural"),
  TITLE = glue::glue("Implementation to Exposure;"),
  VARIABLE = "IDVARIABLE = ID;
              CLUSTER = School;",
  ANALYSIS = "TYPE = COMPLEX;",
  MODEL = glue::glue("
                      AEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
                      PEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
                      AEXS3 ON AEXS2 PEXS2;
                      PEXS3 ON AEXS2 PEXS2;
                      AEXS4 WITH PEXS4;
                      AEXS3 WITH PEXS3;
                      AEXS2 WITH PEXS2;
                      AEXS2 PEXS2 ON plpcy1 plmapcy1 ratiozy1 SchN Rural;
                      AEXS3 PEXS3 ON plpcy1 plmapcy1 ratiozy1;
                      AEXS4 PEXS4 ON plpcy2 plmapcy2 ratiozy2;
                      plpcy1 plpcy2 plmapcy1 plmapcy2 ratiozy1 ratiozy2 WITH SchN Rural;
                      plpcy1 WITH plpcy2 plmapcy1 plmapcy2 ratiozy1 ratiozy2;
                      plpcy2 WITH plmapcy1 plmapcy2 ratiozy1 ratiozy2;
                      plmapcy1 WITH plmapcy2 ratiozy1 ratiozy2;
                      plmapcy2 WITH ratiozy1 ratiozy2;
                      ratiozy1 WITH ratiozy2;
                      SchN WITH Rural;
                     [plpcy1 plpcy2 plmapcy1 plmapcy2 ratiozy1 ratiozy2 SchN Rural];"),
  OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 MODINDICES (ALL);")

impexp.sntx <- mplusObject(
  rdata = ds4, #[ds4$Tx==1,],
  usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
                   paste0("plpcy", 1:2), paste0("plmapcy", 1:2),
                   "SchN", "Rural"),
  TITLE = glue::glue("Implementation to Exposure;"),
  VARIABLE = "IDVARIABLE = ID;
              CLUSTER = School;",
  ANALYSIS = "TYPE = COMPLEX;",
  MODEL = glue::glue("
                      AEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
                      PEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
                      AEXS3 ON AEXS2 PEXS2;
                      PEXS3 ON AEXS2 PEXS2;
                      AEXS4 WITH PEXS4;
                      AEXS3 WITH PEXS3;
                      AEXS2 WITH PEXS2;
                      AEXS2 PEXS2 ON plpcy1 plmapcy1 SchN Rural;
                      AEXS3 PEXS3 ON plpcy1 plmapcy1;
                      AEXS4 PEXS4 ON plpcy2 plmapcy2;
                      plpcy1 plpcy2 plmapcy1 plmapcy2 WITH SchN Rural;
                      plpcy1 WITH plpcy2 plmapcy1 plmapcy2;
                      plpcy2 WITH plmapcy1 plmapcy2;
                      plmapcy1 WITH plmapcy2;
                      SchN WITH Rural;
                     [plpcy1 plpcy2 plmapcy1 plmapcy2 SchN Rural];"),
  OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 MODINDICES (ALL);")


####   Generate and Run Mplus Files   #####

impexp.run <- mplusModeler(impexp.sntx,
                           dataout = glue::glue("Mplus/Automatr/Implementation/IMP_EXP.dat"),
                           modelout = glue::glue("Mplus/Automatr/Implementation/IMP_EXP.inp"),
                           run = 1, check = TRUE, hashfilename = TRUE,
                           writeData = "always")

#####   Extract Results from Mplus files   ####

impexp.preds <- c(PLPC = "PPL", PLMAPC = "PL Meeting Att", SCHN = "School Size", RURAL = "Rural")

## Importing models
impexp.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/Implementation")

## Model Fit
impexp.fit <- mplus_fit(impexp.mods$imp_exp.out, digits = 2)

## Fixed effect estimates - std, stdy, or stdyx for continuous outcome models?
# binary outcome estimates will be unstandardized and converted to odds ratio
# From discussions below, seems like stdyx is probably the best way to go
# http://www.statmodel.com/discussion/messages/11/8505.html?1583623607
# http://www.statmodel.com/discussion/messages/12/957.html?1344516621
impexp.est <- mplus_est(impexp.mods$imp_exp.out, std = "stdyx.standardized", params = c("ON"),
                              digits = 2, combine = TRUE, ci = FALSE) %>%
  filter(param %in% c("PLPCY1", "PLPCY2", "PLMAPCY1", "PLMAPCY2","RURAL", "SCHN")) %>%
  select(paramHeader, param, estimate) %>%
  mutate(paramHeader = str_remove(paramHeader, "\\.ON"),
         param = str_remove(param, "Y[0-9]") %>%
           recode(., !!!impexp.preds) %>%
           factor(., levels = impexp.preds)) %>%
  spread(paramHeader, estimate) %>%
  mutate(across(.cols = -param, ~replace_na(., "-"))) %>%
  arrange(param) %>%
  bind_rows(
    mplus_est(impexp.mods$imp_exp.out, std = "r2") %>%
      select(param, est) %>%
      mutate(est = format(round(est, 2), nsmall = 2)) %>%
      spread(param, est) %>%
      mutate(param = "R2"))

# #### Attempt at using a Latent Growth Model for Exposure ####
# # Latent Growth Model for Passive produces warning:
# # THE CHI-SQUARE COULD NOT BE COMPUTED.
# # THE CORRECTION FACTOR IS NEGATIVE.
# # Latent Growth Model for Active had horrible fit (RMSEA = .46, CFI = .10)
# # For both Active and Passive; doesn't matter if full sample or Tx only is used
# impexpis.sntx <- mplusObject(
#   rdata = ds4[ds4$Tx==1,],
#   usevariables = c("ID", "School", paste0("PEXS", 2:4), #paste0("PEXS", 2:4),
#                    "SchN", "Rural", paste0("plpcy", 1:2), paste0("plmapcy", 1:2)),
#   TITLE = glue::glue("Implementation to Exposure;"),
#   VARIABLE = "IDVARIABLE = ID;
#               CLUSTER = School;",
#   ANALYSIS = "TYPE = COMPLEX;",
#   MODEL = glue::glue("ai as | PEXS2@0 PEXS3@1 PEXS4@2;
#                      ai as ON SchN Rural;
#                      PEXS2 ON plpcy1 plmapcy1;
#                      PEXS3 ON plpcy1 plmapcy1;
#                      PEXS4 ON plpcy2 plmapcy2;
#                      plpcy1 plpcy2 plmapcy1 plmapcy2 WITH SchN Rural;
#                      plpcy1 WITH plpcy2 plmapcy1 plmapcy2;
#                      plpcy2 WITH plmapcy1 plmapcy2;
#                      plmapcy1 WITH plmapcy2;
#                      Rural WITH SchN;
#                      [plpcy1 plpcy2 plmapcy1 plmapcy2 SchN Rural];"),
#   OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 MODINDICES (ALL);")
# 
# # pi ps | PEXS2@0 PEXS3@1 PEXS4@2;
# # ai WITH as pi ps;
# # pi WITH as ps;
# # as WITH ps;
# # AEXS2 PEXS2 ON plpcy1 plmapcy1;
# # AEXS3 PEXS3 ON plpcy1 plmapcy1;
# # AEXS4 PEXS4 ON plpcy2 plmapcy2;
# 
# 
# ####   Generate and Run Mplus Files   #####
# 
# impexpis.run <- mplusModeler(impexpis.sntx,
#                              dataout = glue::glue("Mplus/Automatr/Implementation/IMP_EXP_IS_TX.dat"),
#                              modelout = glue::glue("Mplus/Automatr/Implementation/IMP_EXP_IS_TX.inp"),
#                              run = 1, check = TRUE, hashfilename = TRUE,
#                              writeData = "always")


########################################################


# ------   All Together Now (Part C)   ------------


#### Creating Mplus Syntax ####
# ## Practice syntax before mapping across all outcomes
# ncps.imp.sntx <- mplusObject(
#   rdata = ds4, #[ds4$Tx==1,]
#   usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
#                    paste0("NCPS", 1:4), "SchN", "Rural",
#                    paste0("plpcy", 1:2), paste0("plmapcy", 1:2)),
#   TITLE = glue::glue("SH Perpetration - Exposure only;"),
#   VARIABLE = "IDVARIABLE = ID;
#               CLUSTER = School;",
#   ANALYSIS = "TYPE = COMPLEX;",
#   MODEL = glue::glue("i s | NCPS1@0 NCPS2@1 NCPS3@2 NCPS4@3;
#                       i s ON SchN Rural;
#                       NCPS2 ON AEXS2 PEXS2;
#                       NCPS3 ON AEXS3 PEXS3;
#                       NCPS4 ON AEXS4 PEXS4;
#                       AEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
#                       PEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
#                       AEXS3 ON AEXS2 PEXS2;
#                       PEXS3 ON AEXS2 PEXS2;
#                       AEXS4 WITH PEXS4;
#                       AEXS3 WITH PEXS3;
#                       AEXS2 WITH PEXS2;
#                       AEXS2 PEXS2 ON plpcy1 plmapcy1 SchN Rural;
#                       AEXS3 PEXS3 ON plpcy1 plmapcy1;
#                       AEXS4 PEXS4 ON plpcy2 plmapcy2;
#                       plpcy1 plpcy2 plmapcy1 plmapcy2 WITH SchN Rural i@0 s@0;
#                       plpcy1 plpcy2 plmapcy1 plmapcy2 WITH SchN Rural;
#                       plpcy1 WITH plpcy2 plmapcy1 plmapcy2;
#                       plpcy2 WITH plmapcy1 plmapcy2;
#                       plmapcy1 WITH plmapcy2;
#                       SchN WITH Rural;
#                      [plpcy1 plpcy2 plmapcy1 plmapcy2 SchN Rural];"),
#   OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 MODINDICES (ALL);")
# 
# 
# 
# 
# ####   Generate and Run Mplus Files   #####
# 
# ncps.imp.run <- mplusModeler(ncps.imp.sntx,
#                          dataout = glue::glue("Mplus/Automatr/Exposure Imp/Prac2_EXP_IMP.dat"),
#                          modelout = glue::glue("Mplus/Automatr/Exposure Imp/Prac2_EXP_IMP.inp"),
#                          run = 1, check = TRUE, hashfilename = TRUE,
#                          writeData = "always")


#### Creating Mplus Syntax ####

imp.sntx <- map2(.x = impexpout, .y = names(impexpout),
                    ~mplusObject(
  rdata = ds4,
  usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
                   paste0(.y, 1:4), "SchN", "Rural",
                   paste0("plpcy", 1:2), paste0("plmapcy", 1:2)),
  TITLE = glue::glue("{.x} - With Exposure;"),
  VARIABLE = "IDVARIABLE = ID;
              CLUSTER = School;",
  ANALYSIS = "TYPE = COMPLEX;",
  MODEL = glue::glue("i s | {.y}1@0 {.y}2@1 {.y}3@2 {.y}4@3;
                      i s ON SchN Rural;
                      {.y}2 ON AEXS2 PEXS2;
                      {.y}3 ON AEXS3 PEXS3;
                      {.y}4 ON AEXS4 PEXS4;
                      AEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
                      PEXS4 ON AEXS2 PEXS2 AEXS3 PEXS3;
                      AEXS3 ON AEXS2 PEXS2;
                      PEXS3 ON AEXS2 PEXS2;
                      AEXS4 WITH PEXS4;
                      AEXS3 WITH PEXS3;
                      AEXS2 WITH PEXS2;
                      AEXS2 PEXS2 ON plpcy1 plmapcy1 SchN Rural;
                      AEXS3 PEXS3 ON plpcy1 plmapcy1;
                      AEXS4 PEXS4 ON plpcy2 plmapcy2;
                      plpcy1 plpcy2 plmapcy1 plmapcy2 WITH SchN Rural i@0 s@0;
                     [plpcy1 plpcy2 plmapcy1 plmapcy2 SchN Rural];"),
  OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 MODINDICES (ALL);"))


####   Generate and Run Mplus Files   #####

imp.run <- map2(.x = imp.sntx, .y = names(imp.sntx),
               ~mplusModeler(.x,
                             dataout = glue::glue("Mplus/Automatr/Exposure Imp/{.y}_EXP_IMP.dat"),
                             modelout = glue::glue("Mplus/Automatr/Exposure Imp/{.y}_EXP_IMP.inp"),
                             run = 1, check = TRUE, hashfilename = TRUE,
                             writeData = "always"))


#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later

## Importing models
imp.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/Exposure Imp")

## Model Fit
imp.fit <- map_dfr(imp.mods, ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_exp_imp.out", recodes = impexpout, tolower = TRUE) %>%
  select(Outcome = outcome, chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, `90P CI` = rmsea.ci, SRMR = srmr)

## Fixed effect estimates - outcomes paper order c(6,7,1,2,4,5,3) - not correct anymore b/c deps model was added after
imp.est <- map2(.x = imp.mods[c(11, 12, 3, 4, 9, 10, 5, 7, 8, 14, 15, 1, 2, 13, 6)], #adjusting order to sync with impexpout rather than be alphabetic
                .y = names(impexpout),
               ~mplus_est(.x, std = "stdyx.standardized", params = c("ON"),
                          digits = 2, combine = TRUE, ci = FALSE) %>%
                 filter(str_detect(paramHeader, paste(c("I\\.", "S\\.", toupper(str_remove(names(imp.mods), "_exp_imp.out"))), collapse = "|"))) %>%
                 mutate(Effect = case_when(param == "SCHN" & paramHeader == "I.ON" ~ "School Size -> Intercept",
                                           param == "SCHN" & paramHeader == "S.ON" ~ "School Size -> Slope",
                                           param == "RURAL" & paramHeader == "I.ON" ~ "Rural -> Intercept",
                                           param == "RURAL" & paramHeader == "S.ON" ~ "Rural -> Slope",
                                           str_detect(param, "AEXS") ~ str_replace(param, "AEXS", "PWave "),
                                           str_detect(param, "PEXS") ~ str_replace(param, "PEXS", "NPWave "))) %>%
                 select(Effect, estimate) %>%
                 bind_rows(data.frame(Effect = c("Participatory -> Outcome", "Non-participatory -> Outcome"), estimate = NA)) %>%
                 rename(!!.y := estimate) %>%
                 mutate(Effect = factor(Effect, levels = c("Participatory -> Outcome", "PWave 2", "PWave 3", "PWave 4",
                                                           "Non-participatory -> Outcome", "NPWave 2", "NPWave 3", "NPWave 4",
                                                           "School Size -> Intercept", "School Size -> Slope",
                                                           "Rural -> Intercept", "Rural -> Slope"))) %>%
                 arrange(Effect))

## Combine estimates into a single table for publication
imp.est.df <- purrr::reduce(imp.est, dplyr::left_join, by = 'Effect')


####   Compiling results objects into one object    ####
imp.results <- list(Models = imp.mods,
                    Fit = imp.fit,
                    Estimates = imp.est,
                    Estimate_Table = imp.est.df,
                    R2 = map_dfr(.x = imp.mods, ~.x$parameters$r2, .id = "outcome") %>%
                      outcome_shortcut(remove = "_exp_imp.out", recodes = impexpout, tolower = TRUE),
                    Warn_Error = map_dfr(.x = imp.mods,
                                         ~data.frame(errors = length(.x$errors),
                                                     warnings = length(.x$warnings)),
                                         .id = "outcome") %>%
                      outcome_shortcut(remove = "_exp_imp.out", recodes = impexpout, tolower = TRUE))

######################################################################

# --------   Saving Results    ------------
save(impexpout, impexpbi, ds4, impexp.preds,
     impexp.sntx, impexp.mods, impexp.fit, impexp.est,
     imp.sntx, imp.results,
     file = "Output/Implmentation_Exposure_Models.RData")
# load("Output/Implmentation_Exposure_Models.RData")



# ------   Exposure Only Growth Models    ------------
# NOTE: Will not be using these models

# ncps.sntx <- mplusObject(
#   rdata = ds4,
#   usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
#                    paste0("NCPS", 1:4)),
#   TITLE = glue::glue("SH Perpetration - Exposure only;"),
#   VARIABLE = glue::glue("IDVARIABLE = ID;
#                           CLUSTER = School;"),
#   ANALYSIS = "TYPE = COMPLEX;",
#   MODEL = glue::glue("i s | NCPS1@0 NCPS2@1 NCPS3@2 NCPS4@3;
#                       NCPS2 ON AEXS2 PEXS2;
#                       NCPS3 ON AEXS3 PEXS3;
#                       NCPS4 ON AEXS4 PEXS4;
#                       AEXS2-AEXS4 PEXS2-PEXS4 WITH i@0;
#                       AEXS2-AEXS4 PEXS2-PEXS4 WITH s@0;
#                       [AEXS2-AEXS4 PEXS2-PEXS4];"),
#   OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;")
# 
# ncps.run <- mplusModeler(ncps.sntx,
#                          dataout = glue::glue("Mplus/Automatr/Exposure only/NCPS_EXP_ONLY.dat"),
#                          modelout = glue::glue("Mplus/Automatr/Exposure only/NCPS_EXP_ONLY.inp"),
#                          run = 1, check = TRUE, hashfilename = TRUE,
#                          writeData = "ifmissing")
# 
# #### Creating Mplus Syntax ####
# # Trusted adult needs to be run separately
# exponly.sntx <- map2(.x = impexpout[-impexpout.len], .y = names(impexpout)[-impexpout.len],
#                      ~mplusObject(
#                        rdata = ds4,
#                        usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
#                                         paste0(.y, 1:4)),
#                        TITLE = glue::glue("{.x} - Exposure only;"),
#                        VARIABLE = "IDVARIABLE = ID;
#                           CLUSTER = School;",
#                        ANALYSIS = "TYPE = COMPLEX;",
#                        MODEL = glue::glue("i s | {.y}1@0 {.y}2@1 {.y}3@2 {.y}4@3;
#                       {.y}2 ON AEXS2 PEXS2;
#                       {.y}3 ON AEXS3 PEXS3;
#                       {.y}4 ON AEXS4 PEXS4;
#                       AEXS2-AEXS4 PEXS2-PEXS4 WITH i@0 s@0;
#                       [AEXS2-AEXS4 PEXS2-PEXS4];"),
#                        OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))
# 
# # Trusted Adult - Apparently need to use montecarlo integration
# exponly.sntx$TA <- mplusObject(
#   rdata = ds4,
#   usevariables = c("ID", "School", paste0("AEXS", 2:4), paste0("PEXS", 2:4),
#                    paste0("TA", 1:4)),
#   TITLE = "Trusted Adult - Exposure Only;",
#   VARIABLE = "CATEGORICAL = TA1-TA4;
#               IDVARIABLE = ID;
#               CLUSTER = School;",
#   ANALYSIS = "TYPE = COMPLEX;
#               ESTIMATOR = MLR;
#               LINK = LOGIT;
#               ALGORITHM = INTEGRATION;
#               INTEGRATION = MONTECARLO (5000);",
#   MODEL = "i s | TA1@0 TA2@1 TA3@2 TA4@3;
#            TA2 ON AEXS2 PEXS2;
#            TA3 ON AEXS3 PEXS3;
#            TA4 ON AEXS4 PEXS4;
#            AEXS2-AEXS4 PEXS2-PEXS4 WITH i@0 s@0;
#            ![AEXS2-AEXS4 PEXS2-PEXS4];
#            [TA1$1-TA4$1@0]; 
#            [i];",
#   OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 TECH10;") 
# # [TA_1$1-TA_4$1@0];  constrains threshold at each time to 0; this or intercept mean to 0 for identification (Lee et al., 2018)
# 
# 
# ####   Generate and Run Mplus Files   #####
# exponly.run <- map2(.x = exponly.sntx, .y = names(exponly.sntx),
#                     ~mplusModeler(.x,
#                                   dataout = glue::glue("Mplus/Automatr/Exposure only/{.y}_EXP_ONLY.dat"),
#                                   modelout = glue::glue("Mplus/Automatr/Exposure only/{.y}_EXP_ONLY.inp"),
#                                   run = 1, check = TRUE, hashfilename = TRUE,
#                                   writeData = "always"))
# 
# 
# 
# #####   Extract Results from Mplus files   ####
# 
# ## Importing models
# exponly.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/Exposure Only")
# 
# ## Model Fit
# exponly.fit <- map_dfr(exponly.mods[-impexpout.len], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
#   outcome_shortcut(remove = "_exp_only.out", recodes = impexpout[-impexpout.len], tolower = TRUE)
# 
# 
# ## Fixed effect estimates - std, stdy, or stdyx for non-TA models (TA will be unstandardized)?
# # From discussions below, seems like stdyx is probably the best way to go
# # http://www.statmodel.com/discussion/messages/11/8505.html?1583623607
# # http://www.statmodel.com/discussion/messages/12/957.html?1344516621
# exponly.est <- map(.x = exponly.mods[-impexpout.len],
#                    ~mplus_est(.x, std = "stdyx.standardized", params = c("ON", "WITH", "Means"),
#                               digits = 2, combine = TRUE, ci = FALSE) %>%
#                      filter(str_detect(paramHeader, "\\.ON|Means") | (str_detect(paramHeader, "\\.WITH") & est !=0)) %>%
#                      select(parameter, estimate))
# 
# ####   Compiling results objects into one object    ####
# exponly.results <- list(Models = exponly.mods[-impexpout.len],
#                         Fit = exponly.fit,
#                         Estimates = exponly.est,
#                         R2 = map_dfr(.x = exponly.mods[-impexpout.len], ~.x$parameters$r2, .id = "outcome") %>%
#                           outcome_shortcut(remove = "_exp_only.out", recodes = impexpout[-impexpout.len], tolower = TRUE),
#                         Warn_Error = map_dfr(.x = exponly.mods[-impexpout.len],
#                                              ~data.frame(errors = length(.x$errors),
#                                                          warnings = length(.x$warnings)),
#                                              .id = "outcome") %>%
#                           outcome_shortcut(remove = "_exp_only.out", recodes = impexpout[-impexpout.len], tolower = TRUE))

