#################################
#                               #
#      Sources of Strength      #
#     Outcome Model Analysis    #
#                               #
#################################

## Loading packages and functions
source("Scripts/PnF.R")
library(MplusAutomation)
# library(glue)

## Defining outcome names and shorthands
# Note: readModels organizes them in alphabetical order
mplusoutcomes = c(NCPS = "SH Perpetration", NCVS = "SH Victimization", CPS = "FSC Perpetration", CVS = "FCS Victimization", 
                  HOPS = "HNC Perpetration", HOVS = "HNC Victimization", CYPS = "Cybersex Perpetration", DSVS = "SV Dismissiveness",
                  GWBS = "General Well-Being", SHAS = "SH Attitudes", SHIS = "SH Help Intent", BLLY = "Bully Perpetration",
                  CBLY = "Cyberbully Perpetration", PEER = "Peer Victimization", DEPS = "Depression/Anxiety")
bioutcomes = c(TA = "Trusted Adult", SI = "Suicide Ideation")

# shortcut for excluding binary outcomes (Trusted Adult, Suicide Ideation) from models
drop.bi <- length(mplusoutcomes)

## import mplus dataset
# mpluswide2 <- readr::read_csv("Mplus/mpluswide_wbully_colnames.csv", col_types = "d", na = c("-999"))
mpluswide2 <- readRDS("SoS Data and Codes/mpluswide_wbully.rds")


## Investigating functional form
# this is not helpful
# ghettidata <- mpluswide2 %>%
#   select(ID, School, Tx, starts_with("NCPS")) %>%
#   gather(Temp, Score, starts_with("NCPS")) %>%
#   separate(Temp, into = c("NCPS", "Wave"), sep = "_") %>%
#   mutate(Wave = as.character(Wave)) %>%
#   filter(!is.na(Score))
# 
# ids <- sample(ghettidata$ID, 50)
# 
# ghettiplot <- ghettidata %>%
#   filter(ID %in% ids) %>%
#   ggplot(aes(x = Wave, y = Score)) +
#   geom_line(aes(group = ID)) +
#   stat_smooth(aes(group = 1), size = 2, method = "loess", se = TRUE) +
#   theme_bw(base_size = 18)



#  -------  Latent Growth Models  ---------------------

#### Creating Mplus Syntax ####

lgm.sntx <- map2(.x = mplusoutcomes, .y = names(mplusoutcomes),
                       ~mplusObject(
                         rdata = mpluswide2,
                         usevariables = c("ID", "School", paste0(.y, "_", 1:4)),
                         TITLE = glue::glue("{.x} - LGM;"),
                         VARIABLE ="IDVARIABLE = ID;
                                    CLUSTER = School;",
                         ANALYSIS = "TYPE = COMPLEX;",
                         MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;"),
                         OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

## binary outcome model
ta.lgm.sntx <- map2(.x = bioutcomes, .y = names(bioutcomes),
                    ~mplusObject(
                      rdata = mpluswide2,
                      usevariables = c("ID", "School", paste0(.y, "_", 1:4)),
                      TITLE = glue::glue("{.x} - LGM;"),
                      VARIABLE = glue::glue("CATEGORICAL = {.y}_1-{.y}_4;
              IDVARIABLE = ID;
              CLUSTER = School;"),
                      ANALYSIS = "TYPE = COMPLEX;
              ESTIMATOR = MLR;
              LINK = LOGIT;",
                      MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
  [{.y}_1$1-{.y}_4$1@0]; 
  [i];"),
                      OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 TECH10;"))
# [TA_1$1-TA_4$1@0];  constrains threshold at each time to 0; this or intercept mean to 0 (Mplus default) for identification (Lee et al., 2018)

####   Generate and Run Mplus Files   #####
# Note on writeData = "always" - The md5 hash is based on: (1) the dimensions of the dataset, (2) the variable names, (3) the class
# of every variable, and (4) the raw data from the first and last rows. This combination ensures that
# under most all circumstances, if the data changes, the hash will change

lgm.run <- map2(.x = lgm.sntx, .y = names(lgm.sntx),
               ~mplusModeler(.x,
                        dataout = glue::glue("Mplus/Automatr/LGM/{.y}_LGM.dat"),
                        modelout = glue::glue("Mplus/Automatr/LGM/{.y}_LGM.inp"),
                        run = 1, check = TRUE, hashfilename = TRUE,
                        writeData = "always"))

ta.lgm.run <- map2(.x = ta.lgm.sntx, .y = names(ta.lgm.sntx),
                   ~mplusModeler(.x,
                           dataout = glue::glue("Mplus/Automatr/LGM/{.y}_LGM.dat"),
                           modelout = glue::glue("Mplus/Automatr/LGM/{.y}_LGM.inp"),
                           run = 1, check = TRUE, hashfilename = TRUE,
                           writeData = "always"))


#####   Extract Results from Mplus files   ####
# Note: all binary outcome results (Trusted Adult, Suicide Ideation) will be gathered separately later
# lgm.preds <- c(I = "Intercept", S = "Intercept")

## Importing models and converting results to tables
lgm.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/LGM")
lgm.fit <- map_dfr(lgm.mods[names(lgm.mods) %in% paste0(tolower(names(mplusoutcomes)), "_lgm.out")],
                   ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_lgm.out", recodes = mplusoutcomes, tolower = TRUE)
# lgm.fit will go into supplemental materials


lgm.est <- map_dfr(lgm.mods[names(lgm.mods) %in% paste0(tolower(names(mplusoutcomes)), "_lgm.out")],
                   ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized"), .id = "outcome") %>%
  outcome_shortcut(remove = "_lgm.out", recodes = mplusoutcomes, tolower = TRUE) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(outcome, Means_I, Variances_I, Means_S, Variances_S, S.WITH_I)

####   Compiling results objects into one object    ####
lgm.results <- list(Models = lgm.mods[names(lgm.mods) %in% paste0(tolower(names(mplusoutcomes)), "_lgm.out")],
                       Fit = lgm.fit,
                       Estimates = lgm.est,
                       R2 = map_dfr(.x = lgm.mods[names(lgm.mods) %in% paste0(tolower(names(mplusoutcomes)), "_lgm.out")],
                                    ~.x$parameters$r2, .id = "outcome") %>%
                         outcome_shortcut(remove = "_lgm.out", recodes = mplusoutcomes, tolower = TRUE),
                       Warn_Error = map_dfr(.x = lgm.mods[names(lgm.mods) %in% paste0(tolower(names(mplusoutcomes)), "_lgm.out")],
                                            ~data.frame(errors = length(.x$errors),
                                                        warnings = length(.x$warnings)),
                                            .id = "outcome") %>%
                         outcome_shortcut(remove = "_lgm.out", recodes = mplusoutcomes, tolower = TRUE))

######################################################################################

# ------  Quadratic GM   ------------
## These are run to compare to the linear models
quad.sntx <- map2(.x = mplusoutcomes, .y = names(mplusoutcomes),
                 ~mplusObject(
                   rdata = mpluswide2,
                   usevariables = c("ID", "School", paste0(.y, "_", 1:4)),
                   TITLE = glue::glue("{.x} - Quadratic;"),
                   VARIABLE ="IDVARIABLE = ID;
                                    CLUSTER = School;",
                   ANALYSIS = "TYPE = COMPLEX;",
                   MODEL = glue::glue("i s q | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;"),
                   OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

quad.run <- map2(.x = quad.sntx, .y = names(quad.sntx),
                ~mplusModeler(.x,
                              dataout = glue::glue("Mplus/Automatr/LGM/Quadratic/{.y}_QUAD.dat"),
                              modelout = glue::glue("Mplus/Automatr/LGM/Quadratic/{.y}_QUAD.inp"),
                              run = 1, check = TRUE, hashfilename = TRUE,
                              writeData = "always"))

quad.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/LGM/Quadratic")

quad.fit <- map_dfr(quad.mods, ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_quad.out", recodes = mplusoutcomes, tolower = TRUE)

## comparing linear and quadratic
lq.comp <- map2_dfr(.x = lgm.mods[-c(16:17)], .y = quad.mods,
     ~mplus_sb2001(.x, .y), .id = "outcome") %>%
  outcome_shortcut(remove = "_lgm.out", recodes = mplusoutcomes, tolower = TRUE)

# Linear better except for SH intent help and SH attitudes


###########################################################################

# ---- Exploring use of multilevel model LGM ----------------
## Multilevel Model - https://www.statmodel.com/usersguide/chap9/ex9.12.html
ml.lgm.sntx <- map2(.x = mplusoutcomes, .y = names(mplusoutcomes), 
                    ~mplusObject(rdata = mpluswide2,
                                 usevariables = c("ID", "School", paste0(.y, "_", 1:4)),
                                 TITLE = glue::glue("{.x} - Multilevel LGM;"),
                                 VARIABLE = "IDVARIABLE = ID;
              CLUSTER = School;",
                                 ANALYSIS = "TYPE = TWOLEVEL;",
                                 MODEL = glue::glue("%WITHIN%
  iw sw | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
  {.y}_1-{.y}_4 (1);
  %BETWEEN%
  ib sb | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
  {.y}_1-{.y}_4@0;"),
                                 OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))
# (1) constrains within (residual) variance to be equal at across time points
# between (residual) variance constrained to 0

ml.lgm.run <- map2(.x = ml.lgm.sntx, .y = names(ml.lgm.sntx),
                   ~mplusModeler(.x,
                                 dataout = glue::glue("Mplus/Automatr/LGM/Multilevel/{.y}_ML_LGM.dat"),
                                 modelout = glue::glue("Mplus/Automatr/LGM/Multilevel/{.y}_ML_LGM.inp"),
                                 run = 1, check = TRUE, hashfilename = TRUE,
                                 writeData = "always"))


#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later

## Importing models and converting results to tables
ml.lgm.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/LGM/Multilevel")
ml.lgm.fit <- map_dfr(ml.lgm.mods, ~mplus_fit(.x, digits = 2, ml = TRUE), .id = "outcome") %>%
  outcome_shortcut(remove = "_ml_lgm.out", recodes = mplusoutcomes, tolower = TRUE)

ml.lgm.est <- map_dfr(ml.lgm.mods, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized"), .id = "outcome") %>%
  outcome_shortcut(remove = "_ml_lgm.out", recodes = mplusoutcomes, tolower = TRUE) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate)


####   Compiling results objects into one object    ####
ml.lgm.results <- list(Models = ml.lgm.mods,
                    Fit = ml.lgm.fit,
                    Estimates = lgm.est,
                    R2 = map_dfr(.x = ml.lgm.mods, ~.x$parameters$r2, .id = "outcome") %>%
                      outcome_shortcut(remove = "_ml_lgm.out", recodes = mplusoutcomes, tolower = TRUE),
                    Warn_Error = map_dfr(.x = ml.lgm.mods,
                                         ~data.frame(errors = length(.x$errors),
                                                     warnings = length(.x$warnings)),
                                         .id = "outcome") %>%
                      outcome_shortcut(remove = "_ml_lgm.out", recodes = mplusoutcomes, tolower = TRUE))

############################################################33

# ------   Tx Only Growth Models    ------------

#### Creating Mplus Syntax ####
only.sntx <- map2(.x = mplusoutcomes, .y = names(mplusoutcomes),
                        ~mplusObject(
                          rdata = mpluswide2,
                          usevariables = c("ID", "School", "Tx", paste0(.y, "_", 1:4)),
                          TITLE = glue::glue("{.x} - Tx only;"),
                          VARIABLE = "IDVARIABLE = ID;
                          CLUSTER = School;",
                          ANALYSIS = "TYPE = COMPLEX;",
                          MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
                          i s ON Tx;
                          [Tx];"),
                          OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

## binary outcome models
ta.only.sntx <- map2(.x = bioutcomes, .y = names(bioutcomes),
     ~mplusObject(
       rdata = mpluswide2,
       usevariables = c("ID", "School", "Tx", paste0(.y, "_", 1:4)),
       TITLE = glue::glue("{.x} - TX only;"),
       VARIABLE = glue::glue("CATEGORICAL = {.y}_1-{.y}_4 Tx;
              IDVARIABLE = ID;
              CLUSTER = School;"),
       ANALYSIS = "TYPE = COMPLEX;
              ESTIMATOR = MLR;
              LINK = LOGIT;",
       MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
         i s ON Tx;
         Tx;
         [{.y}_1$1-{.y}_4$1@0]; 
         [i];"),
       OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 TECH10;"))

# [TA_1$1-TA_4$1@0];  constrains threshold at each time to 0; this or intercept mean to 0 for identification (Lee et al., 2018)


####   Generate and Run Mplus Files   #####

only.run <- map2(.x = only.sntx, .y = names(only.sntx),
                 ~mplusModeler(.x,
                               dataout = glue::glue("Mplus/Automatr/TX ONLY/{.y}_TX_ONLY.dat"),
                               modelout = glue::glue("Mplus/Automatr/TX ONLY/{.y}_TX_ONLY.inp"),
                               run = 1, check = TRUE, hashfilename = TRUE,
                               writeData = "always"))

ta.only.run <- map2(.x = ta.only.sntx, .y = names(ta.only.sntx),
                    ~mplusModeler(.x,
                                  dataout = glue::glue("Mplus/Automatr/TX ONLY/{.y}_TX_ONLY.dat"),
                                  modelout = glue::glue("Mplus/Automatr/TX ONLY/{.y}_TX_ONLY.inp"),
                                  run = 1, check = TRUE, hashfilename = TRUE,
                                  writeData = "always"))


#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later
tx.only.preds <- c(I = "Intercept", S = "Intercept", TX = "Sources")

## Importing models
tx.only.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/TX ONLY")

## Model Fit
tx.only.fit <- map_dfr(tx.only.mods[names(tx.only.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_only.out")],
                       ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_tx_only.out", recodes = mplusoutcomes, tolower = TRUE)

## Fixed effect estimates
tx.only.est <- map_dfr(.x = tx.only.mods[names(tx.only.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_only.out")],
                          ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"),
                                     digits = 2, combine = TRUE, ci = FALSE) %>%
                            filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                            mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                                   predictor = recode(param, !!!tx.only.preds) %>%
                                     factor(., levels = unique(tx.only.preds)),
                                   temp = paste(predictor, is, sep = ".")) %>%  # specific to latent growth models where I and S recoded to same value
                            select(temp, estimate) %>%
                            spread(temp, estimate), .id = "outcome") %>%
  outcome_shortcut(remove = "_tx_only.out", recodes = mplusoutcomes, tolower = TRUE) %>%
  select(outcome, Sources.I, Sources.S)

####   Compiling results objects into one object    ####
tx.only.results <- list(Models = tx.only.mods[names(tx.only.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_only.out")],
                        Fit = tx.only.fit,
                        Estimates = tx.only.est,
                        R2 = map_dfr(.x = tx.only.mods[names(tx.only.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_only.out")],
                                     ~.x$parameters$r2, .id = "outcome") %>%
                          outcome_shortcut(remove = "_tx_only.out", recodes = mplusoutcomes, tolower = TRUE),
                        Warn_Error = map_dfr(.x = tx.only.mods[names(tx.only.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_only.out")],
                                             ~data.frame(errors = length(.x$errors),
                                                         warnings = length(.x$warnings)),
                                             .id = "outcome") %>%
                          outcome_shortcut(remove = "_tx_only.out", recodes = mplusoutcomes, tolower = TRUE))

#### Extracting Results for publication ####
# Note. Unconditional and Tx only models are combined for output

## Model fit - formatting will occur in 5bc-OutcomePaper.Rmd
unctx.fit <- select(lgm.results$Fit, -npar) %>% filter(outcome %in% mplusoutcomes[c(1:6, 8)]) %>%
  left_join(select(tx.only.results$Fit, -npar), by = "outcome") # don't need to filter here since we are using left_join

## Estimates
unctx.est <- tx.only.results$R2 %>%
  filter(param %in% c("I", "S") & outcome %in% mplusoutcomes[c(1:6, 8)]) %>%
  select(outcome, param, est) %>%
  spread(param, est) %>%
  mutate(across(.cols = c(I, S), ~format(round(., 2), nsmall = 2))) %>%
  mutate(blank = NA) %>%
  left_join(tx.only.results$Estimates, by = "outcome") %>%
  left_join(lgm.results$Estimates, by = "outcome") %>%
  select(outcome, Means_I:S.WITH_I, blank, Sources.I, I, Sources.S, S)


#### Grabbing results for non-pub outcomes ####
## Model fit - formatting will occur in 5bc-OutcomePaper.Rmd
nopubtx.fit <- select(lgm.results$Fit, -npar) %>% filter(outcome %in% mplusoutcomes[c(7,9:15)]) %>%
  left_join(select(tx.only.results$Fit, -npar), by = "outcome") # don't need to filter here since we are using left_join

## Estimates
nopubtx.est <- tx.only.results$R2 %>%
  filter(param %in% c("I", "S") & outcome %in% mplusoutcomes[c(7,9:15)]) %>%
  select(outcome, param, est) %>%
  spread(param, est) %>%
  mutate(across(.cols = c(I, S), ~format(round(., 2), nsmall = 2))) %>%
  mutate(blank = NA) %>%
  left_join(tx.only.results$Estimates, by = "outcome") %>%
  left_join(lgm.results$Estimates, by = "outcome") %>%
  select(outcome, Means_I:S.WITH_I, blank, Sources.I, I, Sources.S, S)


#######################################################

#### Exploring use of multilevel model ####
## Multilevel Model - https://www.statmodel.com/usersguide/chap9/ex9.12.html
ncps.ml.only.sntx <- mplusObject(
  rdata = mpluswide2,
  usevariables = c("ID", "School", "Tx", "SchN", paste0("NCPS_", 1:4)),
  TITLE = "NCPS - Multilevel Tx Only;",
  VARIABLE = "IDVARIABLE = ID;
  CLUSTER = School;
  BETWEEN = Tx SchN;",
  ANALYSIS = "TYPE = TWOLEVEL;",
  MODEL = "%WITHIN%
  iw sw | NCPS_1@0 NCPS_2@1 NCPS_3@2 NCPS_4@3;
  NCPS_1-NCPS_4 (1);
  %BETWEEN%
  ib sb | NCPS_1@0 NCPS_2@1 NCPS_3@2 NCPS_4@3;
  NCPS_1-NCPS_4@0;
  ib sb ON Tx SchN;",
  OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;")
# (1) constrains within (residual) variance to be equal at across time points
# between (residual) variance constrained to 0

ncps.only.run <- mplusModeler(ncps.ml.only.sntx,
                              dataout = "Mplus/Automatr/TX ONLY/MultL/NCPS_ML_TXO.dat",
                              modelout = "Mplus/Automatr/TX ONLY/MultL/NCPS_ML_TXO.inp",
                              run = 1, check = TRUE, hashfilename = TRUE,
                              writeData = "always")

# ------   Conditional Growth Models   -------------------

#### Creating Mplus Syntax ####

covs.sntx <- map2(.x = mplusoutcomes, .y = names(mplusoutcomes),
                        ~mplusObject(
                          rdata = mpluswide2,
                          usevariables = c("ID", "School", "Tx",
                                           "Female", "OtherG", "White", "POC", "LGBQ",
                                           "FGxTx", "OGxTx", "WRxTx", "POCxTx", "LGBQxTx",
                                           "SchN", "Rural", #"Teas_Per", "MHT_Per",
                                           paste0(.y, "_", 1:4)),
                          TITLE = glue::glue("{.x} - Tx with covariates;"),
                          VARIABLE = "IDVARIABLE = ID;
                          CLUSTER = School;",
                          ANALYSIS = "TYPE = COMPLEX;",
                          MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
                          i s ON Tx Female OtherG White POC LGBQ
                          FGxTx OGxTx WRxTx POCxTx LGBQxTx SchN Rural; 
                          [Tx Female OtherG White POC LGBQ
                          FGxTx OGxTx WRxTx POCxTx LGBQxTx SchN Rural];"), # Teas_Per MHT_Per;
                          OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

## binary outcome models
ta.covs.sntx <- map2(.x = bioutcomes, .y = names(bioutcomes),
                     ~mplusObject(
                       rdata = mpluswide2,
                       usevariables = c("ID", "School", "Tx",
                                        "Female", "OtherG", "White", "POC", "LGBQ",
                                        #"FGxTx", "OGxTx", "WRxTx", "POCxTx", "LGBQxTx",
                                        #"Rural", "Teas_Per", "MHT_Per",
                                        paste0(.y, "_", 1:4)),
                       TITLE = glue::glue("{.x} - TX with no interactions;"),
                       VARIABLE = glue::glue("CATEGORICAL = {.y}_1-{.y}_4 Tx Female OtherG White POC LGBQ;
              IDVARIABLE = ID;
              CLUSTER = School;"),
                       ANALYSIS = "TYPE = COMPLEX;
      ESTIMATOR = MLR;
      LINK = LOGIT;
      MITERATIONS = 1000; 
      PROCESSORS = 4;",
                       MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
         i s ON Tx Female OtherG White POC LGBQ;
         [Tx Female OtherG White POC LGBQ];
         [{.y}_1$1-{.y}_4$1@0]; 
         [i];"),
                       OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4 TECH10;"))
# MITERATIONS: the default 500 produced error that mconvergence criterion of the EM algorithm not fulfilled
# [TA_1$1-TA_4$1@0];  constrains threshold at each time to 0; this or intercept mean to 0 for identification (Lee et al., 2018)


####   Generate and Run Mplus Files   #####

covs.run <- map2(.x = covs.sntx, .y = names(covs.sntx),
                ~mplusModeler(.x,
                              dataout = glue::glue("Mplus/Automatr/TX COVS/{.y}_TX_COVS.dat"),
                              modelout = glue::glue("Mplus/Automatr/TX COVS/{.y}_TX_COVS.inp"),
                              run = 1, check = TRUE, hashfilename = TRUE,
                              writeData = "always"))

ta.covs.run <- map2(.x = ta.covs.sntx, .y = names(ta.covs.sntx),
                    ~mplusModeler(.x,
                           dataout = glue::glue("Mplus/Automatr/TX COVS/{.y}_TX_NOINT.dat"),
                           modelout = glue::glue("Mplus/Automatr/TX COVS/{.y}_TX_NOINT.inp"),
                           run = 1, check = TRUE, hashfilename = TRUE,
                           writeData = "always"))



#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later

tx.covs.preds <- c(I = "Intercept", S = "Intercept", TX = "Sources",
                   FEMALE = "Female", OTHERG = "Other Gender",
                   WHITE = "White", POC = "POC", LGBQ = "LGBQ",
                   FGXTX = "Female:Sources", OGXTX = "OG:Sources",
                   WRXTX = "White:Sources", POCXTX = "POC:Sources", LGBQXTX = "LGBQ:Sources",
                   SCHN = "School Size", RURAL = "Rural") #, TEAS_PER = "School Teasing", MHT_PER = "School MH Training")

## Importing models
tx.covs.mods <- MplusAutomation::readModels(target = "Mplus/Automatr/TX COVS")

## Model fit
# all models in single dataframe
tx.covs.fit <- map_dfr(tx.covs.mods[names(tx.covs.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_covs.out")],
                       ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_tx_covs.out", recodes = mplusoutcomes, tolower = TRUE)

# in a list for joining with estimates
tx.covs.fitlong <- map(tx.covs.mods[names(tx.covs.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_covs.out")],
                          ~mplus_fit(.x, digits = 2) %>%
                            mutate(x2_31 = case_when(pvalue < .01 ~ paste0(chisq, "**"),
                                                     pvalue < .05 ~ paste0(chisq, "*"),
                                                     TRUE ~ chisq),
                                   RMSEA_90CI = paste(rmsea, rmsea.ci)) %>%
                            select(x2_31, CFI = cfi, RMSEA_90CI, SRMR = srmr) %>%
                            gather(predictor, I))


## Fixed effect estimates
tx.covs.est <- map(.x = tx.covs.mods[names(tx.covs.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_covs.out")],
                      ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"), #"stdy.standardized"
                                 digits = 2, combine = TRUE, ci = FALSE) %>%
                        filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                        mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                               predictor = recode(param, !!!tx.covs.preds) %>%
                                 factor(., levels = unique(tx.covs.preds))) %>%
                        select(predictor, is, estimate) %>%
                        spread(is, estimate))

## Random effect estimates and R2
# Note: These are residual variances, not the random effect variance
# In other words, this is the remaining variance unexplained by the predictors
tx.covs.re <- map(.x = tx.covs.mods[names(tx.covs.mods) %in% paste0(tolower(names(mplusoutcomes)), "_tx_covs.out")],
                     ~mplus_est(.x, std = "unstandardized", params = "Residual.Variances",
                                digits = 2, combine = TRUE, ci = FALSE) %>%
                       bind_rows(.x$parameters$r2 %>%
                                   mutate(estimate = format(round(est, 2), nsmall = 2))) %>%
                       filter(param %in% c("I", "S")) %>%
                       mutate(predictor = c(rep("Residual Variance", 2), rep("R2", 2)) %>% as_factor()) %>%
                       select(predictor, param, estimate) %>%
                       spread(param, estimate))


## Binding all results into a single dataframe for each model
tx.covs.results <- map2(.x = tx.covs.est, .y = tx.covs.re,
                           ~bind_rows(.x, .y)) %>%
  map2(.x = ., .y = tx.covs.fitlong,
       ~bind_rows(.x, .y))


## Adjusting names and order of results
names(tx.covs.results) <- names(tx.covs.results) %>% str_remove(., "_tx.covs.out") %>%
  toupper() %>% recode(., !!!mplusoutcomes)
tx.covs.results <- tx.covs.results[mplusoutcomes]


#############################################################

#### Exploring use of multilevel model ####
## Multilevel Model - https://www.statmodel.com/usersguide/chap9/ex9.12.html
# crosslevel interactions go on %within%; see Mengting's question from http://www.statmodel.com/discussion/messages/12/15466.html?1542914221
ncps.ml.covs.sntx <- mplusObject(
  rdata = mpluswide2,
  usevariables = c("ID", "School", "Tx",
                   "Female", "OtherG", "White", "POC", "LGBQ",
                   "FGxTx", "OGxTx", "WRxTx", "POCxTx", "LGBQxTx",
                   "Rural", "Teas_Per", "MHT_Per",
                   paste0("NCPS_", 1:4)),
  TITLE = "NCPS - Multilevel Tx Only;",
  VARIABLE = "IDVARIABLE = ID;
  CLUSTER = School;
  WITHIN = Female OtherG White POC LGBQ
  FGxTx OGxTx WRxTx POCxTx LGBQxTx;
  BETWEEN = Tx Rural Teas_Per MHT_Per;",
  ANALYSIS = "TYPE = TWOLEVEL;",
  MODEL = "%WITHIN%
  iw sw | NCPS_1@0 NCPS_2@1 NCPS_3@2 NCPS_4@3;
  NCPS_1-NCPS_4 (1);
  iw sw ON Female OtherG White POC LGBQ
  FGxTx OGxTx WRxTx POCxTx LGBQxTx;
  [Female OtherG White POC LGBQ
  FGxTx OGxTx WRxTx POCxTx LGBQxTx];
  %BETWEEN%
  ib sb | NCPS_1@0 NCPS_2@1 NCPS_3@2 NCPS_4@3;
  NCPS_1-NCPS_4@0;
  ib sb ON Tx Rural Teas_Per MHT_Per;
  [Tx Rural Teas_Per MHT_Per];",
  OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;")
# (1) constrains within (residual) variance to be equal at across time points
# between (residual) variance constrained to 0

ncps.covs.run <- mplusModeler(ncps.ml.covs.sntx,
                              dataout = "Mplus/Automatr/TX COVS/MultL/NCPS_ML_TXC.dat",
                              modelout = "Mplus/Automatr/TX COVS/MultL/NCPS_ML_TXC.inp",
                              run = 1, check = TRUE, hashfilename = TRUE,
                              writeData = "always")



# -----    All binary Outcome Results    ------------

#### Trusted Adult ####
ta.results <- map_dfr(.x = list(lgm.mods$ta_lgm.out, tx.only.mods$ta_tx_only.out, tx.covs.mods$ta_tx_noint.out),
                      ~mplus_est(.x, std = "unstandardized", params = c("Means", "Variances", "ON", "Intercept")) %>%
                        filter(str_detect(paramHeader, "ON") | param %in% c("I", "S")) %>%
                        mutate(OR = format(round(exp(est), 2), nsmall = 2)) %>%
                        select(parameter, estimate, OR), .id = "Model") %>%
  separate(parameter, into = c("temp1", "temp2"), sep = "_") %>%
  mutate(temp1 = str_remove(temp1, ".ON"),
         is = ifelse(temp2 %in% c("I", "S"), temp2, temp1),
         parameter = ifelse(!(temp2 %in% c("I", "S")), temp2, temp1) %>%
           str_remove(., "Residual.") %>%
           str_replace(., "Means", "Intercepts")) %>%
  select(-temp1, -temp2) %>%
  gather(temp, value, estimate, OR) %>%
  unite(temp1, Model, is, temp) %>%
  spread(temp1, value) %>%
  mutate(parameter = recode(parameter, !!!tx.covs.preds) %>%
           factor(., levels = c("Intercepts", tx.covs.preds[3:8], "Variances"))) %>%
  arrange(parameter)

#### Suicide Ideation ####
# Models give extremely low intercept estimates with huge residual variances
# si.results <- map_dfr(.x = list(lgm.mods$si_lgm.out, tx.only.mods$si_tx_only.out),
#                       ~mplus_est(.x, std = "unstandardized", params = c("Means", "Variances", "ON", "Intercept")) %>%
#                         filter(str_detect(paramHeader, "ON") | param %in% c("I", "S")) %>%
#                         mutate(OR = format(round(exp(est), 2), nsmall = 2)) %>%
#                         select(parameter, estimate, OR), .id = "Model") %>%
#   separate(parameter, into = c("temp1", "temp2"), sep = "_") %>%
#   mutate(temp1 = str_remove(temp1, ".ON"),
#          is = ifelse(temp2 %in% c("I", "S"), temp2, temp1),
#          parameter = ifelse(!(temp2 %in% c("I", "S")), temp2, temp1) %>%
#            str_remove(., "Residual.") %>%
#            str_replace(., "Means", "Intercepts")) %>%
#   select(-temp1, -temp2) %>%
#   gather(temp, value, estimate, OR) %>%
#   unite(temp1, Model, is, temp) %>%
#   spread(temp1, value) %>%
#   mutate(parameter = recode(parameter, !!!tx.covs.preds) %>%
#            factor(., levels = c("Intercepts", tx.covs.preds[3:8], "Variances"))) %>%
#   arrange(parameter)
# 
# 
# mplus_est(object = tx.covs.mods$si_tx_noint.out, std = "unstandardized", params = c("Means", "Variances", "ON", "Intercept"))
# 
# params <- ifelse(str_detect(params, "\\|"), "[|]", params)
# params = paste(params, collapse = "|")
# 
# est <- object$parameters[[std]] %>%
#   filter(str_detect(paramHeader, params))
# 
# 
#   est <- est %>%
#     mutate(parameter = paste(paramHeader, param, sep = "_"),
#            estimate = paste0(format(round(est, digits), nsmall = digits),
#                              " (", format(round(se, digits), nsmall = digits), ")"),
#            estimate = case_when(pval < .01 ~ paste0(estimate, "**"),
#                                 pval < alpha ~ paste0(estimate, "*"),
#                                 TRUE ~ estimate))
############################################################

## If interaction plots are needed, see 4a-ReadMplus.R for code

####################################################
####       Wave 1 Only sensitivity analysis     ####

#  -------  Latent Growth Models  ---------------------

#### Creating Mplus Syntax ####

lgm.sntx1 <- map2(.x = mplusoutcomes, .y = names(mplusoutcomes),
                 ~mplusObject(
                   rdata = mpluswide2[mpluswide2$srvy_1 == 1, ],
                   usevariables = c("ID", "School", paste0(.y, "_", 1:4)),
                   TITLE = glue::glue("{.x} - LGM;"),
                   VARIABLE ="IDVARIABLE = ID;
                                    CLUSTER = School;",
                   ANALYSIS = "TYPE = COMPLEX;",
                   MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;"),
                   OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

####   Generate and Run Mplus Files   #####
# Note on writeData = "always" - The md5 hash is based on: (1) the dimensions of the dataset, (2) the variable names, (3) the class
# of every variable, and (4) the raw data from the first and last rows. This combination ensures that
# under most all circumstances, if the data changes, the hash will change

lgm.run1 <- map2(.x = lgm.sntx1, .y = names(lgm.sntx1),
                ~mplusModeler(.x,
                              dataout = glue::glue("Mplus/Wave 1 Sample/LGM/{.y}_W1LGM.dat"),
                              modelout = glue::glue("Mplus/Wave 1 Sample/LGM/{.y}_W1LGM.inp"),
                              run = 1, check = TRUE, hashfilename = TRUE,
                              writeData = "always"))


#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later
# lgm.preds <- c(I = "Intercept", S = "Intercept")

## Importing models and converting results to tables
lgm.mods1 <- MplusAutomation::readModels(target = "Mplus/Wave 1 Sample/LGM")
lgm.fit1 <- map_dfr(lgm.mods1[1:drop.ta-1], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_w1lgm.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE)
# lgm.fit will go into supplemental materials


lgm.est1 <- map_dfr(lgm.mods1[1:drop.ta-1], ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized"), .id = "outcome") %>%
  outcome_shortcut(remove = "_w1lgm.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(outcome, Means_I, Variances_I, Means_S, Variances_S, S.WITH_I)

####   Compiling results objects into one object    ####
lgm.results1 <- list(Models = lgm.mods1[1:drop.ta-1],
                    Fit = lgm.fit1,
                    Estimates = lgm.est1,
                    R2 = map_dfr(.x = lgm.mods1[1:drop.ta-1], ~.x$parameters$r2, .id = "outcome") %>%
                      outcome_shortcut(remove = "_w1lgm.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE),
                    Warn_Error = map_dfr(.x = lgm.mods1[1:drop.ta-1],
                                         ~data.frame(errors = length(.x$errors),
                                                     warnings = length(.x$warnings)),
                                         .id = "outcome") %>%
                      outcome_shortcut(remove = "_w1lgm.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE))


# ------   Tx Only Growth Models    ------------

#### Creating Mplus Syntax ####

only.sntx1 <- map2(.x = mplusoutcomes[-drop.ta], .y = names(mplusoutcomes)[-drop.ta], # exclude TA, which has a separate model
                  ~mplusObject(
                    rdata = mpluswide2[mpluswide2$srvy_1 == 1, ],
                    usevariables = c("ID", "School", "Tx", paste0(.y, "_", 1:4)),
                    TITLE = glue::glue("{.x} - Tx only;"),
                    VARIABLE = "IDVARIABLE = ID;
                          CLUSTER = School;",
                    ANALYSIS = "TYPE = COMPLEX;",
                    MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
                          i s ON Tx;
                          [Tx];"),
                    OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

####   Generate and Run Mplus Files   #####

only.run1 <- map2(.x = only.sntx1, .y = names(only.sntx1),
                 ~mplusModeler(.x,
                               dataout = glue::glue("Mplus/Wave 1 Sample/TX ONLY/{.y}_W1TX.dat"),
                               modelout = glue::glue("Mplus/Wave 1 Sample/TX ONLY/{.y}_W1TX.inp"),
                               run = 1, check = TRUE, hashfilename = TRUE,
                               writeData = "always"))



#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later
tx.only.preds <- c(I = "Intercept", S = "Intercept", TX = "Sources")

## Importing models
tx.only.mods1 <- MplusAutomation::readModels(target = "Mplus/Wave 1 Sample/TX ONLY")

## Model Fit
tx.only.fit1 <- map_dfr(tx.only.mods1[1:drop.ta-1], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_w1tx.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE)

## Fixed effect estimates
tx.only.est1 <- map_dfr(.x = tx.only.mods1[1:drop.ta-1],
                       ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"),
                                  digits = 2, combine = TRUE, ci = FALSE) %>%
                         filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                         mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                                predictor = recode(param, !!!tx.only.preds) %>%
                                  factor(., levels = unique(tx.only.preds)),
                                temp = paste(predictor, is, sep = ".")) %>%  # specific to latent growth models where I and S recoded to same value
                         select(temp, estimate) %>%
                         spread(temp, estimate), .id = "outcome") %>%
  outcome_shortcut(remove = "_w1tx.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE) %>%
  select(outcome, Sources.I, Sources.S)

####   Compiling results objects into one object    ####
tx.only.results1 <- list(Models = tx.only.mods1[1:drop.ta-1],
                        Fit = tx.only.fit1,
                        Estimates = tx.only.est1,
                        R2 = map_dfr(.x = tx.only.mods1[1:drop.ta-1], ~.x$parameters$r2, .id = "outcome") %>%
                          outcome_shortcut(remove = "_w1tx.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE),
                        Warn_Error = map_dfr(.x = tx.only.mods1[1:drop.ta-1],
                                             ~data.frame(errors = length(.x$errors),
                                                         warnings = length(.x$warnings)),
                                             .id = "outcome") %>%
                          outcome_shortcut(remove = "_w1tx.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE))

#### Extracting Results for publication ####
# Note. Unconditional and Tx only models are combined for output

## Model fit - formatting will occur in 5bc-OutcomePaper.Rmd
unctx.fit1 <- select(lgm.results1$Fit, -npar) %>% filter(outcome %in% mplusoutcomes[c(1:6, 8)]) %>%
  left_join(select(tx.only.results1$Fit, -npar), by = "outcome") # don't need to filter here since we are using left_join

## Estimates
unctx.est1 <- tx.only.results1$R2 %>%
  filter(param %in% c("I", "S") & outcome %in% mplusoutcomes[c(1:6, 8)]) %>%
  select(outcome, param, est) %>%
  spread(param, est) %>%
  mutate(across(.cols = c(I, S), ~format(round(., 2), nsmall = 2))) %>%
  mutate(blank = NA) %>%
  left_join(tx.only.results1$Estimates, by = "outcome") %>%
  left_join(lgm.results1$Estimates, by = "outcome") %>%
  select(outcome, Means_I:S.WITH_I, blank, Sources.I, I, Sources.S, S)


#######################################################

# ------   Conditional Growth Models   -------------------

#### Creating Mplus Syntax ####

covs.sntx1 <- map2(.x = mplusoutcomes[-15], .y = names(mplusoutcomes)[-15], # drop TA, which has a separate model
                  ~mplusObject(
                    rdata = mpluswide2[mpluswide2$srvy_1 == 1, ],
                    usevariables = c("ID", "School", "Tx",
                                     "Female", "OtherG", "White", "POC", "LGBQ",
                                     "FGxTx", "OGxTx", "WRxTx", "POCxTx", "LGBQxTx",
                                     "SchN", "Rural", #"Teas_Per", "MHT_Per",
                                     paste0(.y, "_", 1:4)),
                    TITLE = glue::glue("{.x} - Tx with covariates;"),
                    VARIABLE = "IDVARIABLE = ID;
                          CLUSTER = School;",
                    ANALYSIS = "TYPE = COMPLEX;",
                    MODEL = glue::glue("i s | {.y}_1@0 {.y}_2@1 {.y}_3@2 {.y}_4@3;
                          i s ON Tx Female OtherG White POC LGBQ
                          FGxTx OGxTx WRxTx POCxTx LGBQxTx SchN Rural; 
                          [Tx Female OtherG White POC LGBQ
                          FGxTx OGxTx WRxTx POCxTx LGBQxTx SchN Rural];"), # Teas_Per MHT_Per;
                    OUTPUT = "STANDARDIZED CINTERVAL RESIDUAL TECH1 TECH4;"))

####   Generate and Run Mplus Files   #####

covs.run1 <- map2(.x = covs.sntx1, .y = names(covs.sntx1),
                 ~mplusModeler(.x,
                               dataout = glue::glue("Mplus/Wave 1 Sample/TX COVS/{.y}_W1COVS.dat"),
                               modelout = glue::glue("Mplus/Wave 1 Sample/TX COVS/{.y}_W1COVS.inp"),
                               run = 1, check = TRUE, hashfilename = TRUE,
                               writeData = "always"))

#####   Extract Results from Mplus files   ####
# Note: all Trusted Adult Results will be gathered separately later

tx.covs.preds <- c(I = "Intercept", S = "Intercept", TX = "Sources",
                   FEMALE = "Female", OTHERG = "Other Gender",
                   WHITE = "White", POC = "POC", LGBQ = "LGBQ",
                   FGXTX = "Female:Sources", OGXTX = "OG:Sources",
                   WRXTX = "White:Sources", POCXTX = "POC:Sources", LGBQXTX = "LGBQ:Sources",
                   SCHN = "School Size", RURAL = "Rural") #, TEAS_PER = "School Teasing", MHT_PER = "School MH Training")

## Importing models
tx.covs.mods1 <- MplusAutomation::readModels(target = "Mplus/Wave 1 Sample/TX COVS")

## Model fit
# all models in single dataframe
tx.covs.fit1 <- map_dfr(tx.covs.mods1[1:drop.ta-1], ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  outcome_shortcut(remove = "_w1covs.out", recodes = mplusoutcomes[1:drop.ta-1], tolower = TRUE)

# in a list for joining with estimates
tx.covs.fitlong1 <- map(tx.covs.mods1[1:drop.ta-1],
                       ~mplus_fit(.x, digits = 2) %>%
                         mutate(x2_31 = case_when(pvalue < .01 ~ paste0(chisq, "**"),
                                                  pvalue < .05 ~ paste0(chisq, "*"),
                                                  TRUE ~ chisq),
                                RMSEA_90CI = paste(rmsea, rmsea.ci)) %>%
                         select(x2_31, CFI = cfi, RMSEA_90CI, SRMR = srmr) %>%
                         gather(predictor, I))


## Fixed effect estimates
tx.covs.est1 <- map(.x = tx.covs.mods1[1:drop.ta-1],
                   ~mplus_est(.x, std = "stdy.standardized", params = c("ON", "Intercepts"), #"stdy.standardized"
                              digits = 2, combine = TRUE, ci = FALSE) %>%
                     filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                     mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                            predictor = recode(param, !!!tx.covs.preds) %>%
                              factor(., levels = unique(tx.covs.preds))) %>%
                     select(predictor, is, estimate) %>%
                     spread(is, estimate))

## Random effect estimates and R2
# Note: These are residual variances, not the random effect variance
# In other words, this is the remaining variance unexplained by the predictors
tx.covs.re1 <- map(.x = tx.covs.mods1[1:drop.ta-1],
                  ~mplus_est(.x, std = "unstandardized", params = "Residual.Variances",
                             digits = 2, combine = TRUE, ci = FALSE) %>%
                    bind_rows(.x$parameters$r2 %>%
                                mutate(estimate = format(round(est, 2), nsmall = 2))) %>%
                    filter(param %in% c("I", "S")) %>%
                    mutate(predictor = c(rep("Residual Variance", 2), rep("R2", 2)) %>% as_factor()) %>%
                    select(predictor, param, estimate) %>%
                    spread(param, estimate))


## Binding all results into a single dataframe for each model
tx.covs.results1 <- map2(.x = tx.covs.est1, .y = tx.covs.re1,
                        ~bind_rows(.x, .y)) %>%
  map2(.x = ., .y = tx.covs.fitlong1,
       ~bind_rows(.x, .y))


## Adjusting names and order of results
names(tx.covs.results1) <- names(tx.covs.results1) %>% str_remove(., "_w1covs.out") %>%
  toupper() %>% recode(., !!!mplusoutcomes[1:drop.ta-1])
tx.covs.results1 <- tx.covs.results1[mplusoutcomes[1:drop.ta-1]]

# ----- Saving Latent Growth Model Objects -----------------
save(mplusoutcomes, lgm.mods, lgm.results, ml.lgm.results,
     quad.mods, quad.fit, lq.comp,
     tx.only.results, unctx.fit, unctx.est, nopubtx.fit, nopubtx.est,
     tx.covs.mods, tx.covs.fit, tx.covs.est, tx.covs.re, tx.covs.fitlong,
     tx.covs.results, ta.results,
     lgm.mods1, lgm.results1,
     tx.only.results1, unctx.fit1, unctx.est1,
     tx.covs.mods1, tx.covs.fit1, tx.covs.est1,
     tx.covs.re1, tx.covs.fitlong1, tx.covs.results1,
     # intx.data, intx.plot, onperp.lm, onperp.mlm,
     # params.lm, perform.lm, mcplots.lm,
     # params.mlm, perform.mlm, mcplots.mlm,
     # ncp.int.plot, cp.int.plot,
     file = "Output/Mplus_Automatr_Results.RData")

load("Output/Mplus_Automatr_Results.RData")
