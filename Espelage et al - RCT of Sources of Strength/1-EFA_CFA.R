########################################
#                                      #
#         Sources of Strength          #
#          Scale Development           #
#         EFA/CFA/Reliability          #
#                                      #
########################################

## Notes:
# These analyses were run prior to full development of the kfa package with results documented in "SoS_Scale_Psychometric_Report_04202021.docx"
# These analyses were also run using the dscombo data prior to the minor fixes that were rectified in the dsclean data
# kfa script is included starting in line 954 using the dsclean data and runs as of 5/18/2022

## Loading packages, functions, and data
source("Scripts/PnF.R")
load("Output/SoS_Data.RData")

##########################################################
####  Exploratory and Confirmatory Factor Analysis   #####

# ---- Samples for Factor Exploration ------------

## creating efa and cfa samples by randomly selecting rows in wide format data
set.seed(191591)

#### Primary Outcomes and Protective Factors ####

## wide format
# EFA
efa.samplew <- ds2 %>%
  group_by(School) %>%
  sample_frac(size = .5, replace = FALSE)
# CFA
cfa.samplew <- ds2[!(ds2$StudentID %in% efa.samplew$StudentID), ]

# long format
efa.samplel <- ds2long[ds2long$StudentID %in% efa.samplew$StudentID, ]
cfa.samplel <- ds2long[ds2long$StudentID %in% cfa.samplew$StudentID, ]

#### Exposure ####

## wide
efa.sampwexp <- ds2 %>%
  filter(Tx == 1) %>%
  group_by(School) %>%
  sample_frac(size = .5, replace = FALSE)
cfa.sampwexp <- ds2 %>%
  filter(Tx == 1 & !(StudentID %in% efa.sampwexp$StudentID))

## long
efa.samplexp <- ds2long[ds2long$StudentID %in% efa.sampwexp$StudentID, ]
cfa.samplexp <- ds2long[ds2long$StudentID %in% cfa.sampwexp$StudentID, ]


########################################################

######################################
####     Exposure Variable(s)     ####

## NOTES:
# - Using Wave 1 only for EFAs (W2 for Exposure); CFAs fit separately on each wave
# - All items are ordinal, so the polychoric correlations are used
# - Accounting for nesting structure of schools using cluster argument in lavaan

# ------- Exposure ------------------------------

## Sample, polychoric correlations, consensus number of factors
exp <- efa_cfa_prelim(efa.samplexp, ExposureVars, Wave = 2)
summary(exp$nf)

#### Running and comparing EFA models ####
exp.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = exp$sample,
                                     varList = ExposureVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = ExposureVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  exp.efa[[nf]] <- unrotated
}

lavTestLRT(exp.efa[[1]], exp.efa[[2]]) # 2 factor fits better

#### Extracting Factor Loadings ####
## Rotating 2 factor model loadings
exp.rot.f2 <- get_std_loadings(exp.efa[[2]], type = "std.all") %>%
  GPArotation::GPFoblq(method = "oblimin")
# Loadings show Item 7 loading on its own

exp.efa.loadings <- list(get_std_loadings(exp.efa[[1]]),
                         exp.rot.f2$loadings)

## converting EFA results to CFA syntax
exp.syntax <- lapply(exp.efa.loadings, efa_cfa_syntax, single.item = 'drop')

#### Running and comparing CFA models ####

exp.cfa1 <- map(.x = 2:4, ~cfa(mod = exp.syntax[[1]],
                               data = cfa.samplexp[cfa.samplexp$Wave == .x, ],
                               ordered = ExposureVars,
                               cluster = "School",
                               missing = "pairwise")) %>%
  set_names(c("2", "3", "4")) %>%
  cfa_wrapper(mods = .)

exp.cfa2 <- map(.x = 2:4, ~cfa(mod = exp.syntax[[2]],
                               data = cfa.samplexp[cfa.samplexp$Wave == .x, ],
                               ordered = ExposureVars,
                               cluster = "School",
                               missing = "pairwise")) %>%
  set_names(c("2", "3", "4")) %>%
  cfa_wrapper(mods = .)


## CONCLUSION: 2 factor model fits much better

## Can active and passive stand on their own?
# syntax
active.syntax <- paste0("Active =~ ", paste(ActiveExpVars, collapse = " + "))
passive.syntax <- paste0("Passive =~ ", paste(PassiveExpVars, collapse = " + "))
passiveno4.syntax <- paste0("Passive =~ ", paste(PassiveExpVars[-4], collapse = " + "))
passiveno3.syntax <- paste0("Passive =~ ", paste(PassiveExpVars[-3], collapse = " + "))# passive.syntax,"\nEXPOSURE_STRENGTHS_RESILIENCE ~~  EXPOSURE_HELP_SUICIDAL_TEENS")

exp.cors <- map(.x = 2:4,
                ~lavCor(object = cfa.samplexp[cfa.samplexp$Wave == .x, ExposureVars],
                        ordered = ExposureVars,
                        output = "cor",
                        estimator = "WLSMV",
                        missing = "pairwise",
                        cor.smooth = FALSE,
                        cluster = "School")) %>%
  set_names(c("2", "3", "4"))

lapply(exp.cors, round, 2)
                

# cfas
exp.active <- map(.x = 2:4, ~cfa(mod = active.syntax,
                                 data = cfa.samplexp[cfa.samplexp$Wave == .x, ],
                                 ordered = ExposureVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  set_names(c("2", "3", "4")) %>%
  cfa_wrapper(mods = .)

# all items
exp.passive <- map(.x = 2:4, ~cfa(mod = passive.syntax,
                                 data = cfa.samplexp[cfa.samplexp$Wave == .x, ],
                                 ordered = ExposureVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  set_names(c("2", "3", "4")) %>%
  cfa_wrapper(mods = .)

# excluding item 3 - Resilience
exp.passiveno3 <- map(.x = 2:4, ~cfa(mod = passiveno3.syntax,
                                     data = cfa.samplexp[cfa.samplexp$Wave == .x, ],
                                     ordered = ExposureVars,
                                     cluster = "School",
                                     missing = "pairwise")) %>%
  set_names(c("2", "3", "4")) %>%
  cfa_wrapper(mods = .)

# excluding item 4 - Suicidal teens
exp.passiveno4 <- map(.x = 2:4, ~cfa(mod = passiveno4.syntax,
                                  data = cfa.samplexp[cfa.samplexp$Wave == .x, ],
                                  ordered = ExposureVars,
                                  cluster = "School",
                                  missing = "pairwise")) %>%
  set_names(c("2", "3", "4")) %>%
  cfa_wrapper(mods = .)


## CONCLUSION: The 1 factor is poor
# The 2 factor model has adequate fit (rmsea and srmr a bit high); ACTIVITY_PARTICIPATION loads on passive factor for some reason
# Active can stand on its own with or without including ACTIVITY_PARTICIPATION - keep it in for completeness
# Passive has high rmsea and srmr with all items (with or without ACTIVITY_PARTICIPATION); residual correlation b/t RESILIENCE and SUICIDE TEENS
#   removing RESILIENCE or SUICIDE TEENS improves fit quite a bit; keeping RESILIENCE as fit, reliability, and loadings are a bit better than SUICIDE TEENS


#### compiling results into a single object ####
exp.results <- list(prelim = exp,
                    efa.loadings = exp.efa.loadings,
                    syntax = exp.syntax,
                    cfas = list(One_Factor = exp.cfa1, Two_Factor = exp.cfa2,
                                Active = exp.active, Passive_All = exp.passive,
                                Passive_No3 = exp.passiveno3, Passive_No4 = exp.passiveno4))

############################################################

#####################################
####      Primary Outcomes       ####

# ------- Sexual Harassment (No Contact) Perpetration -------

## Sample, polychoric correlations, consensus number of factors
ncperp <- efa_cfa_prelim(efa.samplel, NoContactPerpVars)

#### Running 1 factor CFA ####
ncperp.syntax <- paste0("factor1 =~ ", paste(NoContactPerpVars, collapse = " + "))

ncperp.cfa <- map(.x = 1:4, ~cfa(mod = ncperp.syntax,
                                       data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                       ordered = NoContactPerpVars,
                                       cluster = "School",
                                       missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: heywood case for item 3 in W3; otherwise seems alright

#### compiling results into a single object ####
ncperp.results <- list(prelim = ncperp,
                             efa.loadings = NULL,
                             syntax = ncperp.syntax,
                             cfas = ncperp.cfa)

########################################################

# ------- Sexual Harassment (No Contact) Victimization -------

## Sample, polychoric correlations, consensus number of factors
ncvict <- efa_cfa_prelim(efa.samplel, NoContactVictVars)

#### Running 1 factor CFA ####
ncvict.syntax <- paste0("factor1 =~ ", paste(NoContactVictVars, collapse = " + "))

ncvict.cfa <- map(.x = 1:4, ~cfa(mod = ncvict.syntax,
                                 data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                 ordered = NoContactVictVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Looks good

#### compiling results into a single object ####
ncvict.results <- list(prelim = ncvict,
                       efa.loadings = NULL,
                       syntax = ncvict.syntax,
                       cfas = ncvict.cfa)

########################################################

# ------- Sexual Violence (Contact) Perpetration -------

## Sample, polychoric correlations, consensus number of factors
conperp <- efa_cfa_prelim(efa.samplel, ContactPerpVars)

#### Running 1 factor CFA ####
conperp.syntax <- paste0("factor1 =~ ", paste(ContactPerpVars, collapse = " + "))

conperp.cfa <- map(.x = 1:4, ~cfa(mod = conperp.syntax,
                                 data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                 ordered = ContactPerpVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: some high correlations b/t various items at each wave; otherwise seems alright

#### compiling results into a single object ####
conperp.results <- list(prelim = conperp,
                       efa.loadings = NULL,
                       syntax = conperp.syntax,
                       cfas = conperp.cfa)

########################################################


# ------- Sexual Violence (Contact) Victimization -------

## Sample, polychoric correlations, consensus number of factors
convict <- efa_cfa_prelim(efa.samplel, ContactVictVars)

#### Running 1 factor CFA ####
convict.syntax <- paste0("factor1 =~ ", paste(ContactVictVars, collapse = " + "))

convict.cfa <- map(.x = 1:4, ~cfa(mod = convict.syntax,
                                 data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                 ordered = ContactVictVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Looks good

#### compiling results into a single object ####
convict.results <- list(prelim = convict,
                       efa.loadings = NULL,
                       syntax = convict.syntax,
                       cfas = convict.cfa)

########################################################

# ------- Homophobic Name Calling Perpetration -------

## Sample, polychoric correlations, consensus number of factors
hncperp <- efa_cfa_prelim(efa.samplel, HNCPerpVars)

#### Running 1 factor CFA ####
hncperp.syntax <- paste0("factor1 =~ ", paste(HNCPerpVars, collapse = " + "))

hncperp.cfa <- map(.x = 1:4, ~cfa(mod = hncperp.syntax,
                                 data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                 ordered = HNCPerpVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Looks good

#### compiling results into a single object ####
hncperp.results <- list(prelim = hncperp,
                       efa.loadings = NULL,
                       syntax = hncperp.syntax,
                       cfas = hncperp.cfa)


########################################################

# ------- Homophobic Name Calling Victimization -------

## Sample, polychoric correlations, consensus number of factors
hncvict <- efa_cfa_prelim(efa.samplel, HNCVictVars)

#### Running 1 factor CFA ####
hncvict.syntax <- paste0("factor1 =~ ", paste(HNCVictVars, collapse = " + "))

hncvict.cfa <- map(.x = 1:4, ~cfa(mod = hncvict.syntax,
                                 data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                 ordered = HNCVictVars,
                                 cluster = "School",
                                 missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Looks good

#### compiling results into a single object ####
hncvict.results <- list(prelim = hncvict,
                       efa.loadings = NULL,
                       syntax = hncvict.syntax,
                       cfas = hncvict.cfa)


########################################################


# ------- Cybersex Perpetration -------

## Sample, polychoric correlations, consensus number of factors
cyber <- efa_cfa_prelim(efa.samplel, CyberVars)

#### Running 1 factor CFA ####
cyber.syntax <- paste0("factor1 =~ ", paste(CyberVars, collapse = " + "))

cyber.cfa <- map(.x = 1:4, ~cfa(mod = cyber.syntax,
                                  data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                  ordered = CyberVars,
                                  cluster = "School",
                                  missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Looks good

#### compiling results into a single object ####
cyber.results <- list(prelim = cyber,
                        efa.loadings = NULL,
                        syntax = cyber.syntax,
                        cfas = cyber.cfa)

########################################################


# ------- Sexual Violence Dismissiveness -------

## Sample, polychoric correlations, consensus number of factors
dismiss <- efa_cfa_prelim(efa.samplel, DismisssvVars)

#### Running 1 factor CFA ####
dismiss.syntax <- paste0("factor1 =~ ", paste(DismisssvVars, collapse = " + "))

dismiss.cfa <- map(.x = 1:4, ~cfa(mod = dismiss.syntax,
                                data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                ordered = DismisssvVars,
                                cluster = "School",
                                missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Looks good

#### compiling results into a single object ####
dismiss.results <- list(prelim = dismiss,
                      efa.loadings = NULL,
                      syntax = dismiss.syntax,
                      cfas = dismiss.cfa)

#############################################################


#####################################
####      Protective Factors     ####

# ------- General Well Being ------------------------------

## Sample, polychoric correlations, consensus number of factors
gwb <- efa_cfa_prelim(efa.samplel, WellBeingVars)

#### Running and comparing EFA models ####
gwb.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = gwb$sample,
                                     varList = WellBeingVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = WellBeingVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  gwb.efa[[nf]] <- unrotated
}

lavTestLRT(gwb.efa[[1]], gwb.efa[[2]]) # 2 factor fits better

#### Extracting Factor Loadings ####
## Rotating 2 factor model loadings
gwb.rot.f2 <- get_std_loadings(gwb.efa[[2]], type = "std.all") %>%
  GPArotation::GPFoblq(method = "oblimin")
# Loadings show Item 7 loading on its own

gwb.efa.loadings <- list(get_std_loadings(gwb.efa[[1]]),
                         gwb.rot.f2$loadings)

## converting EFA results to CFA syntax
gwb.syntax <- lapply(gwb.efa.loadings, efa_cfa_syntax, single.item = 'drop')

#### Running and comparing CFA models ####

gwb.cfa1 <- map(.x = 1:4, ~cfa(mod = gwb.syntax[[1]],
                              data = cfa.samplel[cfa.samplel$Wave == .x, ],
                              ordered = WellBeingVars,
                              cluster = "School",
                              missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

gwb.cfa2 <- map(.x = 1:4, ~cfa(mod = gwb.syntax[[2]],
                               data = cfa.samplel[cfa.samplel$Wave == .x, ],
                               ordered = WellBeingVars,
                               cluster = "School",
                               missing = "pairwise")) %>%
  cfa_wrapper(mods = .)


## CONCLUSION: Model fit improves and reliability drops slightly  when item 7 is removed
# Item 7 has high loadings across waves, so keeping it

#### compiling results into a single object ####
gwb.results <- list(prelim = gwb,
                    efa.loadings = gwb.efa.loadings,
                    syntax = gwb.syntax,
                    cfas = list(One_Factor = gwb.cfa1,
                                Two_Factor = gwb.cfa2)) # i.e., removing item 7

##########################################################

# ------- SH Help Seeking ------------------------------
## NOTE: Earlier runs revealed these items distinct from the other help seeking items

## Sample, polychoric correlations, consensus number of factors
shhelpseek <- efa_cfa_prelim(efa.samplel, SHHelpIntentVars)

#### Running 1 factor CFA ####
shhelpseek.syntax <- paste0("factor1 =~ ", paste(SHHelpIntentVars, collapse = " + "))

shhelpseek.cfa <- map(.x = 1:4, ~cfa(mod = shhelpseek.syntax,
                               data = cfa.samplel[cfa.samplel$Wave == .x, ],
                               ordered = SHHelpIntentVars,
                               cluster = "School",
                               missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: RMSEA a bit high in Wave 2, but otherwise acceptable

#### compiling results into a single object ####
shhelpseek.results <- list(prelim = shhelpseek,
                    efa.loadings = NULL,
                    syntax = shhelpseek.syntax,
                    cfas = shhelpseek.cfa)


#############################################################

# -------    SH Help Attitudes      ---------------------
## NOTE: Earlier runs revealed these items distinct from the other help seeking items

## Sample, polychoric correlations, consensus number of factors
shhelpatt <- efa_cfa_prelim(efa.samplel, SHHelpAttitudeVars)

#### Running 1 factor CFA ####
shhelpatt.syntax <- paste0("factor1 =~ ", paste(SHHelpAttitudeVars, collapse = " + "))

shhelpatt.cfa <- map(.x = 1:4, ~cfa(mod = shhelpatt.syntax,
                                     data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                     ordered = SHHelpAttitudeVars,
                                     cluster = "School",
                                     missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: RMSEA a bit high, but otherwise acceptable

#### compiling results into a single object ####
shhelpatt.results <- list(prelim = shhelpatt,
                           efa.loadings = NULL,
                           syntax = shhelpatt.syntax,
                           cfas = shhelpatt.cfa)

########################################################


# ------- General Help Attitudes ------------------------------

## Sample, polychoric correlations, consensus number of factors
gha <- efa_cfa_prelim(efa.samplel, HelpAttVars)


#### Running and comparing EFA models ####
gha.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = gha$sample,
                                     varList = HelpAttVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = HelpAttVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  gha.efa[[nf]] <- unrotated
}

lavTestLRT(gha.efa[[1]], gha.efa[[2]]) # 2 factor fits better

#### Extracting Factor Loadings ####
## Rotating 2+ factor model loadings
gha.rot <-get_std_loadings(gha.efa[[2]], type = "std.all") %>%
                 GPArotation::GPFoblq(method = "oblimin")

gha.efa.loadings <- list(get_std_loadings(gha.efa[[1]]), gha.rot$loadings)

## converting EFA results to CFA syntax
gha.syntax <- lapply(gha.efa.loadings, efa_cfa_syntax, single.item = 'drop')

# cat(ghs.syntax[[2]])


#### Running and comparing CFA models ####
# 1 factor
gha.cfa1 <- map(.x = 1:4, ~cfa(mod = gha.syntax[[1]],
                               data = cfa.samplel[cfa.samplel$Wave == .x, ],
                               ordered = HelpAttVars,
                               cluster = "School",
                               missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

gha.cfa2 <- map(.x = 1:4, ~cfa(mod = gha.syntax[[2]],
                               data = cfa.samplel[cfa.samplel$Wave == .x, ],
                               ordered = HelpAttVars,
                               cluster = "School",
                               missing = "pairwise")) %>%
  cfa_wrapper(mods = .)


## CONCLUSION: Not clear whether 1 or 2 factor model is more appropriate

#### compiling results into a single object ####
gha.results <- list(prelim = gha,
                    efa.loadings = gha.efa.loadings,
                    syntax = gha.syntax,
                    cfas = list(One_Factor = gha.cfa1,
                                Two_Factor = gha.cfa2))

##############################################

# ------- Helping Others -----------------------

## Sample, polychoric correlations, consensus number of factors
hothers <- efa_cfa_prelim(efa.samplel, HelpOthersVars)

#### Running and comparing EFA models ####
hothers.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = hothers$sample,
                                     varList = HelpOthersVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = HelpOthersVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  hothers.efa[[nf]] <- unrotated
}

lavTestLRT(hothers.efa[[1]], hothers.efa[[2]]) # 2 factor fits better

#### Extracting Factor Loadings ####
## Rotating 2 factor model loadings
hothers.rot.f2 <- get_std_loadings(hothers.efa[[2]], type = "std.all") %>%
  GPArotation::GPFoblq(method = "oblimin")

hothers.efa.loadings <- list(get_std_loadings(hothers.efa[[1]]),
                             hothers.rot.f2$loadings)

## converting EFA results to CFA syntax
hothers.syntax <- c(lapply(hothers.efa.loadings, efa_cfa_syntax, single.item = 'drop'),
                    list( paste0("factor1 =~ ", paste(paste0("INTENT_HELP_OTHERS_", c(1,2,4,5)),collapse = " + "))))

#### Running and comparing CFA models ####
hothers.cfa1 <- map(.x = 1:4, ~cfa(mod = hothers.syntax[[1]],
                                   data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                   ordered = HelpOthersVars,
                                   cluster = "School",
                                   missing = "pairwise")) %>%
  cfa_wrapper(mods = .)
hothers.cfa2 <- map(.x = 1:4, ~cfa(mod = hothers.syntax[[2]],
                                   data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                   ordered = HelpOthersVars,
                                   cluster = "School",
                                   missing = "pairwise")) %>%
  cfa_wrapper(mods = .)
hothers.cfa1adj <- map(.x = 1:4, ~cfa(mod = hothers.syntax[[3]],
                                   data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                   ordered =  HelpOthersVars,
                                   cluster = "School",
                                   missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: RMSEA way too high on all models (1 factor, 2 factor, custom 1 factor) at all waves

#### compiling results into a single object ####
hothers.results <- list(prelim = hothers,
                    efa.loadings = hothers.efa.loadings,
                    syntax = hothers.syntax,
                    cfas = list(One_Factor = hothers.cfa1,
                                Two_Factor = hothers.cfa2,
                                One_Factor_Adj = hothers.cfa1adj))

########################################################


# --------------- Teacher and Staff Help -----------------

lapply(1:4, function(x) table(ds2long[ds2long$Wave == x, ]$STAFF_INT_BULY_SEX_VIOL_4,
                              ds2long[ds2long$Wave == x, ]$STAFF_INT_BULY_SEX_VIOL_5, useNA = "always"))

staffhelp <- efa_cfa_prelim(efa.samplel, StaffIntentVars)

#### Running and comparing EFA models ####
staffhelp.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = staffhelp$sample,
                                     varList = StaffIntentVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = StaffIntentVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  staffhelp.efa[[nf]] <- unrotated
}

lavTestLRT(staffhelp.efa[[1]], staffhelp.efa[[2]]) # 2 factor fits better


## Rotating 2 factor model loadings
staffhelp.rot <- get_std_loadings(staffhelp.efa[[2]], type = "std.all") %>%
  GPArotation::GPFoblq(method = "oblimin")
# All items load on first factor; factor 2 has really low loadings and 0 unique items

staffhelp.efa.loadings <- list(get_std_loadings(staffhelp.efa[[1]]),
                               staffhelp.rot$loadings)


staffhelp.syntax <- list(efa_cfa_syntax(get_std_loadings(staffhelp.efa[[1]])),
                         paste0("factor1 =~ ", paste(StaffIntentVars[-5], collapse = " + ")),
                         paste0("factor1 =~ ", paste(StaffIntentVars[-6], collapse = " + ")),
                         paste0("factor1 =~ ", paste(StaffIntentVars[-c(5,6)], collapse = " + ")))


## cfa model on all 4 waves
staffhelp.cfa1 <- map(.x = 1:4, ~cfa(mod = staffhelp.syntax[[1]],
                                     data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                     ordered = StaffIntentVars,
                                     cluster = "School",
                                     missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

# staffhelp.cfano5 <- map(.x = 1:4, ~cfa(mod = staffhelp.syntax[[2]],
#                                      data = cfa.samplel[cfa.samplel$Wave == .x, ],
#                                      ordered = StaffIntentVars,
#                                      cluster = "School",
#                                      missing = "pairwise")) %>%
#   cfa_wrapper(mods = .)
# 
# 
# staffhelp.cfano6 <- map(.x = 1:4, ~cfa(mod = staffhelp.syntax[[3]],
#                                        data = cfa.samplel[cfa.samplel$Wave == .x, ],
#                                        ordered = StaffIntentVars,
#                                        cluster = "School",
#                                        missing = "pairwise")) %>%
#   cfa_wrapper(mods = .)


staffhelp.cfano56 <- map(.x = 1:4, ~cfa(mod = staffhelp.syntax[[4]],
                                       data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                       ordered = StaffIntentVars,
                                       cluster = "School",
                                       missing = "pairwise")) %>%
  cfa_wrapper(mods = .)


# CONCLUSION: With all items, the model shows high correlations b/t items 4 & 5 and 6 & 7 with items 5 and 6 being heywood cases.
#             Removing these items produced a model with no warnings, but rmsea is still bad (~.25)

#### compiling results into a single object ####
staffhelp.results <- list(prelim = staffhelp,
                        efa.loadings = staffhelp.efa.loadings,
                        syntax = staffhelp.syntax,
                        cfas = list(One_Factor = staffhelp.cfa1,
                                    One_Factor_Adj = staffhelp.cfano56))

##########################################################

##############################
####     Other Scales     ####


# -------    Alcohol and Drugs      ---------------------

## Sample, polychoric correlations, consensus number of factors
alcoholdrugs <- efa_cfa_prelim(efa.samplel, AODVars)

#### Running 1 factor CFA ####
alcoholdrugs.syntax <- paste0("factor1 =~ ", paste(AODVars, collapse = " + "))

alcoholdrugs.cfa <- map(.x = 1:4, ~cfa(mod = alcoholdrugs.syntax,
                                     data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                     ordered = AODVars,
                                     cluster = "School",
                                     missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Seems alright

#### compiling results into a single object ####
alcoholdrugs.results <- list(prelim = alcoholdrugs,
                           efa.loadings = NULL,
                           syntax = alcoholdrugs.syntax,
                           cfas = alcoholdrugs.cfa)

#############################################################

# -------    Bullying    ---------------------

bullying <- efa_cfa_prelim(efa.samplel, BullyPerpVars)

#### Running and comparing EFA models ####
bullying.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = bullying$sample,
                                     varList = BullyPerpVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = BullyPerpVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  bullying.efa[[nf]] <- unrotated
}

lavTestLRT(bullying.efa[[1]], bullying.efa[[2]]) # 2 factor model did not seem to run well


## Rotating 2 factor model loadings
bullying.rot <- get_std_loadings(bullying.efa[[2]], type = "std.all") %>%
  GPArotation::GPFoblq(method = "oblimin")

bullying.efa.loadings <- list(get_std_loadings(bullying.efa[[1]]),
                            bullying.rot$loadings)
# yea the 2 factor model was not good.

## converting EFA results to CFA syntax
bullying.syntax <- efa_cfa_syntax(bullying.efa.loadings[[1]])


## cfa model on all 4 waves
bullying.cfa1 <- map(.x = 1:4, ~cfa(mod = bullying.syntax,
                                  data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                  ordered = BullyPerpVars,
                                  cluster = "School",
                                  missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: that'll work

#### compiling results into a single object ####
bullying.results <- list(prelim = bullying,
                       efa.loadings = bullying.efa.loadings,
                       syntax = bullying.syntax,
                       cfas = bullying.cfa1)


#############################################################


# -------   Cyberbullying    ---------------------

## Sample, polychoric correlations, consensus number of factors
cyberbully <- efa_cfa_prelim(efa.samplel, CyberBullyPerpVars)

#### Running 1 factor CFA ####
cyberbully.syntax <- paste0("factor1 =~ ", paste(CyberBullyPerpVars, collapse = " + "))

cyberbully.cfa <- map(.x = 1:4, ~cfa(mod = cyberbully.syntax,
                                       data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                       ordered = CyberBullyPerpVars,
                                       cluster = "School",
                                       missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: Perfect fit since only 3 items, but reliability and loadings good

#### compiling results into a single object ####
cyberbully.results <- list(prelim = cyberbully,
                             efa.loadings = NULL,
                             syntax = cyberbully.syntax,
                             cfas = cyberbully.cfa)

#############################################################


# -------    Depression & Anxiety    ---------------------

depanx <- efa_cfa_prelim(efa.samplel, DepAnxVars)

#### Running and comparing EFA models ####
depanx.efa <- vector("list", length = 2)

for(nf in 1:2){
  unrotated <- semTools::efaUnrotate(data = depanx$sample,
                                     varList = DepAnxVars,
                                     nf = nf,
                                     start = FALSE,
                                     ordered = DepAnxVars,
                                     missing = "pairwise",
                                     cluster = "School",
                                     parameterization = "theta")
  depanx.efa[[nf]] <- unrotated
}

lavTestLRT(depanx.efa[[1]], depanx.efa[[2]]) # 2 factor fits better


## Rotating 2 factor model loadings
depanx.rot <- get_std_loadings(depanx.efa[[2]], type = "std.all") %>%
  GPArotation::GPFoblq(method = "oblimin")

depanx.efa.loadings <- list(get_std_loadings(depanx.efa[[1]]),
                               depanx.rot$loadings)

## converting EFA results to CFA syntax
depanx.syntax <- lapply(depanx.efa.loadings, efa_cfa_syntax, single.item = 'drop')


## cfa model on all 4 waves
depanx.cfa1 <- map(.x = 1:4, ~cfa(mod = depanx.syntax[[1]],
                                     data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                     ordered = DepAnxVars,
                                     cluster = "School",
                                     missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

depanx.cfa2 <- map(.x = 1:4, ~cfa(mod = depanx.syntax[[2]],
                                  data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                  ordered = DepAnxVars,
                                  cluster = "School",
                                  missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION:
# 2 factor fits a bit better with f1 = feelings; f2 = thoughts
# 1 factor still good other than RMSEA ~.10 compared to .07
# What would be more appropriate factor names?

#### compiling results into a single object ####
depanx.results <- list(prelim = depanx,
                          efa.loadings = depanx.efa.loadings,
                          syntax = depanx.syntax,
                          cfas = list(One_Factor = depanx.cfa1,
                                      Two_Factor = depanx.cfa2))

#############################################################


# -------    Peer Victimization    ---------------------

## Sample, polychoric correlations, consensus number of factors
peervict <- efa_cfa_prelim(efa.samplel, PeerVictVars)

#### Running 1 factor CFA ####
peervict.syntax <- paste0("factor1 =~ ", paste(PeerVictVars, collapse = " + "))

peervict.cfa <- map(.x = 1:4, ~cfa(mod = peervict.syntax,
                                       data = cfa.samplel[cfa.samplel$Wave == .x, ],
                                       ordered = PeerVictVars,
                                       cluster = "School",
                                       missing = "pairwise")) %>%
  cfa_wrapper(mods = .)

## CONCLUSION: looks good

#### compiling results into a single object ####
peervict.results <- list(prelim = peervict,
                             efa.loadings = NULL,
                             syntax = peervict.syntax,
                             cfas = peervict.cfa)

#############################################################


# ---- Saving Scale Development Objects ---------------

save(efa.samplel, efa.samplew, cfa.samplel, cfa.samplew,
     efa.samplexp, efa.sampwexp, cfa.samplexp, cfa.sampwexp,
     exp.results, ncperp.results, ncvict.results, conperp.results, convict.results,
     hncperp.results, hncvict.results, cyber.results, dismiss.results,
     gwb.results, shhelpseek.results, shhelpatt.results,
     gha.results, hothers.results, staffhelp.results,
     alcoholdrugs.results, bullying.results, cyberbully.results,
     depanx.results, peervict.results,
     file = "Output/Construct_EFA_CFA.RData")
# load("Output/Construct_EFA_CFA.RData")

##################################################################


# -------   kfa   -------------------------
library(kfa)
all.scales <- c(OutcomeVars, ProtectiveVars, list(Exposure = ExposureVars), OtherScaleVars)
find_k(variables = ds2[paste0(all.scales$Contact_Perpetration, "_W1")], m = 2)
find_k(variables = ds2[ds2$Tx == 1, paste0(ExposureVars, "_W2")], m = 4)

scale.lengths <- map_dbl(all.scales, length)

#### Running kfa ####
## scales with < 8 items
kfas8 <- map(.x = all.scales[scale.lengths < 8],
             ~ds2[paste0(.x, "_W1")] %>%
               mutate(across(.fns = haven::zap_labels)) %>% # convert haven_labelled to numeric
               kfa(variables = .,
                   k = 10,
                   m = 2,
                   ordered = TRUE))

## scales with 8 or more items, except for exposure
kfas8plus <- map(.x = all.scales[scale.lengths >= 8 & scale.lengths < 15],
                 ~ds2[paste0(.x, "_W1")] %>%
                   mutate(across(.fns = haven::zap_labels)) %>% # convert haven_labelled to numeric
                   kfa(variables = .,
                       k = 10,
                       m = 3,
                       ordered = TRUE))


#### Generation Reports ####
map2(.x = c(kfas8, kfas8plus), .y = c(names(kfas8), names(kfas8plus)),
     ~kfa_report(models = .x,
           file.name = paste0("Output/", .y, "_w1_psychometric_report"),
           report.title = paste0(gsub("_", " ", .y), "Wave 1 Psychometric Report"),
           index = c("cfi.scaled", "rmsea.scaled", "srmr")))

#### Exposure ####
## kfa for exposure - using W2 and only Tx data
kfaexp <- map(.x = list(Active_Exposure = ActiveExpVars, Passive_Exposure = PassiveExpVars, Passive_Exposure_Drop4 = PassiveExpVars[-4]),
              ~kfa(data = ds2[ds2$Tx == 1, paste0(.x, "_W2")] %>% mutate(across(.fns = haven::zap_labels)),
                   k = 10,
                   m = 2,
                   ordered = TRUE))

kfaexp$Exposure <- kfa(data = ds2[ds2$Tx == 1, paste0(ExposureVars, "_W2")] %>% mutate(across(.fns = haven::zap_labels)),
                       k = 10,
                       m = 4,
                       ordered = TRUE,
                       custom.cfas = list(`Active-Passive` = paste0("active =~ ", paste(paste0(ActiveExpVars, "_W2"), collapse = " + "), "\n",
                                                                    "passive =~ ", paste(paste0(PassiveExpVars, "_W2"), collapse = " + "))),
                       cluster = "School") # cluster argument doesn't seem to change anything

map2(.x = kfaexp, .y = names(kfaexp),
     ~kfa_report(models = .x,
                 file.name = paste0("Output/", .y, "_w2_psychometric_report"),
                 report.title = paste0(gsub("_", " ", .y), " Wave 2 Psychometric Report"),
                 index = c("cfi.scaled", "rmsea.scaled", "srmr")))
kfa_report(models = kfaexp$Passive_Exposure_Drop4,
           file.name = paste0("Output/Passive_Exposure_Drop4_w2_psychometric_report"),
           report.title = paste0("Passive Exposure (Drop 4) Wave 2 Psychometric Report"),
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

#### Depression and Anxiety ####
kfadep <- map(.x = 1:4,
              ~kfa(data = ds2[, paste0(OtherScaleVars$Depression.Anxiety, "_W", .x)] %>% mutate(across(.fns = haven::zap_labels)),
                   k = 10,
                   m = 4,
                   ordered = TRUE))

map2(.x = kfadep, .y = 1:4,
    ~kfa_report(models = .x,
           file.name = paste0("Output/Reports/Psychometrics/Depression.Anxiety_w", .y, "_psychometric_report"),
           report.title = paste0("Depression/Anxiety Wave ", .y," Psychometric Report"),
           index = c("cfi.scaled", "rmsea.scaled", "srmr")))

## investigating dynamic fit index cutoffs (uses ML and assumes variables are continuous)
# factor loadings taken from Wave 4
dynamo <- dynamic::cfaOne(model = "DA =~ .92*d1 + .91*d2 + .85*d3 + .84*d4 + .94*d5 + .80*d6 +
                                    .87*d7 + .94*d8 + .90*d9 + .87*d10 + .92*d11 + .90*d12 + .93*d13",
                          n = 3088, plot = TRUE, manual = TRUE)
dynamo$cutoffs
# .04, .09, .99
# poor, poor, good


# Notes after reviewing results:
# - Bullying perp had 1 heywood case (item 5) in 1 of 10 folds
# - Contact vict had 1 heywood case (item 11) in 2 of 10 folds
# - Custom Exposure model had best fit, though could still be improved
#   - when run separately, active exposure was good, passive had high rmsea (.17) and srmr (.15)
#   - Dropping item 4 (1 of 2 presentation variables), rmsea improved to .12 and srmr .09
# - General Help Attitudes had poor rmsea (.20); 2-factor improves fit some (.16), but has high factor correlations (.84)
# - Intent to help others had poor rmsea (.31) and no other factor structure was extracted
# - Staff help intent had poor rmsea (.25) and 2 heywood cases (items 5 and 6) in 1 of 10 folds each.
# - 1-factor model had acceptable fit for all other models
# - Overall, kfa aligns with original psychometric report findings
