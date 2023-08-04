###########################################
#                                         #
#          Sources of Strength            #
#   Longitudinal Measurement Invariance   #
#                                         #
###########################################

## Notes:


## Loading packages and functions
source("Scripts/PnF.R")
load("Output/SoS_Data.RData")

# --------- Setting up models -----------------------------


## organizing time specific item names by base item name
# The items included here should be the items supported by the EFA/CFAs
# Produces list of lists, which is needed for the longIndNames argument of measEq.syntax
longIndNames <- map2(.x = c(list(Active_Exposure = ActiveExpVars, Passive_Exposure = PassiveExpVars[-4]),
                            OutcomeVars, ProtectiveVars[c(1,3,4)], OtherScaleVars),
                     .y = c(list(2:4, 2:4), rep(list(1:4), length(c(OutcomeVars, ProtectiveVars[c(1,3,4)], OtherScaleVars)))),
                    ~lapply(.x, paste0, "_W", .y) %>%
                      set_names(.x))

#### Base Models ####
# Note: The base model outlines the factor structure for which the measurement invariance models will be built.
# The base model is not run nor does it imply anything about the baseline time point

## Defining base model
mi.mods <- map2(.x = longIndNames[3:length(longIndNames)],
                .y = c("NCPerp", "NCVict","ConPerp", "ConVict", "HNCPerp", "HNCVict",
                       "Cybersex", "Dismiss", "GenWellBeing", "SHHelpAtt", "SHHelpSeek",
                       "AOD", "Bully", "Cyberbully", "DepAnx", "PeerVict"),
               ~write_mi_mod(list = .x,
                             fname = .y,
                             waves = 1:4))

## Exposure models are defined separately because write_mi_mod function currently uses waves argument as proxy for position in vector
# Active
mi.mods$Active_Exposure <- paste(lapply(2:4, function(x){
  paste0("Active", x," =~ ",
         paste(
           sapply(longIndNames$Active_Exposure, '[', x-1),  # extracts the item for each wave
           collapse = " + "))
}), collapse = "\n") 

# Passive - Note. Does not include item 4
mi.mods$Passive_Exposure <- paste(lapply(2:4, function(x){
  paste0("Passive", x," =~ ",
         paste(
           sapply(longIndNames$Passive_Exposure, '[', x-1),  # extracts the item for each wave
           collapse = " + "))
}), collapse = "\n") 

## checking base model set up
cat(as.character(mi.mods$No_Contact_Perpetration))

##########################################################

#########################################
####     Running MI by Construct     ####

## All models run with the following settings:
# ID.fac = "auto.fix.first",
# ID.cat = "millsap",
# long.equal = "",  # changes based on level of invariance tested
# auto = "all",
# group = NULL,
# group.equal = "",
# parameterization = "theta",
# meanstructure = TRUE,
# mimic = "Mplus",
# estimator = "WLSMV",
# missing = "pairwise",

# ----------- Exposure - Active ------------------
# These are binary items, so metric and scalar are equivalent
# Only uses Treatment students as all control students are missing

act.config <- mi_wrapper(model = mi.mods$Active_Exposure,
                         data = ds2[ds2$Tx == 1, ],
                         items = longIndNames$Active_Exposure,
                         fnames = list(Active = paste0("Active", 2:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise")

# cat(as.character(act.config$syntax))
# summary(act.config$fit, standardized = TRUE)

act.metric <- mi_wrapper(model = mi.mods$Active_Exposure,
                         data = ds2[ds2$Tx == 1, ],
                         items = longIndNames$Active_Exposure,
                         fnames = list(Active = paste0("Active", 2:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings"))

act.strict <- mi_wrapper(model = mi.mods$Active_Exposure,
                         data = ds2[ds2$Tx == 1, ],
                         items = longIndNames$Active_Exposure,
                         fnames = list(Active = paste0("Active", 2:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds", "residuals"))

act.pi.strict.syn <- as.character(act.strict$syntax) %>%
  sub("1\\*EXPOSURE_ACTIVITITY_PARTICIPATION_W2 \\+ theta.8_8\\*EXPOSURE_ACTIVITITY_PARTICIPATION_W2", "NA\\*EXPOSURE_ACTIVITITY_PARTICIPATION_W2 \\+ theta.9_9\\*EXPOSURE_ACTIVITITY_PARTICIPATION_W2", .)
act.pi.strict.fit <- lavaan(model = act.pi.strict.syn, data = ds2[ds2$Tx == 1, ], ordered = unlist(longIndNames$Active_Exposure, use.names = FALSE),
                           missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
act.pi.strict <- list(syntax = act.pi.strict.syn,
                     fit = act.pi.strict.fit)

#### compiling results across models ####

act.fits <- fits_wrapper(mod.list = list(act.config$fit, act.metric$fit, act.pi.strict$fit),
                         .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))


act.mi <- bind_rows(compare_mods(act.config$fit, act.metric$fit),
                    compare_mods(act.metric$fit, act.pi.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())

lavTestScore(act.strict$fit, cumulative = TRUE)
parTable(act.metric$fit) %>% View()
cat(as.character(act.strict$syntax))
modindices(act.strict$fit, sort. = TRUE) # says free activity participation w2


#######################################################################


###########################################################################


# ----------- Exposure - Passive ------------------
# These are binary items, so metric and scalar are equivalent
# Only uses Treatment students as all control students are missing

pass.config <- mi_wrapper(model = mi.mods$Passive_Exposure,
                               data = ds2[ds2$Tx == 1, ],
                               items = longIndNames$Passive_Exposure,
                               fnames = list(Passive = paste0("Passive", 2:4)),
                               cluster = NULL,
                               ordered = TRUE,
                          missing = "listwise")
# produces warning: The variance-covariance matrix of the estimated parameters (vcov)
#                   does not appear to be positive definite! The smallest eigenvalue
#                   (= 1.148844e-15) is close to zero. This may be a symptom that the
#                    model is not identified.
# Possibly just machine precision problem: https://groups.google.com/g/lavaan/c/4y5pmqRz4nk

cat(as.character(pass.config$syntax))
summary(pass.config$fit, standardized = TRUE) # estimates seem reasonable

pass.metric <- mi_wrapper(model = mi.mods$Passive_Exposure,
                          data = ds2[ds2$Tx ==1, ],
                          items = longIndNames$Passive_Exposure,
                          fnames = list(Passive = paste0("Passive", 2:4)),
                          cluster = NULL,
                          ordered = TRUE,
                          missing = "listwise",
                          long.equal = c("loadings"))

pass.strict <- mi_wrapper(model = mi.mods$Passive_Exposure,
                          data = ds2[ds2$Tx ==1, ],
                          items = longIndNames$Passive_Exposure,
                          fnames = list(Passive = paste0("Passive", 2:4)),
                          cluster = NULL,
                          ordered = TRUE,
                          missing = "listwise",
                          long.equal = c("loadings", "thresholds", "residuals"))

## partial invariance
pass.pi.metric.syn <- as.character(pass.metric$syntax) %>%
  sub("lambda.2_1\\*EXPOSURE_SOCIAL_MEDIA_W4", "lambda.2_4\\*EXPOSURE_SOCIAL_MEDIA_W4", .) %>%
  sub("lambda.3_1\\*EXPOSURE_STRENGTHS_RESILIENCE_W3", "lambda.3_3\\*EXPOSURE_STRENGTHS_RESILIENCE_W3", .) %>%
  sub("lambda.3_1\\*EXPOSURE_STRENGTHS_RESILIENCE_W4", "lambda.3_4\\*EXPOSURE_STRENGTHS_RESILIENCE_W4", .)
pass.pi.metric.fit <- lavaan(model = pass.pi.metric.syn, data = ds2[ds2$Tx == 1, ], ordered = unlist(longIndNames$Passive_Exposure, use.names = FALSE),
                            missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
pass.pi.metric <- list(syntax = pass.pi.metric.syn,
                      fit = pass.pi.metric.fit)

pass.pi.strict.syn <- as.character(pass.strict$syntax) %>%
  sub("lambda.2_1\\*EXPOSURE_SOCIAL_MEDIA_W4", "lambda.2_4\\*EXPOSURE_SOCIAL_MEDIA_W4", .) %>%
  sub("lambda.3_1\\*EXPOSURE_STRENGTHS_RESILIENCE_W3", "lambda.3_3\\*EXPOSURE_STRENGTHS_RESILIENCE_W3", .) %>%
  sub("lambda.3_1\\*EXPOSURE_STRENGTHS_RESILIENCE_W4", "lambda.3_4\\*EXPOSURE_STRENGTHS_RESILIENCE_W4", .) %>%
  sub("1\\*EXPOSURE_STRENGTHS_RESILIENCE_W4 \\+ theta.3_3\\*EXPOSURE_STRENGTHS_RESILIENCE_W4", "NA\\*EXPOSURE_STRENGTHS_RESILIENCE_W4 \\+ theta.7_7\\*EXPOSURE_STRENGTHS_RESILIENCE_W4", .) %>%
  sub("1\\*EXPOSURE_OWN_STRENGTHS_W4 \\+ theta.5_5\\*EXPOSURE_OWN_STRENGTHS_W4", "NA\\*EXPOSURE_OWN_STRENGTHS_W4 \\+ theta.8_8\\*EXPOSURE_OWN_STRENGTHS_W4", .) %>%
  sub("1\\*EXPOSURE_POSTERS_VIDEOS_W4 \\+ theta.1_1\\*EXPOSURE_POSTERS_VIDEOS_W4", "NA\\*EXPOSURE_POSTERS_VIDEOS_W4 \\+ theta.9_9\\*EXPOSURE_POSTERS_VIDEOS_W4", .)
pass.pi.strict.fit <- lavaan(model = pass.pi.strict.syn, data = ds2[ds2$Tx == 1, ], ordered = unlist(longIndNames$Passive_Exposure, use.names = FALSE),
                             missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
pass.pi.strict <- list(syntax = pass.pi.strict.syn,
                       fit = pass.pi.strict.fit)

#### compiling results across models ####

pass.fits <- fits_wrapper(mod.list = list(pass.config$fit, pass.pi.metric$fit, pass.pi.strict$fit),
                          .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))


pass.mi <- bind_rows(compare_mods(pass.config$fit, pass.pi.metric$fit),
                     compare_mods(pass.pi.metric$fit, pass.pi.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())


lavTestScore(pass.pi.strict$fit, cumulative = TRUE)
parTable(pass.strict$fit) %>% View()
cat(as.character(pass.pi.strict$syntax))
modindices(pass.pi.strict$fit, sort. = TRUE)
## metric
# 54.310 10 .000 None
# 32.599 9  .000 social media w4
# 20.737 8  .008 + resilience w4
# 8.449  7  .295 + resilience w3
## strict
# 40.632 11 .000 None


#######################################################################

# ---- Sexual Harassment (No contact) Perpetration --------------

## Items have been dichotomized, so metric and scalar are equivalent (the only threshold has already been constrained to identify the model)

ncperp.config <- mi_wrapper(model = mi.mods$No_Contact_Perpetration,
                            data = ds2,
                            items = longIndNames$No_Contact_Perpetration,
                            fnames = list(NCPerp = paste0("NCPerp", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise")

cat(as.character(ncperp.config$syntax))
summary(ncperp.config$fit, standardized = TRUE)
# Negative residual variance: W3 item 3
# Negative residual covariances: b/t W1&4 for 2 and 4
# occur in metric/scalar model too (not in strict after being constrained)

ncperp.metric <- mi_wrapper(model = mi.mods$No_Contact_Perpetration,
                            data = ds2,
                            items = longIndNames$No_Contact_Perpetration,
                            fnames = list(NCPerp = paste0("NCPerp", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise",
                            long.equal = c("loadings"))

ncperp.strict <- mi_wrapper(model = mi.mods$No_Contact_Perpetration,
                            data = ds2,
                            items = longIndNames$No_Contact_Perpetration,
                            fnames = list(NCPerp = paste0("NCPerp", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise",
                            long.equal = c("loadings", "thresholds", "residuals"))

## partial invariance
ncperp.pi.strict.syn <- as.character(ncperp.strict$syntax) %>%
  sub("1\\*SEX_VIOL_PERP_1_W1 \\+ theta.1_1\\*SEX_VIOL_PERP_1_W1", "NA\\*SEX_VIOL_PERP_1_W1 \\+ theta.5_5\\*SEX_VIOL_PERP_1_W1", .)
ncperp.pi.strict.fit <- lavaan(model = ncperp.pi.strict.syn, data = ds2, ordered = unlist(longIndNames$No_Contact_Perpetration, use.names = FALSE),
                            missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
ncperp.pi.strict <- list(syntax = ncperp.pi.strict.syn,
                      fit = ncperp.pi.strict.fit)

#### compiling results across models ####
ncperp.fits <- fits_wrapper(mod.list = list(ncperp.config$fit, ncperp.metric$fit, ncperp.pi.strict$fit),
                            .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))


ncperp.mi <- bind_rows(compare_mods(ncperp.config$fit, ncperp.metric$fit),
                       compare_mods(ncperp.metric$fit, ncperp.pi.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())

cat(as.character(ncperp.strict$syntax))
modindices(ncperp.pi.strict$fit, sort. = TRUE)


#######################################################################

# ---- Sexual Violence (contact) Perpetration --------------
## Items have been dichotomized, so metric and scalar are equivalent (the only threshold has already been constrained to identify the model)

# for items 9 - 13, almost all bivariate correlations ~ 1 in waves 3 and 4
conperp.config <- mi_wrapper(model = mi.mods$Contact_Perpetration,
                             data = ds2,
                             items = longIndNames$Contact_Perpetration,
                             fnames = list(ConPerp = paste0("ConPerp", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise")

conperp.metric <- mi_wrapper(model = mi.mods$Contact_Perpetration,
                             data = ds2,
                             items = longIndNames$Contact_Perpetration,
                             fnames = list(ConPerp = paste0("ConPerp", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                            long.equal = c("loadings"))

conperp.strict <- mi_wrapper(model = mi.mods$Contact_Perpetration,
                             data = ds2,
                             items = longIndNames$Contact_Perpetration,
                             fnames = list(ConPerp = paste0("ConPerp", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                            long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

conperp.fits <- fits_wrapper(mod.list = list(conperp.config$fit, conperp.metric$fit, conperp.strict$fit),
                             .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))


conperp.mi <- bind_rows(compare_mods(conperp.config$fit, conperp.metric$fit),
                       compare_mods(conperp.metric$fit, conperp.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())

#######################################################################

# ---- Sexual Harassment (No contact) Victimization --------------
## Items have been dichotomized, so metric and scalar are equivalent (the only threshold has already been constrained to identify the model)

# (= 7.358337e-15) 
ncvict.config <- mi_wrapper(model = mi.mods$No_Contact_Victimization,
                            data = ds2,
                            items = longIndNames$No_Contact_Victimization,
                            fnames = list(NCVict = paste0("NCVict", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise")

ncvict.metric <- mi_wrapper(model = mi.mods$No_Contact_Victimization,
                            data = ds2,
                            items = longIndNames$No_Contact_Victimization,
                            fnames = list(NCVict = paste0("NCVict", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise",
                            long.equal = c("loadings"))


ncvict.strict <- mi_wrapper(model = mi.mods$No_Contact_Victimization,
                            data = ds2,
                            items = longIndNames$No_Contact_Victimization,
                            fnames = list(NCVict = paste0("NCVict", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise",
                            long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

ncvict.fits <- fits_wrapper(mod.list = list(ncvict.config$fit, ncvict.metric$fit, ncvict.strict$fit),
                            .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))


ncvict.mi <- bind_rows(compare_mods(ncvict.config$fit, ncvict.metric$fit),
                       compare_mods(ncvict.metric$fit, ncvict.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())

#######################################################################


# ---- Sexual Violence (Contact) Victimization --------------
## Items have been dichotomized, so metric and scalar are equivalent (the only threshold has already been constrained to identify the model)

convict.config <- mi_wrapper(model = mi.mods$Contact_Victimization,
                             data = ds2,
                             items = longIndNames$Contact_Victimization,
                             fnames = list(ConVict = paste0("ConVict", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise")

convict.metric <- mi_wrapper(model = mi.mods$Contact_Victimization,
                             data = ds2,
                             items = longIndNames$Contact_Victimization,
                             fnames = list(ConVict = paste0("ConVict", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings"))

convict.strict <- mi_wrapper(model = mi.mods$Contact_Victimization,
                             data = ds2,
                             items = longIndNames$Contact_Victimization,
                             fnames = list(ConVict = paste0("ConVict", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

convict.fits <- fits_wrapper(mod.list = list(convict.config$fit, convict.metric$fit, convict.strict$fit),
                             .id = "model",digits = 3) %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))


convict.mi <- bind_rows(compare_mods(convict.config$fit, convict.metric$fit),
                        compare_mods(convict.metric$fit, convict.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())

#######################################################################

# ----- Homophobic Name Calling Perpetration ---------------

hncperp.config <- mi_wrapper(model = mi.mods$HNC_Perpetration,
                            data = ds2,
                            items = longIndNames$HNC_Perpetration,
                            fnames = list(HNCPerp = paste0("HNCPerp", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise")

hncperp.metric <- mi_wrapper(model = mi.mods$HNC_Perpetration,
                             data = ds2,
                             items = longIndNames$HNC_Perpetration,
                             fnames = list(HNCPerp = paste0("HNCPerp", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings"))

hncperp.scalar <- mi_wrapper(model = mi.mods$HNC_Perpetration,
                             data = ds2,
                             items = longIndNames$HNC_Perpetration,
                             fnames = list(HNCPerp = paste0("HNCPerp", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds"))

hncperp.strict <- mi_wrapper(model = mi.mods$HNC_Perpetration,
                             data = ds2,
                             items = longIndNames$HNC_Perpetration,
                             fnames = list(HNCPerp = paste0("HNCPerp", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds", "residuals"))

## paritial invariance
hncperp.pi.metric.syn <- as.character(hncperp.metric$syntax) %>%
  sub("lambda.4_1\\*HOM_PERP_4_W2", "lambda.4_2\\*HOM_PERP_4_W2", .) %>%
  sub("lambda.3_1\\*HOM_PERP_3_W1", "lambda.3_2\\*HOM_PERP_3_W1", .)
hncperp.pi.metric.fit <- lavaan(model = hncperp.pi.metric.syn, data = ds2, ordered = unlist(longIndNames$HNC_Perpetration, use.names = FALSE),
                               missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
hncperp.pi.metric <- list(syntax = hncperp.pi.metric.syn,
                         fit = hncperp.pi.metric.fit)

hncperp.pi.scalar.syn <- as.character(hncperp.scalar$syntax) %>%
  sub("lambda.4_1\\*HOM_PERP_4_W2", "lambda.4_2\\*HOM_PERP_4_W2", .) %>%
  sub("lambda.3_1\\*HOM_PERP_3_W1", "lambda.3_2\\*HOM_PERP_3_W1", .)
hncperp.pi.scalar.fit <- lavaan(model = hncperp.pi.scalar.syn, data = ds2, ordered = unlist(longIndNames$HNC_Perpetration, use.names = FALSE),
                                missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
hncperp.pi.scalar <- list(syntax = hncperp.pi.scalar.syn,
                          fit = hncperp.pi.scalar.fit)

# strict invariance will not be reached even after freeing item 1 residual variances
hncperp.pi.strict.syn <- as.character(hncperp.strict$syntax) %>%
  sub("lambda.4_1\\*HOM_PERP_4_W2", "lambda.4_2\\*HOM_PERP_4_W2", .) %>%
  sub("lambda.3_1\\*HOM_PERP_3_W1", "lambda.3_2\\*HOM_PERP_3_W1", .)
  # sub("1\\*HOM_PERP_1_W2 \\+ theta.1_1\\*HOM_PERP_1_W2", "NA\\*HOM_PERP_1_W2 \\+ theta.6_6\\*HOM_PERP_1_W2", .) %>%
  # sub("1\\*HOM_PERP_1_W1 \\+ theta.1_1\\*HOM_PERP_1_W1", "NA\\*HOM_PERP_1_W1 \\+ theta.7_7\\*HOM_PERP_1_W1", .) %>%
  # sub("1\\*HOM_PERP_1_W3 \\+ theta.1_1\\*HOM_PERP_1_W3", "NA\\*HOM_PERP_1_W3 \\+ theta.8_8\\*HOM_PERP_1_W3", .)
hncperp.pi.strict.fit <- lavaan(model = hncperp.pi.strict.syn, data = ds2, ordered = unlist(longIndNames$HNC_Perpetration, use.names = FALSE),
                                missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
hncperp.pi.strict <- list(syntax = hncperp.pi.strict.syn,
                          fit = hncperp.pi.strict.fit)


#### compiling results across models ####

hncperp.fits <- fits_wrapper(mod.list = list(hncperp.config$fit, hncperp.pi.metric$fit,
                                             hncperp.pi.scalar$fit, hncperp.pi.strict$fit),
                             .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

hncperp.mi <- bind_rows(compare_mods(hncperp.config$fit, hncperp.pi.metric$fit),
                        compare_mods(hncperp.pi.metric$fit, hncperp.pi.scalar$fit),
                        compare_mods(hncperp.pi.scalar$fit, hncperp.pi.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

lavTestScore(hncperp.pi.metric$fit, cumulative = TRUE)
parTable(hncperp.metric$fit) %>% View()
cat(as.character(hncperp.pi.strict$syntax))
modindices(hncperp.pi.strict$fit, sort. = TRUE)


#######################################################################



# ----- Homophobic Name Calling Victimization ---------------

hncvict.config <- mi_wrapper(model = mi.mods$HNC_Victimization,
                            data = ds2,
                            items = longIndNames$HNC_Victimization,
                            fnames = list(HNCVict = paste0("HNCVict", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise")

hncvict.metric <- mi_wrapper(model = mi.mods$HNC_Victimization,
                             data = ds2,
                             items = longIndNames$HNC_Victimization,
                             fnames = list(HNCVict = paste0("HNCVict", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings"))

hncvict.scalar <- mi_wrapper(model = mi.mods$HNC_Victimization,
                             data = ds2,
                             items = longIndNames$HNC_Victimization,
                             fnames = list(HNCVict = paste0("HNCVict", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds"))

hncvict.strict <- mi_wrapper(model = mi.mods$HNC_Victimization,
                             data = ds2,
                             items = longIndNames$HNC_Victimization,
                             fnames = list(HNCVict = paste0("HNCVict", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds", "residuals"))

## paritial invariance - metric invariance cannot be established
# hncvict.pi.metric.syn <- as.character(hncvict.metric$syntax) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W4", "lambda.3_4\\*HOM_VICT_3_W4", .) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W3", "lambda.3_3\\*HOM_VICT_3_W3", .) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W2", "lambda.3_2\\*HOM_VICT_3_W2", .) %>%
#   sub("lambda.2_1\\*HOM_VICT_2_W4", "lambda.2_4\\*HOM_VICT_2_W4", .)
# hncvict.pi.metric.fit <- lavaan(model = hncvict.pi.metric.syn, data = ds2, ordered = unlist(longIndNames$HNC_Victimization, use.names = FALSE),
#                                 missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
# hncvict.pi.metric <- list(syntax = hncvict.pi.metric.syn,
#                           fit = hncvict.pi.metric.fit)
# 
# hncvict.pi.scalar.syn <- as.character(hncvict.scalar$syntax) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W4", "lambda.3_4\\*HOM_VICT_3_W4", .) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W3", "lambda.3_3\\*HOM_VICT_3_W3", .) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W2", "lambda.3_2\\*HOM_VICT_3_W2", .) %>%
#   sub("lambda.2_1\\*HOM_VICT_2_W4", "lambda.2_4\\*HOM_VICT_2_W4", .)
# hncvict.pi.scalar.fit <- lavaan(model = hncvict.pi.scalar.syn, data = ds2, ordered = unlist(longIndNames$HNC_Victimization, use.names = FALSE),
#                                 missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
# hncvict.pi.scalar <- list(syntax = hncvict.pi.scalar.syn,
#                           fit = hncvict.pi.scalar.fit)
# 
# 
# hncvict.pi.strict.syn <- as.character(hncvict.strict$syntax) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W4", "lambda.3_4\\*HOM_VICT_3_W4", .) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W3", "lambda.3_3\\*HOM_VICT_3_W3", .) %>%
#   sub("lambda.3_1\\*HOM_VICT_3_W2", "lambda.3_2\\*HOM_VICT_3_W2", .) %>%
#   sub("lambda.2_1\\*HOM_VICT_2_W4", "lambda.2_4\\*HOM_VICT_2_W4", .)
# hncvict.pi.strict.fit <- lavaan(model = hncvict.pi.strict.syn, data = ds2, ordered = unlist(longIndNames$HNC_Victimization, use.names = FALSE),
#                                 missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
# hncvict.pi.strict <- list(syntax = hncvict.pi.strict.syn,
#                           fit = hncvict.pi.strict.fit)

#### compiling results across models ####

hncvict.fits <- fits_wrapper(mod.list = list(hncvict.config$fit, hncvict.metric$fit,
                                             hncvict.scalar$fit, hncvict.strict$fit),
                             .id = "model",
                             digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

hncvict.mi <- bind_rows(compare_mods(hncvict.config$fit, hncvict.metric$fit),
                        compare_mods(hncvict.metric$fit, hncvict.scalar$fit),
                        compare_mods(hncvict.scalar$fit, hncvict.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

lavTestScore(hncvict.pi.metric$fit, cumulative = TRUE)
parTable(hncvict.metric$fit) %>% View()
cat(as.character(hncperp.pi.strict$syntax))
modindices(hncperp.pi.strict$fit, sort. = TRUE)

############################################################

# ----- Cybersex Perpetration ---------------

cyber.config <- mi_wrapper(model = mi.mods$Cybersex_Perpetration,
                            data = ds2,
                            items = longIndNames$Cybersex_Perpetration,
                            fnames = list(Cybersex = paste0("Cybersex", 1:4)),
                            cluster = NULL,
                           ordered = TRUE,
                           missing = "listwise")

cyber.metric <- mi_wrapper(model = mi.mods$Cybersex_Perpetration,
                           data = ds2,
                           items = longIndNames$Cybersex_Perpetration,
                           fnames = list(Cybersex = paste0("Cybersex", 1:4)),
                           cluster = NULL,
                           ordered = TRUE,
                           missing = "listwise",
                           long.equal = c("loadings"))

cyber.scalar <- mi_wrapper(model = mi.mods$Cybersex_Perpetration,
                           data = ds2,
                           items = longIndNames$Cybersex_Perpetration,
                           fnames = list(Cybersex = paste0("Cybersex", 1:4)),
                           cluster = NULL,
                           ordered = TRUE,
                           missing = "listwise",
                           long.equal = c("loadings", "thresholds"))

cyber.strict <- mi_wrapper(model = mi.mods$Cybersex_Perpetration,
                           data = ds2,
                           items = longIndNames$Cybersex_Perpetration,
                           fnames = list(Cybersex = paste0("Cybersex", 1:4)),
                           cluster = NULL,
                           ordered = TRUE,
                           missing = "listwise",
                           long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

cyber.fits <- fits_wrapper(mod.list = list(cyber.config$fit, cyber.metric$fit,
                                             cyber.scalar$fit, cyber.strict$fit),
                             .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

cyber.mi <- bind_rows(compare_mods(cyber.config$fit, cyber.metric$fit),
                        compare_mods(cyber.metric$fit, cyber.scalar$fit),
                        compare_mods(cyber.scalar$fit, cyber.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

############################################################

# ----- Sexual Violence Dismissiveness ---------------
dismiss.config <- mi_wrapper(model = mi.mods$SV_Dismissiveness,
                            data = ds2,
                            items = longIndNames$SV_Dismissiveness,
                            fnames = list(Dismiss = paste0("Dismiss", 1:4)),
                            cluster = NULL,
                            ordered = TRUE,
                            missing = "listwise")

dismiss.metric <- mi_wrapper(model = mi.mods$SV_Dismissiveness,
                             data = ds2,
                             items = longIndNames$SV_Dismissiveness,
                             fnames = list(Dismiss = paste0("Dismiss", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings"))

dismiss.scalar <- mi_wrapper(model = mi.mods$SV_Dismissiveness,
                             data = ds2,
                             items = longIndNames$SV_Dismissiveness,
                             fnames = list(Dismiss = paste0("Dismiss", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds"))

dismiss.strict <- mi_wrapper(model = mi.mods$SV_Dismissiveness,
                             data = ds2,
                             items = longIndNames$SV_Dismissiveness,
                             fnames = list(Dismiss = paste0("Dismiss", 1:4)),
                             cluster = NULL,
                             ordered = TRUE,
                             missing = "listwise",
                             long.equal = c("loadings", "thresholds", "residuals"))

## partial invariance - partial metric is the best we can do here methinks
dismiss.pi.metric.syn <- as.character(dismiss.metric$syntax) %>%
  sub("lambda.6_1\\*DISMISS_SEX_VIOL_6_W4", "lambda.6_4\\*DISMISS_SEX_VIOL_6_W4", .) %>%
  sub("lambda.2_1\\*DISMISS_SEX_VIOL_2_W4", "lambda.2_4\\*DISMISS_SEX_VIOL_2_W4", .) %>%
  sub("lambda.3_1\\*DISMISS_SEX_VIOL_3_W4", "lambda.3_4\\*DISMISS_SEX_VIOL_3_W4", .) %>%
  sub("lambda.6_1\\*DISMISS_SEX_VIOL_6_W3", "lambda.6_3\\*DISMISS_SEX_VIOL_6_W3", .)
dismiss.pi.metric.fit <- lavaan(model = dismiss.pi.metric.syn, data = ds2, ordered = unlist(longIndNames$SV_Dismissiveness, use.names = FALSE),
                                missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
dismiss.pi.metric <- list(syntax = dismiss.pi.metric.syn,
                          fit = dismiss.pi.metric.fit)

dismiss.pi.scalar.syn <- as.character(dismiss.scalar$syntax) %>%
  sub("lambda.6_1\\*DISMISS_SEX_VIOL_6_W4", "lambda.6_4\\*DISMISS_SEX_VIOL_6_W4", .) %>%
  sub("lambda.2_1\\*DISMISS_SEX_VIOL_2_W4", "lambda.2_4\\*DISMISS_SEX_VIOL_2_W4", .) %>%
  sub("lambda.3_1\\*DISMISS_SEX_VIOL_3_W4", "lambda.3_4\\*DISMISS_SEX_VIOL_3_W4", .) %>%
  sub("lambda.6_1\\*DISMISS_SEX_VIOL_6_W3", "lambda.6_3\\*DISMISS_SEX_VIOL_6_W3", .)
  # sub("DISMISS_SEX_VIOL_1_W3 \\| NA\\*t1 \\+ DISMISS_SEX_VIOL_1_W1\\.thr1", "DISMISS_SEX_VIOL_1_W3 \\| NA\\*t1 \\+ DISMISS_SEX_VIOL_1_W3\\.thr1", .)
dismiss.pi.scalar.fit <- lavaan(model = dismiss.pi.scalar.syn, data = ds2, ordered = unlist(longIndNames$SV_Dismissiveness, use.names = FALSE),
                                missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
dismiss.pi.scalar <- list(syntax = dismiss.pi.scalar.syn,
                          fit = dismiss.pi.scalar.fit)

dismiss.pi.strict.syn <- as.character(dismiss.strict$syntax) %>%
  sub("lambda.6_1\\*DISMISS_SEX_VIOL_6_W4", "lambda.6_4\\*DISMISS_SEX_VIOL_6_W4", .) %>%
  sub("lambda.2_1\\*DISMISS_SEX_VIOL_2_W4", "lambda.2_4\\*DISMISS_SEX_VIOL_2_W4", .) %>%
  sub("lambda.3_1\\*DISMISS_SEX_VIOL_3_W4", "lambda.3_4\\*DISMISS_SEX_VIOL_3_W4", .) %>%
  sub("lambda.6_1\\*DISMISS_SEX_VIOL_6_W3", "lambda.6_3\\*DISMISS_SEX_VIOL_6_W3", .)
dismiss.pi.strict.fit <- lavaan(model = dismiss.pi.strict.syn, data = ds2, ordered = unlist(longIndNames$SV_Dismissiveness, use.names = FALSE),
                                missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
dismiss.pi.strict <- list(syntax = dismiss.pi.strict.syn,
                          fit = dismiss.pi.strict.fit)

#### compiling results across models ####

dismiss.fits <- fits_wrapper(mod.list = list(dismiss.config$fit, dismiss.pi.metric$fit,
                                           dismiss.pi.scalar$fit, dismiss.pi.strict$fit),
                           .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

dismiss.mi <- bind_rows(compare_mods(dismiss.config$fit, dismiss.pi.metric$fit),
                      compare_mods(dismiss.pi.metric$fit, dismiss.pi.scalar$fit),
                      compare_mods(dismiss.pi.scalar$fit, dismiss.pi.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

lavTestScore(dismiss.pi.metric$fit, cumulative = TRUE)
parTable(dismiss.scalar$fit) %>% View()
cat(as.character(dismiss.scalar$syntax))
modindices(hncperp.pi.strict$fit, sort. = TRUE)


############################################################


# -----------  General Wellbeing  -------------------

gwb.config <- mi_wrapper(model = mi.mods$General_Well.being,
                         data = ds2,
                         items = longIndNames$General_Well.being,
                         fnames = list(GenWellBeing = paste0("GenWellBeing", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise")

# cat(as.character(gwb.config$syntax))

gwb.metric <- mi_wrapper(model = mi.mods$General_Well.being,
                         data = ds2,
                         items = longIndNames$General_Well.being,
                         fnames = list(GenWellBeing = paste0("GenWellBeing", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings"))

gwb.scalar <- mi_wrapper(model = mi.mods$General_Well.being,
                         data = ds2,
                         items = longIndNames$General_Well.being,
                         fnames = list(GenWellBeing = paste0("GenWellBeing", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds"))

gwb.strict <- mi_wrapper(model = mi.mods$General_Well.being,
                         data = ds2,
                         items = longIndNames$General_Well.being,
                         fnames = list(GenWellBeing = paste0("GenWellBeing", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

gwb.fits <- fits_wrapper(mod.list = list(gwb.config$fit, gwb.metric$fit,
                                         gwb.scalar$fit, gwb.strict$fit),
                         .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


gwb.mi <- bind_rows(compare_mods(gwb.config$fit, gwb.metric$fit),
                    compare_mods(gwb.metric$fit, gwb.scalar$fit),
                    compare_mods(gwb.scalar$fit, gwb.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

########################################################

# -----------  Sexual Harassment Help Attitues  --------------

shhelpatt.config <- mi_wrapper(model = mi.mods$SH_Help_Attitudes,
                               data = ds2,
                               items = longIndNames$SH_Help_Attitudes,
                               fnames = list(SHHelpAtt = paste0("SHHelpAtt", 1:4)),
                               cluster = NULL,
                               ordered = TRUE,
                               missing = "listwise")

# cat(as.character(shhelpatt.config$syntax))
# temp <- fits_wrapper(list(shhelpatt.config$fit))

shhelpatt.metric <- mi_wrapper(model = mi.mods$SH_Help_Attitudes,
                               data = ds2,
                               items = longIndNames$SH_Help_Attitudes,
                               fnames = list(SHHelpAtt = paste0("SHHelpAtt", 1:4)),
                               cluster = NULL,
                               ordered = TRUE,
                               missing = "listwise",
                               long.equal = c("loadings"))

shhelpatt.scalar <- mi_wrapper(model = mi.mods$SH_Help_Attitudes,
                               data = ds2,
                               items = longIndNames$SH_Help_Attitudes,
                               fnames = list(SHHelpAtt = paste0("SHHelpAtt", 1:4)),
                               cluster = NULL,
                               ordered = TRUE,
                               missing = "listwise",
                               long.equal = c("loadings", "thresholds"))

shhelpatt.strict <- mi_wrapper(model = mi.mods$SH_Help_Attitudes,
                               data = ds2,
                               items = longIndNames$SH_Help_Attitudes,
                               fnames = list(SHHelpAtt = paste0("SHHelpAtt", 1:4)),
                               cluster = NULL,
                               ordered = TRUE,
                               missing = "listwise",
                               long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

shhelpatt.fits <- fits_wrapper(mod.list = list(shhelpatt.config$fit, shhelpatt.metric$fit,
                                               shhelpatt.scalar$fit, shhelpatt.strict$fit),
                               .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


shhelpatt.mi <- bind_rows(compare_mods(shhelpatt.config$fit, shhelpatt.metric$fit),
                          compare_mods(shhelpatt.metric$fit, shhelpatt.scalar$fit),
                          compare_mods(shhelpatt.scalar$fit, shhelpatt.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())


########################################################

# -----------  Sexual Harassment Help Seeking  -------------------

shhelpseek.config <- mi_wrapper(model = mi.mods$SH_Help_Seeking,
                                data = ds2,
                                items = longIndNames$SH_Help_Seeking,
                                fnames = list(SHHelpSeek = paste0("SHHelpSeek", 1:4)),
                                cluster = NULL,
                                ordered = TRUE,
                                missing = "listwise")

cat(as.character(shhelpseek.config$syntax))
temp <- fits_wrapper(list(shhelpseek.config$fit))

shhelpseek.metric <- mi_wrapper(model = mi.mods$SH_Help_Seeking,
                                data = ds2,
                                items = longIndNames$SH_Help_Seeking,
                                fnames = list(SHHelpSeek = paste0("SHHelpSeek", 1:4)),
                                cluster = NULL,
                                ordered = TRUE,
                                missing = "listwise",
                                long.equal = c("loadings"))

shhelpseek.scalar <- mi_wrapper(model = mi.mods$SH_Help_Seeking,
                                data = ds2,
                                items = longIndNames$SH_Help_Seeking,
                                fnames = list(SHHelpSeek = paste0("SHHelpSeek", 1:4)),
                                cluster = NULL,
                                ordered = TRUE,
                                missing = "listwise",
                                long.equal = c("loadings", "thresholds"))

shhelpseek.strict <- mi_wrapper(model = mi.mods$SH_Help_Seeking,
                                data = ds2,
                                items = longIndNames$SH_Help_Seeking,
                                fnames = list(SHHelpSeek = paste0("SHHelpSeek", 1:4)),
                                cluster = NULL,
                                ordered = TRUE,
                                missing = "listwise",
                                long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

shhelpseek.fits <- fits_wrapper(mod.list = list(shhelpseek.config$fit, shhelpseek.metric$fit,
                                                shhelpseek.scalar$fit, shhelpseek.strict$fit),
                                .id = "model",
                                digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


shhelpseek.mi <- bind_rows(compare_mods(shhelpseek.config$fit, shhelpseek.metric$fit),
                           compare_mods(shhelpseek.metric$fit, shhelpseek.scalar$fit),
                           compare_mods(shhelpseek.scalar$fit, shhelpseek.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

########################################################



# -----------  Teacher and Staff Help Intentions  ------------------------

# # Items 5 and 4 were highly correlated in Wave 3 and Items 7 and 6 in Wave 4
# staffhelp.config <- mi_wrapper(model = mi.mods$Staff_Help_Intent,
#                                data = ds2,
#                                items = longIndNames$Staff_Help_Intent,
#                                fnames = list(StaffHelp = paste0("StaffHelp", 1:4)),
#                                cluster = NULL,
#                                ordered = TRUE)
# 
# cat(as.character(staffhelp.config$syntax))
# temp <- fits_wrapper(list(staffhelp.config$fit))
# 
# staffhelp.metric <- mi_wrapper(model = mi.mods$Staff_Help_Intent,
#                                data = ds2,
#                                items = longIndNames$Staff_Help_Intent,
#                                fnames = list(StaffHelp = paste0("StaffHelp", 1:4)),
#                                cluster = NULL,
#                                ordered = TRUE,
#                                long.equal = c("loadings"))
# 
# staffhelp.scalar <- mi_wrapper(model = mi.mods$Staff_Help_Intent,
#                                data = ds2,
#                                items = longIndNames$Staff_Help_Intent,
#                                fnames = list(StaffHelp = paste0("StaffHelp", 1:4)),
#                                cluster = NULL,
#                                ordered = TRUE,
#                                long.equal = c("loadings", "thresholds"))
# 
# staffhelp.strict <- mi_wrapper(model = mi.mods$Staff_Help_Intent,
#                                data = ds2,
#                                items = longIndNames$Staff_Help_Intent,
#                                fnames = list(StaffHelp = paste0("StaffHelp", 1:4)),
#                                cluster = NULL,
#                                ordered = TRUE,
#                                long.equal = c("loadings", "thresholds", "residuals"))
# 
# #### compiling results across models ####
# 
# staffhelp.fits <- fits_wrapper(mod.list = list(staffhelp.config$fit, staffhelp.metric$fit,
#                                                staffhelp.scalar$fit, staffhelp.strict$fit),
#                                .id = "model", digits = 3) %>%
#   mutate(model = c("Configural", "Metric", "Scalar", "Strict"))
# 
# 
# staffhelp.mi <- bind_rows(compare_mods(staffhelp.config$fit, staffhelp.metric$fit),
#                           compare_mods(staffhelp.metric$fit, staffhelp.scalar$fit),
#                           compare_mods(staffhelp.scalar$fit, staffhelp.strict$fit)) %>%
#   mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
#   select(constraint_added, everything())

########################################################

# ----------- Alcohol and Drugs ------------------

aod.config <- mi_wrapper(model = mi.mods$Alcohol_and_Drug,
                         data = ds2,
                         items = longIndNames$Alcohol_and_Drug,
                         fnames = list(AOD = paste0("AOD", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise")

# cat(as.character(aod.config$syntax))

aod.metric <- mi_wrapper(model = mi.mods$Alcohol_and_Drug,
                         data = ds2,
                         items = longIndNames$Alcohol_and_Drug,
                         fnames = list(AOD = paste0("AOD", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings"))

aod.scalar <- mi_wrapper(model = mi.mods$Alcohol_and_Drug,
                         data = ds2,
                         items = longIndNames$Alcohol_and_Drug,
                         fnames = list(AOD = paste0("AOD", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds"))

aod.strict <- mi_wrapper(model = mi.mods$Alcohol_and_Drug,
                         data = ds2,
                         items = longIndNames$Alcohol_and_Drug,
                         fnames = list(AOD = paste0("AOD", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

aod.fits <- fits_wrapper(mod.list = list(aod.config$fit, aod.metric$fit,
                                         aod.scalar$fit, aod.strict$fit),
                         .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


aod.mi <- bind_rows(compare_mods(aod.config$fit, aod.metric$fit),
                    compare_mods(aod.metric$fit, aod.scalar$fit),
                    compare_mods(aod.scalar$fit, aod.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

########################################################

# ----------- Bullying ------------------

bully.config <- mi_wrapper(model = mi.mods$Bullying,
                         data = ds2,
                         items = longIndNames$Bullying,
                         fnames = list(Bully = paste0("Bully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise")

# cat(as.character(bully.config$syntax))

bully.metric <- mi_wrapper(model = mi.mods$Bullying,
                         data = ds2,
                         items = longIndNames$Bullying,
                         fnames = list(Bully = paste0("Bully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings"))

bully.scalar <- mi_wrapper(model = mi.mods$Bullying,
                         data = ds2,
                         items = longIndNames$Bullying,
                         fnames = list(Bully = paste0("Bully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds"))

bully.strict <- mi_wrapper(model = mi.mods$Bullying,
                         data = ds2,
                         items = longIndNames$Bullying,
                         fnames = list(Bully = paste0("Bully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

bully.fits <- fits_wrapper(mod.list = list(bully.config$fit, bully.metric$fit,
                                         bully.scalar$fit, bully.strict$fit),
                         .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


bully.mi <- bind_rows(compare_mods(bully.config$fit, bully.metric$fit),
                    compare_mods(bully.metric$fit, bully.scalar$fit),
                    compare_mods(bully.scalar$fit, bully.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

########################################################

# ----------- Cyberbullying ------------------
# Note: need to use pairwise due to low frequencies (otherwise can collapse categories)
cyberbully.config <- mi_wrapper(model = mi.mods$Cyberbullying,
                         data = ds2,
                         items = longIndNames$Cyberbullying,
                         fnames = list(Cyberbully = paste0("Cyberbully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "pairwise")

# cat(as.character(cyberbully.config$syntax))

cyberbully.metric <- mi_wrapper(model = mi.mods$Cyberbullying,
                         data = ds2,
                         items = longIndNames$Cyberbullying,
                         fnames = list(Cyberbully = paste0("Cyberbully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "pairwise",
                         long.equal = c("loadings"))

cyberbully.scalar <- mi_wrapper(model = mi.mods$Cyberbullying,
                         data = ds2,
                         items = longIndNames$Cyberbullying,
                         fnames = list(Cyberbully = paste0("Cyberbully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "pairwise",
                         long.equal = c("loadings", "thresholds"))

cyberbully.strict <- mi_wrapper(model = mi.mods$Cyberbullying,
                         data = ds2,
                         items = longIndNames$Cyberbullying,
                         fnames = list(Cyberbully = paste0("Cyberbully", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "pairwise",
                         long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

cyberbully.fits <- fits_wrapper(mod.list = list(cyberbully.config$fit, cyberbully.metric$fit,
                                         cyberbully.scalar$fit, cyberbully.strict$fit),
                         .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


cyberbully.mi <- bind_rows(compare_mods(cyberbully.config$fit, cyberbully.metric$fit),
                    compare_mods(cyberbully.metric$fit, cyberbully.scalar$fit),
                    compare_mods(cyberbully.scalar$fit, cyberbully.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

########################################################

# ----------- Depression and Anxiety ------------------

# Had trouble running a 1-factor model. The CFAs suggested a 2 factor model too.

depanx.config <- mi_wrapper(model = mi.mods$Depression.Anxiety,
                         data = ds2,
                         items = longIndNames$Depression.Anxiety,
                         fnames = list(DepAnx = paste0("DepAnx", 1:4)),
                         cluster = NULL,
                         ordered = TRUE)

# cat(as.character(depanx.config$syntax))

# depanx.metric <- mi_wrapper(model = mi.mods$Depression.Anxiety,
#                          data = ds2,
#                          items = longIndNames$Depression.Anxiety,
#                          fnames = list(DepAnx = paste0("DepAnx", 1:4)),
#                          cluster = NULL,
#                          ordered = TRUE,
#                          long.equal = c("loadings"))
# 
# depanx.scalar <- mi_wrapper(model = mi.mods$Depression.Anxiety,
#                          data = ds2,
#                          items = longIndNames$Depression.Anxiety,
#                          fnames = list(DepAnx = paste0("DepAnx", 1:4)),
#                          cluster = NULL,
#                          ordered = TRUE,
#                          long.equal = c("loadings", "thresholds"))
# 
# depanx.strict <- mi_wrapper(model = mi.mods$Depression.Anxiety,
#                          data = ds2,
#                          items = longIndNames$Depression.Anxiety,
#                          fnames = list(DepAnx = paste0("DepAnx", 1:4)),
#                          cluster = NULL,
#                          ordered = TRUE,
#                          long.equal = c("loadings", "thresholds", "residuals"))
# 
# #### compiling results across models ####
# 
# depanx.fits <- fits_wrapper(mod.list = list(depanx.config$fit, depanx.metric$fit,
#                                          depanx.scalar$fit, depanx.strict$fit),
#                          .id = "model", digits = 3) %>%
#   mutate(model = c("Configural", "Metric", "Scalar", "Strict"))
# 
# 
# depanx.mi <- bind_rows(compare_mods(depanx.config$fit, depanx.metric$fit),
#                     compare_mods(depanx.metric$fit, depanx.scalar$fit),
#                     compare_mods(depanx.scalar$fit, depanx.strict$fit)) %>%
#   mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
#   select(constraint_added, everything())

########################################################


# ----------- Peer Victimization ------------------

peervict.config <- mi_wrapper(model = mi.mods$Peer_Victimization,
                         data = ds2,
                         items = longIndNames$Peer_Victimization,
                         fnames = list(PeerVict = paste0("PeerVict", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise")

# cat(as.character(peervict.config$syntax))

peervict.metric <- mi_wrapper(model = mi.mods$Peer_Victimization,
                         data = ds2,
                         items = longIndNames$Peer_Victimization,
                         fnames = list(PeerVict = paste0("PeerVict", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings"))

peervict.scalar <- mi_wrapper(model = mi.mods$Peer_Victimization,
                         data = ds2,
                         items = longIndNames$Peer_Victimization,
                         fnames = list(PeerVict = paste0("PeerVict", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds"))

peervict.strict <- mi_wrapper(model = mi.mods$Peer_Victimization,
                         data = ds2,
                         items = longIndNames$Peer_Victimization,
                         fnames = list(PeerVict = paste0("PeerVict", 1:4)),
                         cluster = NULL,
                         ordered = TRUE,
                         missing = "listwise",
                         long.equal = c("loadings", "thresholds", "residuals"))

#### compiling results across models ####

peervict.fits <- fits_wrapper(mod.list = list(peervict.config$fit, peervict.metric$fit,
                                         peervict.scalar$fit, peervict.strict$fit),
                         .id = "model", digits = 3) %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))


peervict.mi <- bind_rows(compare_mods(peervict.config$fit, peervict.metric$fit),
                    compare_mods(peervict.metric$fit, peervict.scalar$fit),
                    compare_mods(peervict.scalar$fit, peervict.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

########################################################


# All models
save(gwb.config, gwb.metric, gwb.scalar, gwb.strict,
     shhelpseek.config, shhelpseek.metric, shhelpseek.scalar, shhelpseek.strict,
     shhelpatt.config, shhelpatt.metric, shhelpatt.scalar, shhelpatt.strict,
     staffhelp.config, staffhelp.metric, staffhelp.scalar, staffhelp.strict,
     act.config, act.metric, act.strict,
     pass.config, pass.metric, pass.strict,
     file = "Output/ProtectFac_MI_Mods.RData")



# ---- Compiling and Saving MI results ------------------

all.config <- list(act.config, pass.config,
                   ncperp.config,ncvict.config,conperp.config, convict.config,
                   hncperp.config, hncvict.config,cyber.config, dismiss.config,
                   gwb.config, shhelpatt.config, shhelpseek.config,
                   aod.config, bully.config, cyberbully.config,  peervict.config) %>% #depanx.config,
  set_names(c("Active_Exposure", "Passive_Exposure", OutcomeNames, ProtectiveNames[c(1, 3, 4)], OtherScaleNames[-4]))

all.strict <- list(act.pi.strict, pass.pi.strict,
                   ncperp.pi.strict,ncvict.strict,conperp.strict, convict.strict,
                   hncperp.pi.strict, hncvict.strict, cyber.strict, dismiss.pi.strict,
                   gwb.strict, shhelpatt.strict, shhelpseek.strict, 
                   aod.strict, bully.strict, cyberbully.strict,  peervict.strict) %>% #depanx.strict,
  set_names(c("Active_Exposure", "Passive_Exposure", OutcomeNames, ProtectiveNames[c(1, 3, 4)], OtherScaleNames[-4]))


all.mi.fits <- list(act.fits, pass.fits,
                   ncperp.fits,ncvict.fits,conperp.fits, convict.fits,
                   hncperp.fits, hncvict.fits,cyber.fits, dismiss.fits,
                   gwb.fits, shhelpatt.fits, shhelpseek.fits,
                   aod.fits, bully.fits, cyberbully.fits,  peervict.fits) %>% #depanx.fits,
  set_names(c("Active_Exposure", "Passive_Exposure", OutcomeNames, ProtectiveNames[c(1, 3, 4)], OtherScaleNames[-4]))


all.mi.comp <- list(act.mi, pass.mi,
                   ncperp.mi,ncvict.mi,conperp.mi, convict.mi,
                   hncperp.mi, hncvict.mi,cyber.mi, dismiss.mi,
                   gwb.mi, shhelpatt.mi, shhelpseek.mi,
                   aod.mi, bully.mi, cyberbully.mi,  peervict.mi) %>% #depanx.mi,
  set_names(c("Active_Exposure", "Passive_Exposure", OutcomeNames, ProtectiveNames[c(1, 3, 4)], OtherScaleNames[-4]))

## Heywood cases
heywood <- map(all.config, ~lavInspect(.x$fit, "post.check"))

table(ds2$SEX_VIOL_VICT_3_W1, ds2$SEX_VIOL_VICT_3_W4, useNA = "always")

# lavInspect(conperp.config$fit, what = "zero.cell.tables") # all bivariate tables with 0 cells


# config.syntax <- lapply(all.config, function(s) as.character(s[["syntax"]]))
# 
# semPlot::semPaths(ncperp.config$fit)

##################################################################################

# ----      Factor Scores   -------------------

# default is se = "none"; SEs can only be calculated for complete continuous data
# sh.fs2 <- as.data.frame(lavPredict(sh.strict$fit, type = "lv", method = "EBM", se = "standard"))
ae.fs <- as.data.frame(lavPredict(all.strict$Active_Exposure$fit, type = "lv", method = "EBM"))
pe.fs <- as.data.frame(lavPredict(all.strict$Passive_Exposure$fit, type = "lv", method = "EBM"))
ncp.fs <- as.data.frame(lavPredict(all.strict$No_Contact_Perpetration$fit, type = "lv", method = "EBM"))
ncv.fs <- as.data.frame(lavPredict(all.strict$No_Contact_Victimization$fit, type = "lv", method = "EBM"))
cp.fs <- as.data.frame(lavPredict(all.strict$Contact_Perpetration$fit, type = "lv", method = "EBM"))
cv.fs <- as.data.frame(lavPredict(all.strict$Contact_Victimization$fit, type = "lv", method = "EBM"))
hop.fs <- as.data.frame(lavPredict(all.strict$HNC_Perpetration$fit, type = "lv", method = "EBM"))
hov.fs <- as.data.frame(lavPredict(all.strict$HNC_Victimization$fit, type = "lv", method = "EBM"))
cyp.fs <- as.data.frame(lavPredict(all.strict$Cybersex_Perpetration$fit, type = "lv", method = "EBM"))
dsv.fs <- as.data.frame(lavPredict(all.strict$SV_Dismissiveness$fit, type = "lv", method = "EBM"))
gwb.fs <- as.data.frame(lavPredict(all.strict$General_Well.being$fit, type = "lv", method = "EBM"))
sha.fs <- as.data.frame(lavPredict(all.strict$SH_Help_Attitudes$fit, type = "lv", method = "EBM"))
shi.fs <- as.data.frame(lavPredict(all.strict$SH_Help_Seeking$fit, type = "lv", method = "EBM"))


factor.scores <- list(ae.fs, pe.fs, ncp.fs, ncv.fs, cp.fs, cv.fs, hop.fs, hov.fs, cyp.fs, dsv.fs, gwb.fs, sha.fs, shi.fs) %>%
  set_names(names(all.strict)[1:13])

## Note: if ds2 has been updated, just need to load("Output/Longitudinal_MI.RData"), run the next section and re-save
# won't work if there are not the 6465 students originally used to run the longMI
ds3 <- bind_cols(ds2,
                 factor.scores$No_Contact_Perpetration,
                 factor.scores$No_Contact_Victimization,
                 factor.scores$Contact_Perpetration,
                 factor.scores$Contact_Victimization,
                 factor.scores$HNC_Perpetration,
                 factor.scores$HNC_Victimzation,
                 factor.scores$Cybersex_Perpetration,
                 factor.scores$SV_Dismissiveness,
                 factor.scores$General_Well.being,
                 factor.scores$SH_Help_Attitudes,
                 factor.scores$SH_Help_Seeking) %>%
  left_join(bind_cols(data.frame(StudentID = ds2[ds2$Tx == 1, ]$StudentID), factor.scores$Active_Exposure,
                      factor.scores$Passive_Exposure), by = "StudentID")
# ds3 <- ds2

## Note: Active and Passive scores will be NA for control students

save(longIndNames, mi.mods, heywood,
     all.config, all.strict,
     all.mi.fits, all.mi.comp,
     factor.scores, ds3,
     file = "Output/Longitudinal_MI.RData")
# load("Output/Longitudinal_MI.RData")

# Note: does not include partial invariance models
save(act.config, act.metric, act.strict, act.fits, act.mi,
     pass.config, pass.metric, pass.strict, pass.fits, pass.mi, 
     ncperp.config, ncperp.metric, ncperp.strict, ncperp.fits, ncperp.mi,
     conperp.config, conperp.metric, conperp.strict, conperp.fits, conperp.mi,
     ncvict.config, ncvict.metric, ncvict.strict, ncvict.fits, ncvict.mi,
     convict.config, convict.metric, convict.strict, convict.fits, convict.mi,
     hncperp.config, hncperp.metric, hncperp.scalar, hncperp.strict, hncperp.fits, hncperp.mi,
     hncvict.config, hncvict.metric, hncvict.scalar, hncvict.strict, hncvict.fits, hncvict.mi,
     cyber.config, cyber.metric, cyber.scalar, cyber.strict, cyber.fits, cyber.mi,
     dismiss.config, dismiss.metric, dismiss.scalar, dismiss.strict, dismiss.fits, dismiss.mi,
     gwb.config, gwb.metric, gwb.scalar, gwb.strict, gwb.fits, gwb.mi,
     shhelpseek.config, shhelpseek.metric, shhelpseek.scalar, shhelpseek.strict, shhelpseek.fits, shhelpseek.mi,
     shhelpatt.config, shhelpatt.metric, shhelpatt.scalar, shhelpatt.strict, shhelpatt.fits, shhelpatt.mi,
     aod.config, aod.metric, aod.scalar, aod.strict, aod.fits, aod.mi,
     bully.config, bully.metric, bully.scalar, bully.strict, bully.fits, bully.mi,
     cyberbully.config, cyberbully.metric, cyberbully.scalar, cyberbully.strict, cyberbully.fits, cyberbully.mi,
     # depanx.config, depanx.metric, depanx.scalar, depanx.strict, depanx.fits, depanx.mi,
     peervict.config, peervict.metric, peervict.scalar, peervict.strict, peervict.fits, peervict.mi,
     # staffhelp.config, staffhelp.metric, staffhelp.scalar, staffhelp.strict,
     file = "Output/All_Longitudinal_MI_Models.RData")

# load("Output/All_Longitudinal_MI_Models.RData")

############################################
