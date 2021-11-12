##############################################
#                                            #
#           Paper 1 - Abuse and SH           #
#        Raw Scale Score LGM Analysis        #
#                                            #
##############################################


source("Scripts/0-PnF.R")
load("Output/MI_For_Analysis.RData")
load("Output/SS_Data_For_Analysis_wFS.RData")



#######################################
####     Latent Growth Models     #####

# lavaan::growth defaults
# meanstructure = TRUE, int.ov.free = FALSE, int.lv.free = TRUE,
# auto.fix.first = TRUE (unless std.lv = TRUE), auto.fix.single = TRUE, auto.var = TRUE,
# auto.cov.lv.x = TRUE, auto.efa = TRUE, auto.th = TRUE, auto.delta = TRUE, and auto.cov.y = TRUE

# ---- With lavaan ----------

## Syntax for latent growth model
lgm.ss.syntax <- map(.x = c("sh", "dep", "del", "schb"),
                     ~str_replace_all("Int =~ 1*xyz_scale_W1 + 1*xyz_scale_W2 + 1*xyz_scale_W3 + 1*xyz_scale_W4
Slope =~ 0*xyz_scale_W1 + .5*xyz_scale_W2 + 1.5*xyz_scale_W3 + 2.5*xyz_scale_W4",
                                      "xyz", .x)) %>%
  set_names(c("Sexual_Harassment", "Depression", 
              "Delinquency", "School_Belonging"))

# lgm.ss.syntax123 <- map(.x = c("sh", "dep", "del", "schb"),
#                      ~str_replace_all("Int =~ 1*xyz_scale_W1 + 1*xyz_scale_W2 + 1*xyz_scale_W3 + 1*xyz_scale_W4
# Slope =~ 0*xyz_scale_W1 + 1*xyz_scale_W2 + 2*xyz_scale_W3 + 3*xyz_scale_W4",
#                                       "xyz", .x)) %>%
#   set_names(c("Sexual_Harassment", "Depression", 
#               "Delinquency", "School_Belonging"))

growth_settings <- list(data = sswidewfs,
                        estimator = "MLR",
                        missing = "fiml",
                        # se = "robust.huber.white", # gets overwritten by "robust.cluster", which is an extension of huber.white
                        cluster = "SCHOOL_ID_W1",
                        meanstructure = TRUE,
                        mimic = "Mplus",
                        # verbose = TRUE,
                        fixed.x = FALSE) # FALSE brings x variables into model so missing handled by ML


#### Running Models in lavaan ####
sh.lgm.ss <- do.call("growth", c(list(model = lgm.ss.syntax$Sexual_Harassment), growth_settings))
dep.lgm.ss <- do.call("growth", c(list(model = lgm.ss.syntax$Depression), growth_settings))
del.lgm.ss <- do.call("growth", c(list(model = lgm.ss.syntax$Delinquency), growth_settings))
# del.lgm.ss123 <- do.call("growth", c(list(model = lgm.ss.syntax123$Delinquency), growth_settings))


# Compiling into a single object
lgm.ss.all <- list(sh.lgm.ss, dep.lgm.ss, del.lgm.ss) #schb.lgm.ss

# Model fit
lgm.ss.fit <- fits_wrapper(lgm.ss.all, .id = "Outcome", measures = "scaled") %>%
  mutate(Outcome = c("Sexual Harassment", "Depression", "Delinquency"))


## Intercept and Slope estimates - returns dataframe with row for each model
isparam.levs <- c("Int", "Slope", "Int_Int", "Slope_Slope", "Int_Slope") 
lgm.ss.est <- map2_dfr(.x = lgm.ss.all, .y = c("Sexual_Harassment", "Depression", "Delinquency"),
                       ~extract_lavaan_parameters(.x, std = "no", params = c("~~","~1")) %>%
                         filter(lhs %in% c("Int", "Slope")) %>%
                         mutate(Outcome = .y)) %>%
  get_est() %>%
  select(Outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(Outcome, all_of(isparam.levs))

#### Compiling results objects into one object ####
lgm.ss.results <- list(Models = lgm.ss.all,
                       Fit = lgm.ss.fit,
                       Estimates = lgm.ss.est)

#########################################################


# -------   With Mplus  -------------------------
# in alphabetical order because that is how readModels organizes them
mplusoutcomes = c("Delinquency", "Depression", "Sexual Harassment")
mpluslgmpred <- c(I = "Intercept", S = "Intercept")

## Importing female models and converting results to tables
lgm.ss.mplus <- MplusAutomation::readModels(target = "mplus files/Latent Growth")
lgm.ss.mplus.fit <- map_dfr(lgm.ss.mplus, ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  mutate(outcome = mplusoutcomes)
lgm.ss.mplus.est <- map2_dfr(lgm.ss.mplus, mplusoutcomes, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized") %>%
                               mutate(outcome = .y)) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)

#### Compiling results objects into one object ####
lgm.ss.mplus.results <- list(Models = lgm.ss.mplus,
                             Fit = lgm.ss.mplus.fit,
                             Estimates = lgm.ss.mplus.est,
                             R2 = map2_dfr(.x = lgm.ss.mplus, .y = mplusoutcomes,
                                           ~.x$parameters$r2 %>% mutate(outcome = .y)),
                             Warn_Error = map2_dfr(.x = lgm.ss.mplus, .y = mplusoutcomes,
                                                   ~data.frame(outcome = .y,
                                                               errors = length(.x$errors),
                                                               warnings = length(.x$warnings))))
## Conclusions:
# Mplus and lavaan estimates match; Mplus uses scaled fit statistics, lavaan can report scaled or robust; either way, fit is inadequate for delinquency
lgm.ss.mplus.results$Fit
lgm.ss.results$Fit
###########################################################################

##########################################################
####            Multigroup Analyses                   ####

# -------- Do Models fit for each group? ----------------

#### Running models in lavaan ####
group.names <- c("Female", "Male", "Black", "Hispanic", "White", "OtherR", "High Belonging", "Moderate Belonging")
lgm.ss.strat <- map(.x = lgm.ss.syntax[-4],
                    ~list(Female = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("Female") == 1)), growth_settings[-1])),
                          Male = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("Female") == 0)), growth_settings[-1])),
                          Black = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("Race4") == "Black")), growth_settings[-1])),
                          Hispanic = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("Race4") == "Hispanic")), growth_settings[-1])),
                          White = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("Race4") == "White")), growth_settings[-1])),
                          OtherR = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("Race4") == "Other")), growth_settings[-1])),
                          High_Belong = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("High_Belong") == 1)), growth_settings[-1])),
                          Moderate_Belong = do.call("growth", c(list(model = .x, data = filter(sswidewfs, !!as.symbol("High_Belong") == 0)), growth_settings[-1]))))

## Model Fit
lgm.ss.strat.fit <- map(lgm.ss.strat, ~fits_wrapper(.x, .id = "Group", measures = "scaled") %>%
                          mutate(Group = group.names))
## Parameter estimates
lgm.ss.strat.est <- map(.x = lgm.ss.strat, ~map2_dfr(.x = .x,
                                                     .y = group.names,
                                                     ~extract_lavaan_parameters(.x, std = "no", params = c("~~","~1")) %>%
                                                       filter(lhs %in% c("Int", "Slope")) %>%
                                                       mutate(Group = .y)) %>%
                          get_est() %>%
                          select(Group, estimate, parameter) %>%
                          spread(parameter, estimate) %>%
                          select(Group, all_of(isparam.levs)) %>%
                          mutate(Group = factor(Group, levels = group.names)) %>%
                          arrange(Group))

## Compiling results objects into one object
lgm.ss.strat.results <- list(Models = lgm.ss.strat,
                             Fit = lgm.ss.strat.fit,
                             Estimates = lgm.ss.strat.est)

#### Running Models in Mplus ####
## Currently only for confirming lavaan results as Not all models have been set up and run in Mplus
lgm.ss.strat.mplus <- MplusAutomation::readModels(target = "mplus files/Latent Growth/Stratified")

lapply(lgm.ss.strat.mplus, "[[", "errors")

# gathering results in a single data.frame
df.strat.mplus.fit <- map_dfr(lgm.ss.strat.mplus, ~mplus_fit(.x, digits = 2), .id = "temp") %>%
  separate(temp, into = c("Outcome", "model", "Group", "out"), sep = "\\.") %>%
  select(-c(model, out))
df.strat.mplus.est <- map_dfr(lgm.ss.strat.mplus, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized"), .id = "temp") %>%
  separate(temp, into = c("Outcome", "model", "Group", "out"), sep = "\\.") %>%
  select(Outcome, Group, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)

# Organizing results into a list for each outcome
lgm.ss.strat.mplus.fit <- map(.x = c("delinquency", "depression", "sh"), ~filter(df.strat.mplus.fit, Outcome == .x)) %>%
  set_names("Delinquency", "Depression", "Sexual_Harassment")
lgm.ss.strat.mplus.est <- map(.x = c("delinquency", "depression", "sh"), ~filter(df.strat.mplus.est, Outcome == .x)) %>%
  set_names("Delinquency", "Depression", "Sexual_Harassment")

## Compiling results objects into one object
lgm.ss.strat.mplus.results <- list(Models = lgm.ss.strat.mplus,
                             Fit = lgm.ss.strat.mplus.fit,
                             Estimates = lgm.ss.strat.mplus.est)


## Comparison
# Fit
lgm.ss.strat.mplus.results$Fit
lgm.ss.strat.results$Fit
# Estimates
lgm.ss.strat.mplus.results$Estimates
lgm.ss.strat.results$Estimates

## Conclusions:
# Fit and Estimates match
# Slope variance for High Belonging group constrained to 0 in Mplus after preliminary analysis in lavaan
################################################################


# -------- By Gender with lavaan  ---------------

## Descriptive Statistics
gen.ss.descrips <- sswidewfs %>%
  group_by(Gender) %>%
  skimr::skim(matches("del_scale|dep_scale|sh_scale|schb_scale")) %>%
  filter(!is.na(Gender))

# scale score density plot by wave
gen.ss.distrib <- cond_dist_plot(sslong, Gender)

## Running freely estimated and constrained models
gen.ss.free <- map(.x = lgm.ss.syntax[-4],
                   ~do.call("growth", c(list(model = .x, group = "Female"), growth_settings)))
gen.ss.equal <- map(.x = lgm.ss.syntax[-4],
                    ~do.call("growth", c(list(model = .x, group = "Female",
                                              group.equal = c("means", "lv.variances", "lv.covariances", "residuals")), growth_settings)))

# summary(gen.ss.equal$Depression, fit.measures = TRUE)


# in case models need to be run individually
# sh.gen.ss.free <- do.call("growth", c(list(model = lgm.ss.syntax$Sexual_Harassment, group = "Female"), growth_settings))
# dep.gen.ss.free <- do.call("growth", c(list(model = lgm.ss.syntax$Depression, group = "Female"), growth_settings))
# del.gen.ss.free <- do.call("growth", c(list(model = lgm.ss.syntax$Delinquency, group = "Female"), growth_settings))
# sh.gen.ss.equal <- do.call("growth", c(list(model = lgm.ss.syntax$Sexual_Harassment, group = "Female",
#                                             group.equal = c("means", "lv.variances", "lv.covariances", "residuals")), growth_settings))

#### Model Fit and Comparisons ####
## Fit
gen.ss.fits <- map(1:3, ~fits_wrapper(mod.list = list(gen.ss.free[[.x]], gen.ss.equal[[.x]]),
                                      .id = "model", measures = "scaled") %>%
                     mutate(model = c("Free", "Equal"))) %>%
  set_names(names(gen.ss.free))

## Comparison
gen.ss.comp <- map2(.x = gen.ss.free, .y = gen.ss.equal,
                    ~bind_rows(data.frame(delta.chisq = NA, delta.df = NA, delta.pvalue = NA,
                                          delta.cfi = NA, delta.rmsea = NA, delta.srmr = NA),
                               compare_mods(.x, .y)))
## Combined into single table
gen.ss.fitcomp <- map2_dfr(gen.ss.fits, gen.ss.comp, ~bind_cols(.x, .y), .id = "Outcome")


#### Parameter Estimates ####

## Intercept and Slope estimates for each group
gen.ss.est <- map(.x = gen.ss.free,
                  ~extract_lavaan_parameters(.x, std = "no", params = c("~~","~1")) %>%
                    filter(lhs %in% c("Int", "Slope")) %>%
                    mutate(group = recode(group, `1` = "Male", `2` = "Female"),
                           parameter = paste(lhs, op, rhs) %>%
                             str_replace_all(., " ~~ ", "_") %>%
                             str_replace_all(., " ~1 ", "")) %>%
                    arrange(op, parameter))

## Parameter estimate output table
gen.ss.est.table <- map_dfr(.x = gen.ss.est, ~.x %>%
                              mutate(sig = ifelse(is.na(pvalue), " ", ifelse(pvalue < .05, "*", " "))) %>%
                              mutate(across(where(is.numeric), ~format(round(., 2), nsmall = 2))) %>%
                              mutate(estimate = paste0(est, " (", se, ")", sig)) %>%
                              select(group, estimate, parameter), .id = "Outcome") %>%
  spread(parameter, estimate) %>%
  select(Outcome, group, all_of(isparam.levs))

## Predicted values at each wave by group
gen.ss.predict <- map_dfr(gen.ss.est, ~predict_growth(.x), .id = "Outcome") %>%
  mutate(Outcome = str_replace(Outcome, "_", " "),
         Wave = str_remove(Wave, "Wave") %>% as.numeric())

# Plot
gen.ss.plot <- multigroup_plot(gen.ss.predict) +
  facet_wrap(~Outcome)

#### Compiling results objects into one object ####
gen.ss.results <- list(Descriptives = gen.ss.descrips,
                       Distribution_Plot = gen.ss.distrib,
                       Free_Models = gen.ss.free,
                       Equal_Models = gen.ss.equal,
                       Model_Comparison = gen.ss.fitcomp,
                       All_Estimates = gen.ss.est,
                       Estimate_Table = gen.ss.est.table,
                       Plot_Data = gen.ss.predict,
                       Estimate_Plot = gen.ss.plot)

#######################################################################

# -------- By Gender with Mplus  ---------------

#### Running Models ####
## Importing Models
gen.ss.mplus <- MplusAutomation::readModels(target = "mplus files/Multigroup/Gender")

## Model Fit and Comparisons
gen.ss.mplus.fit <- map_dfr(gen.ss.mplus, ~mplus_fit(.x, digits = 2), .id = "temp") %>%
  mutate(outcome = rep(mplusoutcomes, each = 2),
         model = rep(c("Equal", "Free"), times = 3) %>% factor(levels = c("Free", "Equal")))

gen.ss.mplus.comp <- map2_dfr(gen.ss.mplus[c(1, 3, 5)], gen.ss.mplus[c(2, 4, 6)], ~mplus_sb2001(.x, .y)) %>% 
  mutate(temp = names(gen.ss.mplus)[c(1,3,5)])

gen.ss.mplus.fitcomp <- left_join(gen.ss.mplus.fit, gen.ss.mplus.comp, by = "temp") %>%
  select(outcome, model, everything(), -temp) %>%
  arrange(outcome, model)

## Parameter Estimates
gen.ss.mplus.est <- map_dfr(gen.ss.mplus, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized", ci = TRUE), .id = "long") %>%
  mutate(Group = str_to_title(Group),
         outcome = case_when(str_detect(long, "delinquency") ~ "Delinquency",
                             str_detect(long, "depression") ~ "Depression",
                             str_detect(long, "sexual.harassment") ~ "Sexual Harassment"))

gen.ss.mplus.est.table <- gen.ss.mplus.est %>%
  filter(str_detect(long, "free")) %>%
  select(outcome, Group, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)


## Predicted values at each wave by group
gen.ss.mplus.predict <- gen.ss.mplus.est %>%
  filter(str_detect(long, "free")) %>%
  predict_growth(lavaan = FALSE) %>%
  mutate(Wave = str_remove(Wave, "Wave") %>% as.numeric())

# Plot
gen.ss.mplus.plot <- multigroup_plot(gen.ss.mplus.predict) +
  facet_wrap(~outcome)

#### Compiling results objects into one object ####
gen.ss.mplus.results <- list(Descriptives = gen.ss.descrips,
                             Distribution_Plot = gen.ss.distrib,
                             Models = gen.ss.mplus,
                             Model_Comparison = gen.ss.mplus.fitcomp,
                             All_Estimates = gen.ss.mplus.est,
                             Estimate_Table = gen.ss.mplus.est.table,
                             Plot_Data = gen.ss.mplus.predict,
                             Estimate_Plot = gen.ss.mplus.plot,
                             Warn_Error = map_dfr(.x = names(gen.ss.mplus),
                                                  ~data.frame(outcome = .x,
                                                              errors = length(gen.ss.mplus[[.x]]$errors),
                                                              warnings = length(gen.ss.mplus[[.x]]$warnings))))

## Comparison
# gen.ss.mplus.results$Model_Comparison
# gen.ss.results$Model_Comparison
# 
# gen.ss.mplus.results$Estimate_Table
# gen.ss.results$Estimate_Table


## Conclusion
# Mplus and lavaan parameter estimates match;
# fit for free models match though equal models have some differences, regardless fit indicates group differences
# fit of free delinquency model is suspect


######################################################


# -------- By Race with lavaan ---------------

## Descriptive Statistics
race.ss.descrips <- sswidewfs %>%
  group_by(Race4) %>%
  skimr::skim(matches("del_scale|dep_scale|sh_scale|schb_scale")) %>%
  filter(!is.na(Race4)) # only 1 case missing

# scale score density plot by wave
race.ss.distrib <- cond_dist_plot(sslong, Race4)

## Running freely estimated and constrained models
race.ss.free <- map(.x = lgm.ss.syntax[-4],
                   ~do.call("growth", c(list(model = .x, group = "Race4"), growth_settings)))
race.ss.equal <- map(.x = lgm.ss.syntax[-4],
                    ~do.call("growth", c(list(model = .x, group = "Race4",
                                              group.equal = c("means", "lv.variances", "lv.covariances", "residuals")), growth_settings)))

summary(race.ss.free$Sexual_Harassment, fit.measures = TRUE, standardized = TRUE) # White negative variance for SH slope and Wave 4 delinquency

#### Model Fit and Comparisons ####
## Fit
race.ss.fits <- map(1:3, ~fits_wrapper(mod.list = list(race.ss.free[[.x]], race.ss.equal[[.x]]),
                                      .id = "model", measures = "scaled") %>%
                     mutate(model = c("Free", "Equal"))) %>%
  set_names(names(race.ss.free))
## Comparison
race.ss.comp <- map2(.x = race.ss.free, .y = race.ss.equal,
                    ~bind_rows(data.frame(delta.chisq = NA, delta.df = NA, delta.pvalue = NA,
                                          delta.cfi = NA, delta.rmsea = NA, delta.srmr = NA),
                               compare_mods(.x, .y)))
## Combined into single table
race.ss.fitcomp <- map2_dfr(race.ss.fits, race.ss.comp, ~bind_cols(.x, .y), .id = "Outcome")


#### Parameter Estimates ####

## Intercept and Slope estimates for each group
race.ss.est <- map(.x = race.ss.free,
                  ~extract_lavaan_parameters(.x, std = "no", params = c("~~","~1")) %>%
                    filter(lhs %in% c("Int", "Slope")) %>%
                    mutate(group = recode(group, `1` = "Black", `4` = "Hispanic", `2` = "White", `3` = "Other"),
                           parameter = paste(lhs, op, rhs) %>%
                             str_replace_all(., " ~~ ", "_") %>%
                             str_replace_all(., " ~1 ", "")) %>%
                    arrange(op, parameter))

## Parameter estimate output table
race.ss.est.table <- map_dfr(.x = race.ss.est, ~.x %>%
                               mutate(sig = ifelse(is.na(pvalue), " ", ifelse(pvalue < .05, "*", " "))) %>%
                               mutate(across(where(is.numeric), ~format(round(., 2), nsmall = 2))) %>%
                               mutate(estimate = paste0(est, " (", se, ")", sig)) %>%
                              select(group, estimate, parameter), .id = "Outcome") %>%
  spread(parameter, estimate) %>%
  select(Outcome, group, all_of(isparam.levs))

## Predicted values at each wave by group
race.ss.predict <- map_dfr(race.ss.est, ~predict_growth(.x), .id = "Outcome") %>%
  mutate(Outcome = str_replace(Outcome, "_", " "),
         Wave = str_remove(Wave, "Wave") %>% as.numeric())

# Plot
race.ss.plot <- multigroup_plot(race.ss.predict) +
  facet_wrap(~Outcome)

#### Compiling results objects into one object ####
race.ss.results <- list(Descriptives = race.ss.descrips,
                        Distribution_Plot = race.ss.distrib,
                        Free_Models = race.ss.free,
                        Equal_Models = race.ss.equal,
                        Model_Comparison = race.ss.fitcomp,
                        All_Estimates = race.ss.est,
                        Estimate_Table = race.ss.est.table,
                        Plot_Data = race.ss.predict,
                        Estimate_Plot = race.ss.plot)

##########################################################################

# -------- By Race with Mplus ---------------

#### Running Models ####
## Importing Models
race.ss.mplus <- MplusAutomation::readModels(target = "mplus files/Multigroup/Race")

## Model Fit and Comparisons
race.ss.mplus.fit <- map_dfr(race.ss.mplus, ~mplus_fit(.x, digits = 2), .id = "temp") %>%
  mutate(outcome = rep(mplusoutcomes, each = 2),
         model = rep(c("Equal", "Free"), times = 3) %>% factor(levels = c("Free", "Equal")))

race.ss.mplus.comp <- map2_dfr(race.ss.mplus[c(1, 3, 5)], race.ss.mplus[c(2, 4, 6)], ~mplus_sb2001(.x, .y)) %>% 
  mutate(temp = names(race.ss.mplus)[c(1,3,5)])

race.ss.mplus.fitcomp <- left_join(race.ss.mplus.fit, race.ss.mplus.comp, by = "temp") %>%
  select(outcome, model, everything(), -temp) %>%
  arrange(outcome, model)

## Parameter Estimates
race.ss.mplus.est <- map_dfr(race.ss.mplus, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized", ci = TRUE), .id = "long") %>%
  mutate(Group = str_to_title(Group),
         outcome = case_when(str_detect(long, "delinquency") ~ "Delinquency",
                             str_detect(long, "depression") ~ "Depression",
                             str_detect(long, "sexual.harassment") ~ "Sexual Harassment"))

race.ss.mplus.est.table <- race.ss.mplus.est %>%
  filter(str_detect(long, "free")) %>%
  select(outcome, Group, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)


## Predicted values at each wave by group
race.ss.mplus.predict <- race.ss.mplus.est %>%
  filter(str_detect(long, "free")) %>%
  predict_growth(lavaan = FALSE) %>%
  mutate(Wave = str_remove(Wave, "Wave") %>% as.numeric())

# Plot
race.ss.mplus.plot <- multigroup_plot(race.ss.mplus.predict) +
  facet_wrap(~outcome)

#### Compiling results objects into one object ####
race.ss.mplus.results <- list(Descriptives = race.ss.descrips,
                              Distribution_Plot = race.ss.distrib,
                              Models = race.ss.mplus,
                              Model_Comparison = race.ss.mplus.fitcomp,
                              All_Estimates = race.ss.mplus.est,
                              Estimate_Table = race.ss.mplus.est.table,
                              Plot_Data = race.ss.mplus.predict,
                              Estimate_Plot = race.ss.mplus.plot,
                              Warn_Error = map_dfr(.x = names(race.ss.mplus),
                                                   ~data.frame(outcome = .x,
                                                               errors = length(race.ss.mplus[[.x]]$errors),
                                                               warnings = length(race.ss.mplus[[.x]]$warnings))))

# ## Comparison
# race.ss.mplus.results$Model_Comparison
# race.ss.results$Model_Comparison
# 
# race.ss.mplus.results$Estimate_Table
# race.ss.results$Estimate_Table

## Conclusion
# Mplus and lavaan parameter estimates match;
# fit for free models match though equal models have some differences, regardless fit indicates group differences
# fit of free delinquency model is suspect

######################################################

# -------- By School Belonging with lavaan ---------------

## Descriptive Statistics
bel.ss.descrips <- sswidewfs %>%
  group_by(High_Belong) %>%
  skimr::skim(matches("del_scale|dep_scale|sh_scale")) %>%
  filter(!is.na(High_Belong))

# scale score density plot by wave
bel.ss.distrib <- cond_dist_plot(sslong, High_Belong)

## Running freely estimated and constrained models
bel.ss.free <- map(.x = lgm.ss.syntax[-4],
                    ~do.call("growth", c(list(model = .x, group = "High_Belong"), growth_settings)))
bel.ss.equal <- map(.x = lgm.ss.syntax[-4],
                     ~do.call("growth", c(list(model = .x, group = "High_Belong",
                                               group.equal = c("means", "lv.variances", "lv.covariances", "residuals")), growth_settings)))

summary(bel.ss.free$Sexual_Harassment, fit.measures = TRUE)
# slightly negative slope variance for High Belonging group which I take to mean as little to no variance - yes, this will be constrained to 0 in Mplus

#### Model Fit and Comparisons ####
## Fit
bel.ss.fits <- map(1:3, ~fits_wrapper(mod.list = list(bel.ss.free[[.x]], bel.ss.equal[[.x]]),
                                       .id = "model", measures = "scaled") %>%
                      mutate(model = c("Free", "Equal"))) %>%
  set_names(names(bel.ss.free))
## Comparison
bel.ss.comp <- map2(.x = bel.ss.free, .y = bel.ss.equal,
                     ~bind_rows(data.frame(delta.chisq = NA, delta.df = NA, delta.pvalue = NA,
                                           delta.cfi = NA, delta.rmsea = NA, delta.srmr = NA),
                                compare_mods(.x, .y)))
## Combined into single table
bel.ss.fitcomp <- map2_dfr(bel.ss.fits, bel.ss.comp, ~bind_cols(.x, .y), .id = "Outcome")


#### Parameter Estimates ####

## Intercept and Slope estimates for each group
bel.ss.est <- map(.x = bel.ss.free,
                   ~extract_lavaan_parameters(.x, std = "no", params = c("~~","~1")) %>%
                     filter(lhs %in% c("Int", "Slope")) %>%
                     mutate(group = recode(group, `1` = "Moderate Belong", `2` = "High Belong"),
                            parameter = paste(lhs, op, rhs) %>%
                              str_replace_all(., " ~~ ", "_") %>%
                              str_replace_all(., " ~1 ", "")) %>%
                     arrange(op, parameter))

## Parameter estimate output table
bel.ss.est.table <- map_dfr(.x = bel.ss.est, ~.x %>%
                              mutate(sig = ifelse(is.na(pvalue), " ", ifelse(pvalue < .05, "*", " "))) %>%
                              mutate(across(where(is.numeric), ~format(round(., 2), nsmall = 2))) %>%
                              mutate(estimate = paste0(est, " (", se, ")", sig)) %>%
                               select(group, estimate, parameter), .id = "Outcome") %>%
  spread(parameter, estimate) %>%
  select(Outcome, group, all_of(isparam.levs))

## Predicted values at each wave by group
bel.ss.predict <- map_dfr(bel.ss.est, ~predict_growth(.x), .id = "Outcome") %>%
  mutate(Outcome = str_replace(Outcome, "_", " "),
         Wave = str_remove(Wave, "Wave") %>% as.numeric())

# Plot
bel.ss.plot <- multigroup_plot(bel.ss.predict) +
  facet_wrap(~outcome)

#### Compiling results objects into one object ####
bel.ss.results <- list(Descriptives = bel.ss.descrips,
                       Distribution_Plot = bel.ss.distrib,
                       Free_Models = bel.ss.free,
                       Equal_Models = bel.ss.equal,
                       Model_Comparison = bel.ss.fitcomp,
                       All_Estimates = bel.ss.est,
                       Estimate_Table = bel.ss.est.table,
                       Plot_Data = bel.ss.predict,
                       Estimate_Plot = bel.ss.plot)

############################################################################

# -------- By School Belonging with Mplus ---------------

#### Running Models ####
## Importing Models
bel.ss.mplus <- MplusAutomation::readModels(target = "mplus files/Multigroup/School Belonging")

## Model Fit and Comparisons
bel.ss.mplus.fit <- map_dfr(bel.ss.mplus, ~mplus_fit(.x, digits = 2), .id = "temp") %>%
  mutate(outcome = rep(mplusoutcomes, each = 2),
         model = rep(c("Equal", "Free"), times = 3) %>% factor(levels = c("Free", "Equal")))

bel.ss.mplus.comp <- map2_dfr(bel.ss.mplus[c(1, 3, 5)], bel.ss.mplus[c(2, 4, 6)], ~mplus_sb2001(.x, .y)) %>% 
  mutate(temp = names(bel.ss.mplus)[c(1,3,5)])

bel.ss.mplus.fitcomp <- left_join(bel.ss.mplus.fit, bel.ss.mplus.comp, by = "temp") %>%
  select(outcome, model, everything(), -temp) %>%
  arrange(outcome, model)

## Parameter Estimates
bel.ss.mplus.est <- map_dfr(bel.ss.mplus, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized", ci = TRUE), .id = "long") %>%
  mutate(Group = str_to_title(Group),
         outcome = case_when(str_detect(long, "delinquency") ~ "Delinquency",
                             str_detect(long, "depression") ~ "Depression",
                             str_detect(long, "sexual.harassment") ~ "Sexual Harassment"))

bel.ss.mplus.est.table <- bel.ss.mplus.est %>%
  filter(str_detect(long, "free")) %>%
  select(outcome, Group, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)


## Predicted values at each wave by group
bel.ss.mplus.predict <- bel.ss.mplus.est %>%
  filter(str_detect(long, "free")) %>%
  predict_growth(lavaan = FALSE) %>%
  mutate(Wave = str_remove(Wave, "Wave") %>% as.numeric())

# Plot
bel.ss.mplus.plot <- multigroup_plot(bel.ss.mplus.predict) +
  facet_wrap(~outcome)


#### Compiling results objects into one object ####
bel.ss.mplus.results <- list(Descriptives = bel.ss.descrips,
                             Distribution_Plot = bel.ss.distrib,
                             Models = bel.ss.mplus,
                             Model_Comparison = bel.ss.mplus.fitcomp,
                             All_Estimates = bel.ss.mplus.est,
                             Estimate_Table = bel.ss.mplus.est.table,
                             Plot_Data = bel.ss.mplus.predict,
                             Estimate_Plot = bel.ss.mplus.plot,
                             Warn_Error = map_dfr(.x = names(bel.ss.mplus),
                                                  ~data.frame(outcome = .x,
                                                              errors = length(bel.ss.mplus[[.x]]$errors),
                                                              warnings = length(bel.ss.mplus[[.x]]$warnings))))

## Comparison
# bel.ss.mplus.results$Model_Comparison # Slope variance of High Belong is constrained to 0
# bel.ss.results$Model_Comparison
# 
# bel.ss.mplus.results$Estimate_Table
# bel.ss.results$Estimate_Table

## Conclusion
# Mplus and lavaan parameter estimates match;
# fit for free models match though equal models have some differences, regardless fit indicates group differences
# fit of free delinquency model is suspect


#########################################################

###############################################
#####   W1 Predictors of Growth in Mplus   ####

# ------  Full Sample -------------------

## Importing full models
full.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Wave 1 Predictors")

#### No Interactions ####
## Renaming predictors for output tables
# binarypreictors <- c("Black", "White", "Other Race", "High Sch Belong")
w1pred <- c(I = "Intercept term", S = "Intercept term", FAMCON = "Family Conflict", ABUSE = "Abuse", SIBAGG = "Sibling Aggression",
            EMP_W1 = "Lack of Empathy", FEMALE = "Female", BLACK = "Black", WHITE = "White", OTHERR = "Other Race",
            SCHBEL = "High School Belonging", GRADES = "Grades")

## Converting results to tables
full.ss.w1p.results <- format_mplus(full.ss.w1p[c(2,4,6)], recodes = w1pred, outcomes = mplusoutcomes,
                                    std = "stdy.standardized", reg = TRUE, r2 = TRUE)

## Combining parameter estimates into single table
full.ss.w1p.est <- inner_join(full.ss.w1p.results$Estimates$`Sexual Harassment`, full.ss.w1p.results$Estimates$Delinquency,
                              by = "predictor", suffix = c(".sh", ".del")) %>%
  inner_join(full.ss.w1p.results$Estimates$Depression)

#### With Interactions ####
## Adding interactions to variable recodes for output tables
w1pred.int <- c(w1pred, SBXFC = "FC x HSB", SBXAB = "Abuse x HSB", SBXSA = "SA x HSB",
                AGXFC = "FC x AG", AGXAB = "Abuse x AG", AGXSA = "SA x AG")


full.ss.w1p.int.results <- format_mplus(full.ss.w1p[c(1,3,5)], recodes = w1pred.int, outcomes = mplusoutcomes,
                                    std = "stdy.standardized", reg = TRUE, r2 = TRUE)

## Combining parameter estimates into single table
full.ss.w1p.int.est <- inner_join(full.ss.w1p.int.results$Estimates$`Sexual Harassment`, full.ss.w1p.int.results$Estimates$Delinquency,
                              by = "predictor", suffix = c(".sh", ".del")) %>%
  inner_join(full.ss.w1p.int.results$Estimates$Depression)

# full.ss.w1p.int.results$Model_Fit
# full.ss.w1p.results$Model_Fit
###############################################


# ------  Gender -------------------

## Importing models
female.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Female")
male.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Male")

#### No Interactions ####
## Renaming predictors for output tables
gen.w1pred <- c(I = "Intercept term", S = "Intercept term", FAMCON = "Family Conflict (FC)", ABUSE = "Abuse", SIBAGG = "Sibling Aggression (SA)",
                EMP_W1 = "Lack of Empathy", BLACK = "Black", WHITE = "White", OTHERR = "Other Race",
                SCHBEL = "High School Belonging (HSB)", GRADES = "Academic Grades")


## Converting results to tables
female.ss.w1p.results <- format_mplus(female.ss.w1p[c(1,3,5)], recodes = gen.w1pred, outcomes = mplusoutcomes,
                                      std = "stdy.standardized", reg = TRUE, r2 = TRUE)
male.ss.w1p.results <- format_mplus(male.ss.w1p[c(1,3,5)], recodes = gen.w1pred, outcomes = mplusoutcomes,
                                    std = "stdy.standardized", reg = TRUE, r2 = TRUE)
  

## Joining Female and Male Tables
# Fit
gen.ss.w1p.fit <- female.ss.w1p.results$Model_Fit %>% mutate(Group = "Female") %>%
  bind_rows(male.ss.w1p.results$Model_Fit %>% mutate(Group = "Male")) %>%
  select(Group, everything())
# Estimates
gen.ss.w1p.est.temp <- map2(.x = female.ss.w1p.results$Estimates,
                       .y = male.ss.w1p.results$Estimates,
                       ~inner_join(.x, .y, by = "predictor", suffix = c(".female", ".male")))
gen.ss.w1p.est <- inner_join(gen.ss.w1p.est.temp$`Sexual Harassment`, gen.ss.w1p.est.temp$Delinquency,
                         by = "predictor", suffix = c(".sh", ".del")) %>%
  inner_join(., gen.ss.w1p.est.temp$Depression, by = "predictor")

#### With Interactions ####
## Adding interactions to variable recodes for output tables
gen.w1pred.int <- c(gen.w1pred, SBXFC = "FC x HSB", SBXAB = "Abuse x HSB", SBXSA = "SA x HSB",
                    AGXFC = "FC x AG", AGXAB = "Abuse x AG", AGXSA = "SA x AG")

## Converting results to tables
female.ss.w1p.int.results <- format_mplus(female.ss.w1p[c(2,4,6)], recodes = gen.w1pred.int, outcomes = mplusoutcomes,
                                      std = "stdy.standardized", reg = TRUE, r2 = TRUE)
male.ss.w1p.int.results <- format_mplus(male.ss.w1p[c(2,4,6)], recodes = gen.w1pred.int, outcomes = mplusoutcomes,
                                    std = "stdy.standardized", reg = TRUE, r2 = TRUE)


## Joining Female and Male Tables
# Fit
gen.ss.w1p.int.fit <- female.ss.w1p.int.results$Model_Fit %>% mutate(Group = "Female") %>%
  bind_rows(male.ss.w1p.int.results$Model_Fit %>% mutate(Group = "Male")) %>%
  select(Group, everything())
# Estimates
gen.ss.w1p.int.est.temp <- map2(.x = female.ss.w1p.int.results$Estimates,
                       .y = male.ss.w1p.int.results$Estimates,
                       ~inner_join(.x, .y, by = "predictor", suffix = c(".female", ".male")))
gen.ss.w1p.int.est <- inner_join(gen.ss.w1p.int.est.temp$`Sexual Harassment`, gen.ss.w1p.int.est.temp$Delinquency,
                         by = "predictor", suffix = c(".sh", ".del")) %>%
  inner_join(., gen.ss.w1p.int.est.temp$Depression, by = "predictor")


#############################################################################

# ------  Race  -------------------

## Importing models
black.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Black")
white.ss.w1p <- MplusAutomation::readModels(target = "mplus files/White")
hispanic.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Hispanic")
# otherr.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Other Race")

#### No Interactions ####

## Renaming predictors for output tables
race.w1pred <- c(I = "Intercept term", S = "Intercept term", FAMCON = "Family Conflict (FC)", ABUSE = "Abuse", SIBAGG = "Sibling Aggression (SA)",
                      EMP_W1 = "Lack of Empathy", FEMALE = "Female", SCHBEL = "High School Belonging (HSB)", GRADES = "Academic Grades")

## Converting results to tables
black.ss.w1p.results <- format_mplus(black.ss.w1p[c(1,3,5)], recodes = race.w1pred, outcomes = mplusoutcomes,
                                     std = "stdy.standardized", reg = TRUE, r2 = TRUE)
white.ss.w1p.results <- format_mplus(white.ss.w1p[c(1,3,5)], recodes = race.w1pred, outcomes = mplusoutcomes,
                                     std = "stdy.standardized", reg = TRUE, r2 = TRUE)
hispanic.ss.w1p.results <- format_mplus(hispanic.ss.w1p[c(1,3,5)], recodes = race.w1pred, outcomes = mplusoutcomes,
                                        std = "stdy.standardized", reg = TRUE, r2 = TRUE)
# otherr.ss.w1p.results <- format_mplus(otherr.ss.w1p[-3], recodes = race.w1pred, outcomes = mplusoutcomes[-3])


## Joining Race Tables
# Fit
race.ss.w1p.fit <- black.ss.w1p.results$Model_Fit %>% mutate(Group = "Black") %>%
  bind_rows(white.ss.w1p.results$Model_Fit %>% mutate(Group = "White")) %>%
  bind_rows(hispanic.ss.w1p.results$Model_Fit %>% mutate(Group = "Hispanic")) %>%
  select(Group, everything())

# Estimates
race.ss.w1p.est.temp <- map2(.x = black.ss.w1p.results$Estimates,
                       .y = white.ss.w1p.results$Estimates,
                       ~inner_join(.x, .y, by = "predictor", suffix = c(".black", ".white")))
  
# race.ss.w1p.est2 <- map2(.x = white.ss.w1p.results$Estimates[-3],
#                          .y = otherr.ss.w1p.results$Estimates,
#                          ~left_join(.x, .y, by = "predictor", suffix = c(".white", ".otherr")))
# race.ss.w1p.est2$`Sexual Harassment` <- rename(white.ss.w1p.results$Estimates$Depression, I.white = "I", S.white = "S")


race.ss.w1p.est <- left_join(race.ss.w1p.est.temp$`Sexual Harassment`,
                         rename(hispanic.ss.w1p.results$Estimates$`Sexual Harassment`, I.hispanic = "I", S.hispanic = "S"), by = "predictor") %>%
  left_join(inner_join(race.ss.w1p.est.temp$Delinquency, rename(hispanic.ss.w1p.results$Estimates$Delinquency, I.hispanic = "I", S.hispanic = "S"),  by = "predictor"),
            by = "predictor", suffix = c(".sh", ".del")) %>%
  left_join(inner_join(race.ss.w1p.est.temp$Depression, rename(hispanic.ss.w1p.results$Estimates$Depression, I.hispanic.dep = "I", S.hispanic.dep = "S"), by = "predictor"),
            by = "predictor")

#### With Interactions ####
## Adding interactions to variable recodes for output tables
race.w1pred.int <- c(race.w1pred, SBXFC = "FC x HSB", SBXAB = "Abuse x HSB", SBXSA = "SA x HSB",
                     AGXFC = "FC x AG", AGXAB = "Abuse x AG", AGXSA = "SA x AG")

## Converting results to tables
black.ss.w1p.int.results <- format_mplus(black.ss.w1p[c(2,4,6)], recodes = race.w1pred.int, outcomes = mplusoutcomes,
                                     std = "stdy.standardized", reg = TRUE, r2 = TRUE)
white.ss.w1p.int.results <- format_mplus(white.ss.w1p[c(2,4,6)], recodes = race.w1pred.int, outcomes = mplusoutcomes,
                                     std = "stdy.standardized", reg = TRUE, r2 = TRUE)
hispanic.ss.w1p.int.results <- format_mplus(hispanic.ss.w1p[c(2,4,6)], recodes = race.w1pred.int, outcomes = mplusoutcomes,
                                        std = "stdy.standardized", reg = TRUE, r2 = TRUE)

## Joining Race Tables
# Fit
race.ss.w1p.int.fit <- black.ss.w1p.int.results$Model_Fit %>% mutate(Group = "Black") %>%
  bind_rows(white.ss.w1p.int.results$Model_Fit %>% mutate(Group = "White")) %>%
  bind_rows(hispanic.ss.w1p.int.results$Model_Fit %>% mutate(Group = "Hispanic")) %>%
  select(Group, everything())

# Estimates
race.ss.w1p.int.est.temp <- map2(.x = black.ss.w1p.int.results$Estimates,
                             .y = white.ss.w1p.int.results$Estimates,
                             ~left_join(.x, .y, by = "predictor", suffix = c(".black", ".white")))

race.ss.w1p.int.est <- left_join(race.ss.w1p.int.est.temp$`Sexual Harassment`,
                             rename(hispanic.ss.w1p.int.results$Estimates$`Sexual Harassment`, I.hispanic = "I", S.hispanic = "S"), by = "predictor") %>%
  left_join(left_join(race.ss.w1p.int.est.temp$Delinquency, rename(hispanic.ss.w1p.int.results$Estimates$Delinquency, I.hispanic = "I", S.hispanic = "S"), by = "predictor"),
            by = "predictor", suffix = c(".sh", ".del")) %>%
  left_join(left_join(race.ss.w1p.int.est.temp$Depression, rename(hispanic.ss.w1p.int.results$Estimates$Depression, I.hispanic.dep = "I", S.hispanic.dep = "S"), by = "predictor"),
            by = "predictor")

#####################################################################

# ------  School Belonging  -------------------

## Importing models
high.ss.w1p <- MplusAutomation::readModels(target = "mplus files/High Belonging")
mod.ss.w1p <- MplusAutomation::readModels(target = "mplus files/Moderate Belonging")

## Renaming predictors for output tables
bel.w1pred <- c(I = "Intercept term", S = "Intercept term", FAMCON = "Family Conflict (FC)", ABUSE = "Abuse", SIBAGG = "Sibling Aggression (SA)",
                      EMP_W1 = "Lack of Empathy", FEMALE = "Female", BLACK = "Black", WHITE = "White", OTHERR = "Other Race",
                      SCHBEL = "High School Belong (HSB)", GRADES = "Academic Grades", AGXFC = "FC x AG", AGXAB = "Abuse x AG", AGXSA = "SA x AG")

## Converting results to tables

high.ss.w1p.results <- format_mplus(high.ss.w1p, recodes = bel.w1pred, outcomes = mplusoutcomes,
                                      std = "stdy.standardized", reg = TRUE, r2 = TRUE)
mod.ss.w1p.results <- format_mplus(mod.ss.w1p, recodes = bel.w1pred, outcomes = mplusoutcomes,
                                    std = "stdy.standardized", reg = TRUE, r2 = TRUE)


## Joining High and Moderate Tables
# Fit
bel.ss.w1p.fit <- high.ss.w1p.results$Model_Fit %>% mutate(Belonging = "High") %>%
  bind_rows(mod.ss.w1p.results$Model_Fit %>% mutate(Belonging = "Moderate")) %>%
  select(Belonging, everything())

# Estimates
bel.ss.w1p.est.temp <- map2(.x = high.ss.w1p.results$Estimates,
                       .y = mod.ss.w1p.results$Estimates,
                       ~inner_join(.x, .y, by = "predictor", suffix = c(".high", ".mod")))
bel.ss.w1p.est <- inner_join(bel.ss.w1p.est.temp$`Sexual Harassment`, bel.ss.w1p.est.temp$Delinquency,
                         by = "predictor", suffix = c(".sh", ".del")) %>%
  inner_join(., bel.ss.w1p.est.temp$Depression, by = "predictor")


#####################################################################

# ---- Additional Plots and Values ----------------------

#### Interaction Plots ####

## extracting means, intercept, and slope estimates for all models
intx.data <- map2(.x = list(female.ss.w1p$sh.w1.predict.intx.female.out,
                      male.ss.w1p$sh.w1.predict.intx.male.out,
                      black.ss.w1p$sh.w1.predict.intx.black.out,
                      hispanic.ss.w1p$sh.w1.predict.intx.hispanic.out,
                      white.ss.w1p$sh.w1.predict.intx.white.out),
                  .y = c(rep(TRUE, 4), FALSE),
                 ~intx_dat_shortcut(.x, grades = .y)) %>%
  set_names(c("Female", "Male", "Black", "Hispanic", "White"))

# check <- mplus_est(high.ss.w1p$sh.w1.predict.highschbel.out, params = c("Intercepts", "Means", "Variances"), std = "unstandardized")

## calculating trajectories for Belonging interactions
hsb.intx.plot.data <- map_dfr(intx.data, ~intx_plot_prep(.x$HSB, HSB), .id = "Group") %>%
  mutate(HSB = recode(HSB, `0` = "Moderate", `1` = "High"),
         ACE = str_replace(ACE, "_", " "))

## calculating trajectories for Grades interactions
ag.intx.plot.data <- map_dfr(intx.data[-5], ~intx_plot_prep(.x$AG, Grade_Cat), .id = "Group") %>%
  mutate(Grade_Cat = recode(Grade_Cat, `Means` = "Mean") %>% factor(., levels = c("High", "Mean", "Low")),
         ACE = str_replace(ACE, "_", " "))

## Abuse and SA by Grades for Females
fem.absa.intx.plot <- ag.intx.plot.data %>%
  filter(Group %in% c("Female") & ACE %in% c("Sibling Aggression", "Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.05, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "HHE") +
  theme_bw(base_size = 20) +
  facet_grid(ACE ~ Grade_Cat)


## FC and SA by Grades for Blacks
black.fcsa.intx.plot <- ag.intx.plot.data %>%
  filter(Group %in% c("Black") & ACE %in% c("Sibling Aggression", "Family Conflict")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "HHE") +
  theme_bw(base_size = 20) +
  facet_grid(ACE ~ Grade_Cat)

## FC by HSB for Blacks
black.fc.intx.plot <- hsb.intx.plot.data %>%
  filter(Group %in% c("Black") & ACE %in% c("Family Conflict")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Family\nConflict") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ HSB)

## SA by HSB for Whites
white.sa.intx.plot <- hsb.intx.plot.data %>%
  filter(Group %in% c("White") & ACE %in% c("Sibling Aggression")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.3, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Sibling\nAggression") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ HSB)

## Abuse by HSB for Males
male.ab.intx.plot <- hsb.intx.plot.data %>%
  filter(Group %in% c("Male") & ACE %in% c("Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(0, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Abuse") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ HSB)

hisp.abhsb.intx.plot <- hsb.intx.plot.data %>%
  filter(Group %in% c("Hispanic") & ACE %in% c("Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = HSB)) +
  geom_line(aes(linetype = HSB), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.1, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "School\nBelonging") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ Level)

hisp.abag.intx.plot <- ag.intx.plot.data %>%
  filter(Group %in% c("Hispanic") & ACE %in% c("Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Grade_Cat)) +
  geom_line(aes(linetype = Grade_Cat), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.1, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Academic\nGrades") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ Level)


# ## Abuse and School Belonging on SH Perpetration for Males and Females
# gen.ab.intx.plot <- intx.plot.data %>%
#   filter(Group %in% c("Female", "Male") & ACE == "Abuse") %>%
#   ggplot(aes(x = Year, y = Estimate, group = Level)) +
#   geom_line(aes(linetype = Level), size = 1) +
#   scale_linetype_manual(values = c(1, 2, 3, 4)) +
#   scale_y_continuous(limits = c(-.02, 6), breaks = seq(0, 6, .5)) +
#   scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
#   labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Abuse") +
#   theme_bw(base_size = 20) +
#   facet_grid(Group ~ HSB)


### Outcome by Group from Multigroup Analysis ####

SHplots <- map2(list(gen.ss.mplus.results$Plot_Data, race.ss.mplus.results$Plot_Data, bel.ss.mplus.results$Plot_Data),
                .y = c("Sexual Harassment Perpetration", "", ""),
                ~.x %>% filter(outcome == "Sexual Harassment") %>%
                  multigroup_plot(., ylab = .y, max.y = 6) +
                  theme(legend.position = "top",
                        legend.key.width = unit(1,"cm")))

SHPlot.grid <- cowplot::plot_grid(plotlist = SHplots, align = "h", nrow = 1, labels = "AUTO")


#### gather R2 ####
grouporder <- c("Female", "Male", "Hispanic", "Black", "White", "High", "Moderate")
all.r2 <- map2_dfr(list(female.ss.w1p.int.results,male.ss.w1p.int.results,
                        hispanic.ss.w1p.int.results, black.ss.w1p.int.results, white.ss.w1p.int.results,
                        high.ss.w1p.results, mod.ss.w1p.results),
                   .y = grouporder,
                   ~.x$R2 %>% filter(outcome == "Sexual Harassment" & param %in% c("I", "S")) %>%
                     mutate(Estimate = paste0(est, " (", se, ")"),
                            Group = .y) %>%
                     select(Group, param, Estimate)) %>%
  spread(param, Estimate) %>%
  mutate(Group = factor(Group, levels = grouporder)) %>%
  arrange(Group)

#####################################

# ------  Saving Raw Scale Score Growth Objects ------------------

save(mplusoutcomes, growth_settings, lgm.ss.syntax, lgm.ss.results, lgm.ss.strat.results,
     lgm.ss.mplus, gen.ss.mplus, race.ss.mplus, bel.ss.mplus,
     lgm.ss.mplus.results, lgm.ss.strat.mplus.results,
     gen.ss.mplus.results, race.ss.mplus.results, bel.ss.mplus.results,
     gen.ss.results, race.ss.results, bel.ss.results,
     full.ss.w1p.results, full.ss.w1p.est,
     female.ss.w1p, female.ss.w1p.results, male.ss.w1p, male.ss.w1p.results, gen.ss.w1p.est, gen.ss.w1p.fit,
     female.ss.w1p.int.results, male.ss.w1p.int.results, gen.ss.w1p.int.est, gen.ss.w1p.int.fit,
     black.ss.w1p, black.ss.w1p.results, hispanic.ss.w1p, hispanic.ss.w1p.results, white.ss.w1p, white.ss.w1p.results,
     race.ss.w1p.est, race.ss.w1p.fit,
     black.ss.w1p.int.results, hispanic.ss.w1p.int.results, white.ss.w1p.int.results, race.ss.w1p.int.est, race.ss.w1p.int.fit,
     high.ss.w1p, high.ss.w1p.results, mod.ss.w1p, mod.ss.w1p.results, bel.ss.w1p.est, bel.ss.w1p.fit,
     intx.data, hsb.intx.plot.data, ag.intx.plot.data,
     fem.absa.intx.plot, black.fc.intx.plot, black.fcsa.intx.plot, white.sa.intx.plot, male.ab.intx.plot,
     hisp.abag.intx.plot, hisp.abhsb.intx.plot ,SHPlot.grid, all.r2,
     file = "Output/Scale_Score_LGM_Results.RData")
# load("Output/Scale_Score_LGM_Results.RData")

####################################################################

gen.ss.mplus$sexual.harassment.multigroup.gender.free.out$parameters$unstandardized %>% filter(paramHeader == "Means")
race.ss.mplus$sexual.harassment.multigroup.race.free.out$parameters$unstandardized %>% filter(paramHeader == "Means")
