##########################################
#                                        #
#       Paper 1 - Abuse and SH           #
#   Construct and Scale Measurement      #
#                                        #
##########################################

# Note. School belonging was re-run on 6/8/2022 to examine partial invariance.
# Due to changes in lavaan, the clustering option was no longer available as it produced the error:
# Error in lav_model_information_firstorder(lavmodel = lavmodel, lavsamplestats = lavsamplestats,  : 
                                            # lavaan ERROR: information = "first.order" not available for estimator ‘DWLS’
# Objects related to the partial invariance were saved separately from the original runs.
# Although it turns out the results don't change much at all.
# Interesting thread: https://groups.google.com/g/lavaan/c/9nvVozLbBng

## TO-TRY FOR PARTIAL INVARIANCE INVESTIGATION
# - items 2 and 4 as marker variable
# - Treating variables as continuous and using FIML
# - Multiple imputation

source("Scripts/0-PnF.R")
load("Output/SS_Data_For_Analysis.RData")

# --------- CFAs by Wave ---------------


####     Predictors     ####

## Family Conflict
fv.cfa <- run_cfa(sslong, inames = fv_items, fname = "FV",
                   waves = 1, missing = "pairwise", cluster = NULL)
fv.cfa.out <- list(fits = NULL,
                    reliability = get_relis(fv.cfa[[1]], 1),
                    loadings = extract_lavaan_parameters(fv.cfa[[1]], std = "std.all", params = "=~"))

## Abuse
ab.cfa <- run_cfa(sslong, inames = ab_items, fname = "Ab",
                  waves = 1, missing = "pairwise", cluster = NULL)
ab.cfa.out <- list(fits = NULL,
                   reliability = get_relis(ab.cfa[[1]], 1),
                   loadings = extract_lavaan_parameters(ab.cfa[[1]], std = "std.all", params = "=~"))

## Sibling Aggression
# Note: model won't be identified since there are only 2 items. Only running to get reliability
sib.cfa <- run_cfa(sslong, inames = sib_items, fname = "Sib",
                  waves = 1, missing = "listwise", cluster = NULL)
sib.cfa.out <- list(fits = NULL,
                   reliability = get_relis(sib.cfa[[1]], 1),
                   loadings = extract_lavaan_parameters(sib.cfa[[1]], std = "std.all", params = "=~"))


## Lack of Empathy
emp.cfa <- run_cfa(sslong, inames = emp_items, fname = "Lack_Empathy",
                   waves = 1:4, missing = "listwise", cluster = NULL)
emp.cfa.out <- list(fits = fits_wrapper(emp.cfa),
                    reliability = map2_dfr(.x = emp.cfa, .y = 1:4, ~get_relis(.x, .y)),
                    loadings = map(emp.cfa, ~extract_lavaan_parameters(.x, std = "std.all", params = "=~")))
# RMSEA a bit high

## School Belonging
schb.cfa <- run_cfa(sslong, inames = schb_items, fname = "School_Belonging",
                    waves = 1:4, missing = "listwise", cluster = NULL)
schb.cfa.out <- list(fits = fits_wrapper(schb.cfa),
                    reliability = map2_dfr(.x = schb.cfa, .y = 1:4, ~get_relis(.x, .y)),
                    loadings = map(schb.cfa, ~extract_lavaan_parameters(.x, std = "std.all", params = "=~")))
# fit gets worse with each wave, but still adequate


####     Outcomes     ####

## Sexual Harassment - removing items 5 (homophobic name calling) and 8-12
sh.cfa <- run_cfa(data = sslong, inames = paste0(c(sh_items[c(1:4, 6, 7)]), "_di"), fname = "SH",
                  waves = 1:4,  missing = "listwise", cluster = NULL)
sh.cfa.out <- list(fits = fits_wrapper(sh.cfa), # Model fit
                   reliability = map2_dfr(.x = sh.cfa, .y = 1:4, ~get_relis(.x, .y)),
                   loadings = map(sh.cfa, ~extract_lavaan_parameters(.x, std = "std.all", params = "=~")))
## Seems to work well for Waves 1-4

## Depression - removing item 6 (happiness item)
dep.cfa <- run_cfa(sslong, inames = dep_items[c(2,4,1,3,5:8)], fname = "Depression",
                   waves = 1:4, missing = "listwise", cluster = NULL)
dep.cfa.out <- list(fits = fits_wrapper(dep.cfa),
                    reliability = map2_dfr(.x = dep.cfa, .y = 1:4, ~get_relis(.x, .y)),
                    loadings = map(dep.cfa, ~extract_lavaan_parameters(.x, std = "std.all", params = "=~")))
# good, though item 6 can be removed


## Deliquency
del.cfa <- run_cfa(sslong, inames = del_items, fname = "Delinquency",
                   waves = 1:4, missing = "listwise", cluster = NULL)
del.cfa.out <- list(fits = fits_wrapper(del.cfa),
                    reliability = map2_dfr(.x = del.cfa, .y = 1:4, ~get_relis(.x, .y)),
                    loadings = map(del.cfa, ~extract_lavaan_parameters(.x, std = "std.all", params = "=~")))
# good across all waves - item 1 has lower loading than the rest


## Substance Use
su.cfa <- run_cfa(sslong, inames = su_items, fname = "Substance_Use",
                  waves = 1:4, missing = "listwise", cluster = NULL)
su.cfa.out <- list(fits = fits_wrapper(su.cfa),
                    reliability = map2_dfr(.x = su.cfa, .y = 1:4, ~get_relis(.x, .y)),
                    loadings = map(su.cfa, ~extract_lavaan_parameters(.x, std = "std.all", params = "=~")))
# good across all waves


## gathering reliabilities for Table 1 - saved with measurement invariance objects in "Output/MI_For_Analysis.RData"
all.relis <- map2_dfr(.x = list(fv.cfa.out, ab.cfa.out, sib.cfa.out,
                                emp.cfa.out, sh.cfa.out, dep.cfa.out, del.cfa.out), # schb.cfa.out, su.cfa.out
                      .y = c("Family_Conflict", "Abuse", "Sibling_Aggression",
                             "emp_scale", "sh_scale", "dep_scale", "del_scale"),
                      ~.x$reliability %>% mutate(name = .y)) %>%
  mutate(scale = ifelse(name %in% c("Family_Conflict", "Abuse", "Sibling_Aggression"),
                        name, paste(name, wave, sep = "_W"))) %>%
  select(scale, alpha, omega)


#### Saving CFA objects ####
save(fv.cfa, fv.cfa.out, ab.cfa, ab.cfa.out, sib.cfa, sib.cfa.out,
     emp.cfa, emp.cfa.out, schb.cfa, schb.cfa.out,
     sh.cfa, sh.cfa.out, dep.cfa, dep.cfa.out,
     del.cfa, del.cfa.out, su.cfa, su.cfa.out, 
     file = "Output/Abuse_SH_CFA.RData")


# load("Output/Abuse_SH_CFA.RData")


############################################################


# ---- Longitudinal Measurement Invariance --------------------

#### Setting up models ####
## Items organized across waves
# Returns list of lists
# - character vector for item name at each wave nested in item nested in scale
longIndNames <- map(.x = list(dep_items[-6], emp_items, 
                              del_items, su_items, schb_items[c(2,3,4,1)]), # reording schb to test for better item
                    ~lapply(.x, paste0, "_W", 1:4) %>%
                      set_names(.x)) %>%
  set_names(c("Depression", 
              "Lack_of_Empathy","Delinquency",
              "Substance_Use","School_Belonging"))

longIndNames[["Sexual_Harassment"]] <- lapply(c(sh_items[c(1:4, 6, 7)]), paste0, "_W", 1:4, "_di") %>%
  set_names(c(sh_items[c(1:4, 6, 7)]))

# ## default options for measurement invariance models - see mi_wrapper function in 0-PnF.R
# default.opts <- list(
#   ID.fac = "auto.fix.first", # fix first loading to identify latent common factor 
#   ID.cat = "millsap",        # Millsap & Tein (2004)
#   ID.thr = c(1L, 2L),        # thresholds to constrain to identify latent residual factors
#   long.equal = "",           # longitudinal constraints (added to establish invariance)
#   auto = "all",         # include lagged residual covariances for each indicator based on longIndNames
#   group = NULL,
#   group.equal = "",
#   parameterization = "theta", # 
#   meanstructure = TRUE,
#   mimic = "Mplus",
#   estimator = "WLSMV",
#   missing = "pairwise",
#   cluster = NULL)

## Double-check categorical longitudinal MI paper
# Is anything different with binary indicators?
# Ah yes, M&T identification constrains the first and, in this case, only threshold (Millsap & Tein, 2004)
# Instead of constraining the second threshold for the reference variable for each factor, since it doesn't exist,
# M&T states to constrain the residual variance for the reference variable to 1, which the syntax does

#### Sexual Harassment (using items 1:4, 6, 7) ####
tictoc::tic()
sh.mi.mod <- write_mi_mod(list = longIndNames$Sexual_Harassment, fname = "sh", waves = 4)

# cat(sh.mi.mod)

sh.config <- mi_wrapper(model = sh.mi.mod,
                        data = sswide,
                        items = longIndNames$Sexual_Harassment,
                        fnames = list(sh = c("sh1", "sh2", "sh3", "sh4")),
                        #cluster = "SCHOOL_ID_W1",
                        missing = "listwise")

sh.metric <- mi_wrapper(model = sh.mi.mod,
                        data = sswide,
                        items = longIndNames$Sexual_Harassment,
                        fnames = list(sh = c("sh1", "sh2", "sh3", "sh4")),
                        #cluster = "SCHOOL_ID_W1",
                        missing = "listwise",
                        long.equal = c("loadings"))

# ## the only threshold is constrained to equality to identify the model
# sh.scalar <- mi_wrapper(model = sh.mi.mod,
#                          data = sswide,
#                          items = longIndNames$Sexual_Harassment,
#                          fnames = list(sh = c("sh1", "sh2", "sh3", "sh4")),
#                          cluster = "SCHOOL_ID_W1",
#                          long.equal = c("loadings", "thresholds"))

sh.strict <- mi_wrapper(model = sh.mi.mod,
                        data = sswide,
                        items = longIndNames$Sexual_Harassment,
                        fnames = list(sh = c("sh1", "sh2", "sh3", "sh4")),
                        # cluster = "SCHOOL_ID_W1",
                        missing = "listwise",
                        long.equal = c("loadings", "thresholds", "residuals"))

tictoc::toc()

## Investigating partial invariance

sh.pi.metric.syn <- as.character(sh.metric$syntax) %>%
  sub("lambda.4_1\\*SH_PERP4_W4_di", "lambda.4_4\\*SH_PERP4_W4_di", .)
sh.pi.metric.fit <- lavaan(model = sh.pi.metric.syn, data = sswide, ordered = unlist(longIndNames$Sexual_Harassment, use.names = FALSE),
                          missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
sh.pi.metric <- list(syntax = sh.pi.metric.syn,
                     fit = sh.pi.metric.fit)

sh.pi.strict.syn <- as.character(sh.strict$syntax) %>%
  sub("lambda.4_1\\*SH_PERP4_W2_di", "lambda.4_2\\*SH_PERP4_W2_di", .) %>%
  # sub("1\\*SH_PERP3_W2_di \\+ theta.3_3\\*SH_PERP3_W2_di", "NA\\*SH_PERP3_W2_di \\+ theta.7_7\\*SH_PERP3_W2_di", .)
  sub("1\\*SH_PERP3_W3_di \\+ theta.3_3\\*SH_PERP3_W3_di", "NA\\*SH_PERP3_W3_di \\+ theta.8_8\\*SH_PERP3_W3_di", .) %>%
  sub("1\\*SH_PERP3_W4_di \\+ theta.3_3\\*SH_PERP3_W4_di", "NA\\*SH_PERP3_W4_di \\+ theta.9_9\\*SH_PERP3_W4_di", .)
sh.pi.strict.fit <- lavaan(model = sh.pi.strict.syn, data = sswide, ordered = unlist(longIndNames$Sexual_Harassment, use.names = FALSE),
                       missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
sh.pi.strict <- list(syntax = sh.pi.strict.syn,
                     fit = sh.pi.strict.fit)

lavTestScore(sh.pi.metric.fit, cumulative = TRUE)
parTable(sh.pi.metric.fit) %>% View()
cat(sh.pi.strict.syn)


sh.fits <- fits_wrapper(mod.list = list(sh.config$fit, sh.pi.metric$fit,
                                        sh.pi.strict$fit),
                        .id = "model") %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))

sh.mi <- bind_rows(compare_mods(sh.config$fit, sh.pi.metric$fit),
                   compare_mods(sh.pi.metric$fit, sh.pi.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())

summary(sh.pi.strict$fit)


#### Depression (removed Item 6) ####
# Not sure what's going on with depression yet
# lavaan WARNING: the optimizer warns that a solution has NOT been found!
# In CFAs, thresholds appear to be in order for wave 1
# changing mimic to lavaan doesn't work
# consider: changing marker item; using Wu rather than Millsap identification constraints
# The first threshold for item 3 in W1 is way different than the other waves
# lapply(1:4, function(x) round(prop.table(table(sslong[sslong$Wave == x,]$DEPRESSION3)),3))
# Suggests that at Wave 1, responses > 0 are very indicative of Depression
# The other thresholds are reasonable, so use ID.thr argument to specify different thresholds

lapply(1:4, function(x) round(prop.table(table(sslong[sslong$Wave == x,]$DEPRESSION1)),3))

tictoc::tic()
dep.mi.mod <- write_mi_mod(list = longIndNames$Depression, fname = "dep", waves = 4)
dep.mi.mod.end1 <- write_mi_mod(list = longIndNames$Depression[c(2:8,1)], fname = "dep", waves = 4)
dep.mi.mod.24 <- write_mi_mod(list = longIndNames$Depression[c(2,4,1,3,5:8)], fname = "dep", waves = 4)


# dep.syn.wu <- do.call(measEq.syntax,list(configural.model = dep.mi.mod,
#                                          data = sswide,
#                                          longFacNames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
#                                          longIndNames = longIndNames$Depression,
#                                          ordered = unlist(longIndNames$Depression),
#                                          ID.fac = "std.lv",
#                                          ID.cat = "Wu",
#                                          long.equal = "",
#                                          auto = "all",  # includes lagged residual covariances for each indicator based on longIndNames
#                                          group = NULL,
#                                          group.equal = "",
#                                          parameterization = "delta",
#                                          meanstructure = TRUE,
#                                          mimic = "Mplus",
#                                          estimator = "WLSMV",
#                                          missing = "pairwise"))
# 
# # converges and fits well (CFI = .98, RMSEA = .03, SRMR = .04)
# # all thresholds are estimated which allows item 3 to be as is
# dep.config.wu <- cfa(model = as.character(dep.syn.wu),
#                      data = sswide,
#                      ordered = unlist(longIndNames$Depression),
#                      parameterization = "delta",
#                      meanstructure = TRUE,
#                      mimic = "Mplus",
#                      estimator = "WLSMV",
#                      missing = "pairwise",
#                      cluster = "SCHOOL_ID_W1")
# 
# 
# ## converges, fit is exactly the same as wu (CFI = .98, RMSEA = .03, SRMR = .04),
# # but item 3 gives ridiculous estimates in waves 2-4
# dep.config.end1 <- mi_wrapper(model = dep.mi.mod.end1,
#                          data = sswide,
#                          items = longIndNames$Depression,
#                          fnames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
#                          cluster = "SCHOOL_ID_W1")

## converges, fit is exactly the same, item 3 still giving ridiculous estimates, but
# only in waves 2 and 3
# dep.config.24 <- mi_wrapper(model = dep.mi.mod.24,
#                             data = sswide,
#                             items = longIndNames$Depression,
#                             fnames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
#                             cluster = "SCHOOL_ID_W1")

# didn't converge with dep.mi.mod
# does with dep.mi.mod.24, so there does seem to be an issue with item 1 being the marker
# fit indices the same suggesting constraint interaction is not present
# item 3 is still ridiculous here;
# as constraints are added the ridiculousness is reigned without loss of fit, which is interesting
dep.config <- mi_wrapper(model = dep.mi.mod.24,
                             data = sswide,
                             items = longIndNames$Depression,
                             fnames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
                             ID.thr = c(4L, 3L),
                             cluster = "SCHOOL_ID_W1")

dep.metric <- mi_wrapper(model = dep.mi.mod.24,
                         data = sswide,
                         items = longIndNames$Depression,
                         fnames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
                         ID.thr = c(4L, 3L),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings"))

dep.scalar <- mi_wrapper(model = dep.mi.mod.24,
                         data = sswide,
                         items = longIndNames$Depression,
                         fnames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
                         ID.thr = c(4L, 3L),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds"))

dep.strict <- mi_wrapper(model = dep.mi.mod.24,
                         data = sswide,
                         items = longIndNames$Depression,
                         fnames = list(dep = c("dep1", "dep2", "dep3", "dep4")),
                         ID.thr = c(4L, 3L),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds", "residuals"))

dep.fits <- fits_wrapper(mod.list = list(dep.config$fit, dep.metric$fit,
                                         dep.scalar$fit, dep.strict$fit),
                         .id = "model") %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

dep.mi <- bind_rows(compare_mods(dep.config$fit, dep.metric$fit),
                    compare_mods(dep.metric$fit, dep.scalar$fit),
                    compare_mods(dep.scalar$fit, dep.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())
tictoc::toc()

#### Lack of Empathy ####
tictoc::tic()
emp.mi.mod <- write_mi_mod(list = longIndNames$Lack_of_Empathy, fname = "emp", waves = 4)

emp.config <- mi_wrapper(model = emp.mi.mod,
                         data = sswide,
                         items = longIndNames$Lack_of_Empathy,
                         fnames = list(emp = c("emp1", "emp2", "emp3", "emp4")),
                         cluster = "SCHOOL_ID_W1")

emp.metric <- mi_wrapper(model = emp.mi.mod,
                         data = sswide,
                         items = longIndNames$Lack_of_Empathy,
                         fnames = list(emp = c("emp1", "emp2", "emp3", "emp4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings"))

emp.scalar <- mi_wrapper(model = emp.mi.mod,
                         data = sswide,
                         items = longIndNames$Lack_of_Empathy,
                         fnames = list(emp = c("emp1", "emp2", "emp3", "emp4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds"))

emp.strict <- mi_wrapper(model = emp.mi.mod,
                         data = sswide,
                         items = longIndNames$Lack_of_Empathy,
                         fnames = list(emp = c("emp1", "emp2", "emp3", "emp4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds", "residuals"))

emp.fits <- fits_wrapper(mod.list = list(emp.config$fit, emp.metric$fit,
                                         emp.scalar$fit, emp.strict$fit),
                         .id = "model") %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

emp.mi <- bind_rows(compare_mods(emp.config$fit, emp.metric$fit),
                    compare_mods(emp.metric$fit, emp.scalar$fit),
                    compare_mods(emp.scalar$fit, emp.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())
tictoc::toc()


####    Delinquency    ####
tictoc::tic()
del.mi.mod <- write_mi_mod(list = longIndNames$Delinquency, fname = "del", waves = 4)

del.config <- mi_wrapper(model = del.mi.mod,
                         data = sswide,
                         items = longIndNames$Delinquency,
                         fnames = list(del = c("del1", "del2", "del3", "del4")),
                         cluster = "SCHOOL_ID_W1")

del.metric <- mi_wrapper(model = del.mi.mod,
                         data = sswide,
                         items = longIndNames$Delinquency,
                         fnames = list(del = c("del1", "del2", "del3", "del4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings"))

# del.scalar <- mi_wrapper(model = del.mi.mod,
#                          data = sswide,
#                          items = longIndNames$Delinquency,
#                          fnames = list(del = c("del1", "del2", "del3", "del4")),
#                          cluster = "SCHOOL_ID_W1",
#                          long.equal = c("loadings", "thresholds"))

del.strict <- mi_wrapper(model = del.mi.mod,
                         data = sswide,
                         items = longIndNames$Delinquency,
                         fnames = list(del = c("del1", "del2", "del3", "del4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds", "residuals"))

del.fits <- fits_wrapper(mod.list = list(del.config$fit, del.metric$fit,del.strict$fit),
                         .id = "model") %>%
  mutate(model = c("Configural", "Metric/Scalar", "Strict"))

del.mi <- bind_rows(compare_mods(del.config$fit, del.metric$fit),
                    compare_mods(del.metric$fit, del.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "residual variance")) %>%
  select(constraint_added, everything())
tictoc::toc()


####    Substance Use    ####
tictoc::tic()
su.mi.mod <- write_mi_mod(list = longIndNames$Substance_Use, fname = "su", waves = 4)

su.config <- mi_wrapper(model = su.mi.mod,
                         data = sswide,
                         items = longIndNames$Substance_Use,
                         fnames = list(su = c("su1", "su2", "su3", "su4")),
                         cluster = "SCHOOL_ID_W1")

su.metric <- mi_wrapper(model = su.mi.mod,
                         data = sswide,
                         items = longIndNames$Substance_Use,
                         fnames = list(su = c("su1", "su2", "su3", "su4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings"))

su.scalar <- mi_wrapper(model = su.mi.mod,
                         data = sswide,
                         items = longIndNames$Substance_Use,
                         fnames = list(su = c("su1", "su2", "su3", "su4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds"))

su.strict <- mi_wrapper(model = su.mi.mod,
                         data = sswide,
                         items = longIndNames$Substance_Use,
                         fnames = list(su = c("su1", "su2", "su3", "su4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds", "residuals"))

su.fits <- fits_wrapper(mod.list = list(su.config$fit, su.metric$fit,
                                         su.scalar$fit, su.strict$fit),
                         .id = "model") %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

su.mi <- bind_rows(compare_mods(su.config$fit, su.metric$fit),
                    compare_mods(su.metric$fit, su.scalar$fit),
                    compare_mods(su.scalar$fit, su.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())
tictoc::toc()

####    School Belonging    ####
tictoc::tic()
# Note: These were originally run with item order of 1, 2, 3, 4; unlike the partial invariance models
schb.mi.mod <- write_mi_mod(list = longIndNames$School_Belonging, fname = "schb", waves = 4)

schb.config <- mi_wrapper(model = schb.mi.mod,
                         data = sswide,
                         items = longIndNames$School_Belonging,
                         fnames = list(schb = c("schb1", "schb2", "schb3", "schb4")),
                         cluster = "SCHOOL_ID_W1")

schb.metric <- mi_wrapper(model = schb.mi.mod,
                         data = sswide,
                         items = longIndNames$School_Belonging,
                         fnames = list(schb = c("schb1", "schb2", "schb3", "schb4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings"))

schb.scalar <- mi_wrapper(model = schb.mi.mod,
                         data = sswide,
                         items = longIndNames$School_Belonging,
                         fnames = list(schb = c("schb1", "schb2", "schb3", "schb4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds"))

schb.strict <- mi_wrapper(model = schb.mi.mod,
                         data = sswide,
                         items = longIndNames$School_Belonging,
                         fnames = list(schb = c("schb1", "schb2", "schb3", "schb4")),
                         cluster = "SCHOOL_ID_W1",
                         long.equal = c("loadings", "thresholds", "residuals"))

schb.fits <- fits_wrapper(mod.list = list(schb.config$fit, schb.metric$fit,
                                         schb.scalar$fit, schb.strict$fit),
                         .id = "model") %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

schb.mi <- bind_rows(compare_mods(schb.config$fit, schb.metric$fit),
                    compare_mods(schb.metric$fit, schb.scalar$fit),
                    compare_mods(schb.scalar$fit, schb.strict$fit)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())
tictoc::toc()

#### Investigating Partial invariance #####
## Needs to be saved separately from original run b/c mi_wrapper is not designed for it
## and cluster argument no longer works in lavaan for this purpose
schb.pi.mod <- write_mi_mod(list = longIndNames$School_Belonging, fname = "schb", waves = 4)

schb.pi.args <- list(configural.model = schb.pi.mod,
                     data = sswide,
                     #cluster = "SCHOOL_ID_W1", 
                     longFacNames = list(schb = c("schb1", "schb2", "schb3", "schb4")),
                     longIndNames = longIndNames$School_Belonging,
                     ordered = unlist(longIndNames$School_Belonging, use.names = FALSE),
                     ID.cat = "millsap", parameterization = "theta", ID.fac = "auto.fix.first",
                     ID.thr = c(2, 3), mimic = "Mplus", missing = "listwise")

schb.pi.config <- do.call(measEq.syntax, args = c(list(long.equal = "", return.fit = TRUE), schb.pi.args))
schb.pi.metric.syn <- do.call(measEq.syntax, args = c(list(long.equal = "loadings", return.fit = FALSE), schb.pi.args)) %>%
  as.character()
  # sub("lambda\\.2_1\\*SCH_BEL3_W2", "lambda\\.2_2\\*SCH_BEL3_W2", .) %>%
  # sub("lambda\\.2_1\\*SCH_BEL3_W3", "lambda\\.2_3\\*SCH_BEL3_W3", .) %>%
  # sub("lambda\\.2_1\\*SCH_BEL3_W4", "lambda\\.2_4\\*SCH_BEL3_W4", .)
schb.pi.metric <- lavaan(model = schb.pi.metric.syn, data = sswide, ordered = unlist(longIndNames$School_Belonging, use.names = FALSE),
                          missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")
  
schb.pi.scalar.syn <- do.call(measEq.syntax, args = c(list(long.equal = c("loadings", "thresholds"), return.fit = FALSE), schb.pi.args)) %>%
  as.character() %>%
  # sub("SCH_BEL2_W2 \\| NA\\*t3 \\+ SCH_BEL2_W1\\.thr3", "SCH_BEL2_W2 \\| NA\\*t3 \\+ SCH_BEL2_W2\\.thr3", .) %>%
  # sub("SCH_BEL2_W3 \\| NA\\*t3 \\+ SCH_BEL2_W1\\.thr3", "SCH_BEL2_W3 \\| NA\\*t3 \\+ SCH_BEL2_W3\\.thr3", .) %>%
  sub("SCH_BEL2_W4 \\| NA\\*t3 \\+ SCH_BEL2_W1\\.thr3", "SCH_BEL2_W4 \\| NA\\*t3 \\+ SCH_BEL2_W4\\.thr3", .) %>%
  # sub("SCH_BEL1_W2 \\| NA\\*t3 \\+ SCH_BEL1_W1\\.thr3", "SCH_BEL1_W2 \\| NA\\*t3 \\+ SCH_BEL1_W2\\.thr3", .) %>%
  sub("SCH_BEL1_W3 \\| NA\\*t3 \\+ SCH_BEL1_W1\\.thr3", "SCH_BEL1_W3 \\| NA\\*t3 \\+ SCH_BEL1_W3\\.thr3", .) %>%
  sub("SCH_BEL1_W4 \\| NA\\*t3 \\+ SCH_BEL1_W1\\.thr3", "SCH_BEL1_W4 \\| NA\\*t3 \\+ SCH_BEL1_W4\\.thr3", .) %>%
  sub("SCH_BEL3_W2 \\| NA\\*t1 \\+ SCH_BEL3_W1\\.thr1", "SCH_BEL3_W2 \\| NA\\*t1 \\+ SCH_BEL3_W2\\.thr1", .) %>%
  sub("SCH_BEL4_W2 \\| NA\\*t3 \\+ SCH_BEL4_W1\\.thr3", "SCH_BEL4_W2 \\| NA\\*t3 \\+ SCH_BEL4_W2\\.thr3", .) %>%
  sub("SCH_BEL4_W2 \\| NA\\*t1 \\+ SCH_BEL4_W1\\.thr1", "SCH_BEL4_W2 \\| NA\\*t1 \\+ SCH_BEL4_W2\\.thr1", .)
  # sub("SCH_BEL2_W3 \\| NA\\*t2 \\+ SCH_BEL2_W1\\.thr2", "SCH_BEL2_W3 \\| NA\\*t2 \\+ SCH_BEL2_W3\\.thr2", .) # gets p > .05, but would rather keep thr2 invariant

schb.pi.scalar <- lavaan(model = schb.pi.scalar.syn, data = sswide, ordered = unlist(longIndNames$School_Belonging, use.names = FALSE),
                          missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")

schb.pi.strict.syn <- do.call(measEq.syntax, args = c(list(long.equal = c("loadings", "thresholds", "residuals"), return.fit = FALSE), schb.pi.args)) %>%
  as.character() %>%
  sub("SCH_BEL2_W4 \\| NA\\*t3 \\+ SCH_BEL2_W1\\.thr3", "SCH_BEL2_W4 \\| NA\\*t3 \\+ SCH_BEL2_W4\\.thr3", .) %>%
  sub("SCH_BEL1_W3 \\| NA\\*t3 \\+ SCH_BEL1_W1\\.thr3", "SCH_BEL1_W3 \\| NA\\*t3 \\+ SCH_BEL1_W3\\.thr3", .) %>%
  sub("SCH_BEL1_W4 \\| NA\\*t3 \\+ SCH_BEL1_W1\\.thr3", "SCH_BEL1_W4 \\| NA\\*t3 \\+ SCH_BEL1_W4\\.thr3", .) %>%
  sub("SCH_BEL3_W2 \\| NA\\*t1 \\+ SCH_BEL3_W1\\.thr1", "SCH_BEL3_W2 \\| NA\\*t1 \\+ SCH_BEL3_W2\\.thr1", .) %>%
  sub("SCH_BEL4_W2 \\| NA\\*t3 \\+ SCH_BEL4_W1\\.thr3", "SCH_BEL4_W2 \\| NA\\*t3 \\+ SCH_BEL4_W2\\.thr3", .) %>%
  sub("SCH_BEL4_W2 \\| NA\\*t1 \\+ SCH_BEL4_W1\\.thr1", "SCH_BEL4_W2 \\| NA\\*t1 \\+ SCH_BEL4_W2\\.thr1", .) %>%
  # sub("1\\*SCH_BEL2_W2 \\+ theta.1_1\\*SCH_BEL2_W2", "NA\\*SCH_BEL2_W2 \\+ theta.5_5\\*SCH_BEL2_W2", .) %>%
  # sub("1\\*SCH_BEL2_W3 \\+ theta.1_1\\*SCH_BEL2_W3", "NA\\*SCH_BEL2_W3 \\+ theta.6_6\\*SCH_BEL2_W3", .) %>%
  sub("1\\*SCH_BEL2_W4 \\+ theta.1_1\\*SCH_BEL2_W4", "NA\\*SCH_BEL2_W4 \\+ theta.7_7\\*SCH_BEL2_W4", .)
  # sub("1\\*SCH_BEL1_W4 \\+ theta.4_4\\*SCH_BEL1_W4", "NA\\*SCH_BEL1_W4 \\+ theta.8_8\\*SCH_BEL1_W4", .)
  # 
schb.pi.strict <- lavaan(model = schb.pi.strict.syn, data = sswide, ordered = unlist(longIndNames$School_Belonging, use.names = FALSE),
                         missing = "listwise",  parameterization = "theta", auto.fix.first = TRUE, mimic = "Mplus")

# sub("1\\*SCH_BEL4_W4 \\+ theta.3_3\\*SCH_BEL4_W4", "NA\\*SCH_BEL4_W4 \\+ theta.5_5\\*SCH_BEL4_W4", .) %>%
#   sub("1\\*SH_PERP3_W3_di \\+ theta.3_3\\*SH_PERP3_W3_di", "NA\\*SH_PERP3_W3_di \\+ theta.8_8\\*SH_PERP3_W3_di", .) %>%
#   sub("1\\*SH_PERP3_W4_di \\+ theta.3_3\\*SH_PERP3_W4_di", "NA\\*SH_PERP3_W4_di \\+ theta.9_9\\*SH_PERP3_W4_di", .)

schb.pi.fits <- fits_wrapper(mod.list = list(schb.pi.config, schb.pi.metric,
                                          schb.pi.scalar, schb.pi.strict),
                          .id = "model") %>%
  mutate(model = c("Configural", "Metric", "Scalar", "Strict"))

schb.pi.mi <- bind_rows(compare_mods(schb.pi.config, schb.pi.metric),
                     compare_mods(schb.pi.metric, schb.pi.scalar),
                     compare_mods(schb.pi.scalar, schb.pi.strict)) %>%
  mutate(constraint_added = c("loadings", "thresholds", "residual variance")) %>%
  select(constraint_added, everything())

lavTestScore(schb.pi.strict, cumulative = TRUE)
parTable(schb.pi.strict) %>% View()
cat(schb.pi.strict.syn)

# 98.380 12 .000 None
# 93.706 9  .000 it4
# 74.174 9  .00  it3
# 74.203         it1

#### Saving Invariance Objects ####
save(sh.mi.mod, sh.config, sh.metric, sh.strict, sh.pi.metric, sh.pi.strict, sh.fits, sh.mi,
     dep.mi.mod.24, dep.config, dep.metric, dep.scalar, dep.strict, dep.fits, dep.mi,
     emp.mi.mod, emp.config, emp.metric, emp.scalar, emp.strict, emp.fits, emp.mi,
     del.mi.mod, del.config, del.metric, del.strict, del.fits, del.mi,
     su.mi.mod, su.config, su.metric, su.scalar, su.strict, su.fits, su.mi,
     schb.mi.mod, schb.config, schb.metric, schb.scalar, schb.strict, schb.fits, schb.mi,
     schb.pi.mod, schb.pi.config, schb.pi.metric, schb.pi.scalar, schb.pi.strict, schb.pi.fits, schb.pi.mi,
     file = "Output/Abuse_SH_MeasInv.RData")

# load("Output/Abuse_SH_MeasInv.RData")

###################################################################

# ----      Factor Scores   -------------------

# default is se = "none"; SEs can only be calculated for complete continuous data
# sh.fs2 <- as.data.frame(lavPredict(sh.strict$fit, type = "lv", method = "EBM", se = "standard"))
sh.fs <- as.data.frame(lavPredict(sh.strict$fit, type = "lv", method = "EBM"))
dep.fs <- as.data.frame(lavPredict(dep.strict$fit, type = "lv", method = "EBM"))
emp.fs <- as.data.frame(lavPredict(emp.strict$fit, type = "lv", method = "EBM"))
del.fs <- as.data.frame(lavPredict(del.strict$fit, type = "lv", method = "EBM"))
su.fs <- as.data.frame(lavPredict(su.strict$fit, type = "lv", method = "EBM"))
schb.fs <- as.data.frame(lavPredict(schb.strict$fit, type = "lv", method = "EBM"))

## saving into a list of dataframes
factor.scores <- list(sh.fs, dep.fs, emp.fs, del.fs, su.fs, schb.fs) %>%
  set_names(c("Sexual_Harassment", "Depression", 
              "Lack_of_Empathy","Delinquency",
              "Substance_Use","School_Belonging"))



## Changing factor score to NA if did not have a valid raw score (need a minimum of 3 responses to the scale)
# column names for factor and raw scores
fs <- paste0(rep(c("sh", "dep", "emp", "del", "su", "schb"), each = 4), 1:4)
raw <- paste0(rep(c("sh", "dep", "emp", "del", "su", "schb"), each = 4), paste0("_scale_W", 1:4))

# column bound into one dataframe
all.fs.temp <- as.data.frame(factor.scores)
names(all.fs.temp) <- fs

all.fs <- map2_dfc(.x = raw, .y = fs,
                   ~ifelse(is.na(sswide[[.x]]), NA, all.fs.temp[[.y]]))
names(all.fs) <- fs

## adding factor scores to dataset
sswidewfs <- bind_cols(sswide, all.fs)



#### Saving Output for analysis ####
save(longIndNames, sh.strict, dep.strict, 
     emp.strict, del.strict, su.strict, schb.strict,
     factor.scores, sswidewfs, all.relis,
     file = "Output/MI_For_Analysis.RData")

# load("Output/MI_For_Analysis.RData")
