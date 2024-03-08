##############################################
#                                            #
#           Paper 1 - Abuse and SH           #
#               Output Analysis              #
#                                            #
##############################################

#Note. using std for standardization - standardizes latent variables
#  - One point increase in predictor associated with y standard deviation change in SH perp
# see p. 800 in Mplus guide: https://www.statmodel.com/download/usersguide/MplusUserGuideVer_8.pdf


source("Scripts/0-PnF.R")
load("Output/MI_For_Analysis.RData")
load("Output/SS_Data_For_Analysis_wFS.RData")

# mpluswide2 <- readr::read_csv("mplus files/HHE/mpluswide_colnames.csv", 
#                       na = "-999")
# table(sswidewfs$High_Belong, useNA = "always")
# table(mpluswide2$SchBel, useNA = "always")


####################################################
####     Unconditional Latent Growth Model      ####

# ----  Linear v. Quadratic - Full Sample ---------------------

# in alphabetical order because that is how readModels organizes them
mplusoutcomes = c("Sexual Harassment", "Sexual Harassment - Quadratic")
mpluslgmpred <- c(I = "Intercept", S = "Intercept")

## Importing models and converting results to tables
lgm.ss.mplus <- MplusAutomation::readModels(target = "mplus files/Latent Growth")
lgm.ss.mplus.fit <- map_dfr(lgm.ss.mplus, ~mplus_fit(.x, digits = 2), .id = "model") %>%
  left_join(map_dfr(lgm.ss.mplus, ~.x$summaries[,25:27], .id = "model"), by = "model") %>% # adding AIC and BIC
  rename(x2 = "chisq", p = "pvalue") %>%
  bind_rows(cbind(data.frame(model = "difference"), mplus_sb2001(lgm.ss.mplus[[1]], lgm.ss.mplus[[2]])) %>%
              rename_with(.cols = starts_with("delta"), ~str_remove(., "delta.")))
lgm.ss.mplus.est <- map2_dfr(lgm.ss.mplus, mplusoutcomes, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized") %>%
                               mutate(outcome = .y)) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-contains("WITH"), contains("WITH"))


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



# ----  Linear LGM stratified by group  --------------------------------

# in alphabetical order because that is how readModels organizes them
mplusstratgroups = c("Black", "Girls", "School Belonging - High", "Hispanic", "Boys", "School Belonging - Moderate", "Other Race", "White")

## Importing models and converting results to tables
lgm.strat <- MplusAutomation::readModels(target = "mplus files/Latent Growth/Stratified")
lgm.strat.fit <- map_dfr(lgm.strat, ~mplus_fit(.x, digits = 2), .id = "outcome") %>%
  mutate(outcome = mplusstratgroups)
lgm.strat.est <- map2_dfr(lgm.strat, mplusstratgroups, ~mplus_est(.x, params = c("Means", "^Variances", "WITH"), std = "unstandardized") %>%
                            mutate(outcome = .y)) %>%
  select(outcome, estimate, parameter) %>%
  spread(parameter, estimate) %>%
  select(-S.WITH_I, S.WITH_I)

#### Compiling results objects into one object ####
lgm.strat.results <- list(Models = lgm.strat,
                          Fit = lgm.strat.fit,
                          Estimates = lgm.strat.est,
                          R2 = map2_dfr(.x = lgm.strat, .y = mplusstratgroups,
                                        ~.x$parameters$r2 %>% mutate(outcome = .y)),
                          Warn_Error = map2_dfr(.x = lgm.strat, .y = mplusstratgroups,
                                                ~data.frame(outcome = .y,
                                                            errors = length(.x$errors),
                                                            warnings = length(.x$warnings))))

# Note: Slope variance for High Belonging group constrained to 0 

##############################################################

##########################################################
####            Hostile Home Environment              ####

hhe.names <- c(I = "Intercept term", S = "Intercept term",
               FAMCON = "Family Conflict", ABUSE = "Abuse", SIBAGG = "Sibling Aggression",
               SHVICT = "SH Victimization")

## Importing models and converting results to tables
hhe.mods <- MplusAutomation::readModels(target = "mplus files/HHE")
hhe.fit <- mplus_fit(hhe.mods, digits = 2)
hhe.est <- mplus_est(hhe.mods, params = c("ON", "Intercepts"), std = "unstandardized") %>%
  filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
  mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
         predictor = recode(param, !!!hhe.names) %>%
           factor(., levels = hhe.names[-1])) %>%  # the [-1] is specific to latent growth models where I and S recoded to same value
  select(predictor, is, estimate) %>%
  spread(is, estimate) %>%
  bind_rows(mplus_rer2(hhe.mods)[2,]) %>%    # Add r2
  rename(Intercept = "I", Slope = "S")

hhe.stdest <- mplus_est(hhe.mods, params = c("ON", "Intercepts"), std = "stdyx.standardized") %>%
  filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
  mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
         predictor = recode(param, !!!hhe.names) %>%
           factor(., levels = hhe.names[-1])) %>%  # the [-1] is specific to latent growth models where I and S recoded to same value
  select(predictor, is, estimate) %>%
  spread(is, estimate) %>%
  bind_rows(mplus_rer2(hhe.mods)[2,]) %>%    # Add r2
  rename(Intercept = "I", Slope = "S")


#### Compiling results objects into one object ####
hhe.results <- list(Models = hhe.mods,
                    Fit = hhe.fit,
                    Estimates = hhe.est,
                    Std.Estimates = hhe.stdest,
                    R2 = hhe.mods$parameters$r2,
                    Warn_Error = data.frame(errors = length(hhe.mods$errors),
                                            warnings = length(hhe.mods$warnings)))


##############################################################


##########################################################
####            Multigroup Analyses                   ####

mg.shortcut <- function(model, wald, std){
  est <- mplus_est(model, params = c("ON", "Intercepts"), std = std) %>%
    filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
    mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
           predictor = recode(param, !!!hhe.names) %>%
             factor(., levels = hhe.names[-1])) %>%
    unite(col = "Parameter", is, predictor) %>%
    select(Parameter, estimate, Group) %>%
    spread(Group, estimate) %>%
    left_join(select(wald, -df), by = c("Parameter" = "Test")) %>%
    bind_rows(data.frame(Parameter = c("Intercept", "Slope"))) %>%
    mutate(Parameter = factor(Parameter, levels = c("Intercept", mg.var.names[1:4], "Slope", mg.var.names[5:8]))) %>%
    arrange(Parameter) %>%
    filter(!is.na(Parameter)) # removing SHVict estimates as these were not tested for differences (just used as control variable)
  #mutate(padj = p.adjust(p, method = "fdr")) # adjusting p-values for multiple comparisons (NAs are ignored)
  
}


mg.var.names <- c(int = "I_Intercept term", ifc = "I_Family Conflict", iab = "I_Abuse", isa = "I_Sibling Aggression",
                  slope = "S_Intercept term", sfc = "S_Family Conflict", sab = "S_Abuse", ssa = "S_Sibling Aggression")

# -------- By Gender  ---------------

## Importing Models
# Same model in each file; only the parameter constraint tested differs
mg.gen <- MplusAutomation::readModels(target = "mplus files/HHE/Multigroup/Gender")

# Model fit - will be the same for all models
gen.fit <- mplus_fit(mg.gen[[1]], digits = 2)

# wald test for group differences
gen.wald <- map_dfr(mg.gen, ~.x$summaries %>%
                      select(Wald = WaldChiSq_Value, df = WaldChiSq_DF, p = WaldChiSq_PValue), .id = "Test") %>%
  mutate(Test = str_remove(Test, "sh.lgm.mg.gender.") %>% str_remove(".out") %>%
           recode(!!!mg.var.names))


# Parameter estimates - will be the same for each file,
# then joining corresponding wald test
gen.est <- mg.shortcut(mg.gen[[1]], wald = gen.wald, std = "unstandardized") %>%
  select(Parameter, Girl = "FEMALE", Boy = "MALE", everything())

gen.stdest <- mg.shortcut(mg.gen[[1]], wald = gen.wald, std = "stdyx.standardized") %>%
  select(Parameter, Girl = "FEMALE", Boy = "MALE", everything())

#### Compiling results objects into one object ####
gen.results <- list(Models = mg.gen,
                    Fit = gen.fit,
                    Estimates = gen.est,
                    Std.Estimates = gen.stdest,
                    Wald = gen.wald,
                    Warn_Error = map_dfr(.x = names(mg.gen),
                                         ~data.frame(outcome = .x,
                                                     errors = length(mg.gen[[.x]]$errors),
                                                     warnings = length(mg.gen[[.x]]$warnings))))



# -------- By Race  ---------------

## Importing Models
# Same model in each file; only the parameter constraint tested differs
mg.race <- MplusAutomation::readModels(target = "mplus files/HHE/Multigroup/Race")

# Model fit - will be the same for all models
race.fit <- mplus_fit(mg.race[[1]], digits = 2)

# wald test for group differences
race.wald <- map_dfr(mg.race, ~.x$summaries %>%
                       select(Wald = WaldChiSq_Value, df = WaldChiSq_DF, p = WaldChiSq_PValue), .id = "Test") %>%
  mutate(Test = str_remove(Test, "sh.lgm.mg.race.") %>% str_remove(".out") %>%
           recode(!!!mg.var.names))


# Parameter estimates - will be the same for each file,
# then joining corresponding wald test
race.est <- mg.shortcut(mg.race[[1]], wald = race.wald, std = "unstandardized") %>%
  select(Parameter, Black = "BLACK", White = "WHITE", Hispanic = "HISPANIC", everything())

race.stdest <- mg.shortcut(mg.race[[1]], wald = race.wald, std = "stdyx.standardized") %>%
  select(Parameter, Black = "BLACK", White = "WHITE", Hispanic = "HISPANIC", everything())

#### Compiling results objects into one object ####
race.results <- list(Models = mg.race,
                     Fit = race.fit,
                     Estimates = race.est,
                     Std.Estimates = race.stdest,
                     Wald = race.wald,
                     Warn_Error = map_dfr(.x = names(mg.race),
                                          ~data.frame(outcome = .x,
                                                      errors = length(mg.race[[.x]]$errors),
                                                      warnings = length(mg.race[[.x]]$warnings))))

# -------- School Belonging  ---------------

## checking for outliers
# standardized slope estimate is abnormally large (slope variance is 0 so I'm not even sure how a standardized coefficient can be calculated)
# Nonetheless, as a sensitivity check, removing case 232178 did not change results
# sswidewfs %>% filter(High_Belong == 1) %>%
#   select(subjno, Gender, Race4, sh_scale_W1:sh_scale_W4) %>% View()

## Importing Models
# Same model in each file; only the parameter constraint tested differs
mg.bel <- MplusAutomation::readModels(target = "mplus files/HHE/Multigroup/SB")

# Model fit - will be the same for all models
bel.fit <- mplus_fit(mg.bel[[1]], digits = 2)

# wald test for group differences
bel.wald <- map_dfr(mg.bel, ~.x$summaries %>%
                      select(Wald = WaldChiSq_Value, df = WaldChiSq_DF, p = WaldChiSq_PValue), .id = "Test") %>%
  mutate(Test = str_remove(Test, "sh.lgm.mg.sb.") %>% str_remove(".out") %>%
           recode(!!!mg.var.names))


# Unstandardized parameter estimates - High Belonging slope variance is constrained to 0 so standardized results don't have meaning
# When slope variance was free, the model produced a non-positive definitive matrix (inadmissible solution)
# estimates will be the same for each file, so only pulling from the first; then joining corresponding wald test
bel.est <- mg.shortcut(mg.bel[[1]], wald = bel.wald, std = "unstandardized") %>%
  rename(High = "HIGH", Moderate = "MODERATE")

bel.stdest <- mg.shortcut(mg.bel[[1]], wald = bel.wald, std = "stdyx.standardized") %>%
  rename(High = "HIGH", Moderate = "MODERATE")

#### Compiling results objects into one object ####
bel.results <- list(Models = mg.bel,
                    Fit = bel.fit,
                    Estimates = bel.est,
                    Std.Estimates = bel.stdest,
                    Wald = bel.wald,
                    Warn_Error = map_dfr(.x = names(mg.bel),
                                         ~data.frame(outcome = .x,
                                                     errors = length(mg.bel[[.x]]$errors),
                                                     warnings = length(mg.bel[[.x]]$warnings))))


####################################################################



#################################################################
####      Wave 1 Predictors of Growth With Interactions      ####

w1p.shortcut <- function(results, nms, mods){
  
  if(length(results$Estimates)==2){
    
    table <- inner_join(results$Estimates[[1]], results$Estimates[[2]], # join fixed effects
                        by = "predictor", suffix = paste0(".", nms)) %>%
      bind_rows(map(mods, ~mplus_rer2(.x)) %>%                                             # Add random effect and r2
                  reduce(inner_join, by = "predictor", suffix = paste0(".", nms))) %>%
      bind_rows(map(nms, ~results$Model_Fit %>%                                           # Add joined fit indices
                      filter(Outcome == .x) %>%
                      mutate(x2 = case_when(pvalue < .01 ~ paste0(chisq, "**"),
                                            pvalue < .05 ~ paste0(chisq, "*"),
                                            TRUE ~ chisq),
                             RMSEA = paste(rmsea, rmsea.ci)) %>%
                      select(x2, n = ntotal, CFI = cfi, RMSEA, SRMR = srmr) %>%
                      gather(predictor, I)) %>%
                  reduce(inner_join, by = "predictor", suffix = paste0(".", nms)))
  } else {
    
    table <- reduce(results$Estimates, left_join, by = "predictor") %>%        # join fixed effects
      set_names(c("predictor", paste(rep(c("I", "S"), times = 3), rep(nms, each = 2), sep = "."))) %>%
      bind_rows(map(mods, ~mplus_rer2(.x)) %>%                                             # Add random effect and r2
                  reduce(inner_join, by = "predictor") %>%
                  set_names(c("predictor", paste(rep(c("I", "S"), times = 3), rep(nms, each = 2), sep = ".")))) %>%
      bind_rows(map(nms, ~results$Model_Fit %>%                                           # Add joined fit indices
                      filter(Outcome == .x) %>%
                      mutate(x2_31.25.31 = case_when(pvalue < .01 ~ paste0(chisq, "**"),
                                                     pvalue < .05 ~ paste0(chisq, "*"),
                                                     TRUE ~ chisq),
                             RMSEA = paste(rmsea, rmsea.ci)) %>%
                      select(x2_31.25.31, n = ntotal, CFI = cfi, RMSEA, SRMR = srmr) %>%
                      gather(predictor, I)) %>%
                  reduce(inner_join, by = "predictor") %>%
                  set_names(c("predictor", paste("I", nms, sep = "."))))
    
  }
}

## Importing models
w1p.mods <- MplusAutomation::readModels(target = "mplus files/With SH Vict")

w1p.var.names <- c(I = "Intercept term", S = "Intercept term",
                   FAMCON = "Family Conflict (FC)", ABUSE = "Abuse", SIBAGG = "Sibling Aggression (SA)",
                   SHVICT = "SH Victim", FEMALE = "Girls", BLACK = "Black", WHITE = "White", OTHERR = "Other Race",
                   SCHBEL = "High School Belonging (HSB)", GRADES = "Academic Grades",
                   SBXFC = "FC x HSB", SBXAB = "Abuse x HSB", SBXSA = "SA x HSB",
                   AGXFC = "FC x AG", AGXAB = "Abuse x AG", AGXSA = "SA x AG")

# ------    Full Sample    -------------------
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
full.w1p.results <- format_mplus(w1p.mods[length(w1p.mods)], recodes = w1p.var.names, outcomes = "Full Sample",
                                 std = "stdyx.standardized", reg = TRUE, r2 = TRUE)

# -----    Gender    --------------

## group names
gen.names <- c("Girls", "Boys") # order must match order of models in w1p.mods

#### Unstandardized
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
gen.w1p.results <- format_mplus(w1p.mods[c(2,5)], recodes = w1p.var.names[-7], outcomes = gen.names,
                                std = "unstandardized", reg = TRUE, r2 = TRUE)

## Create output table
gen.w1p.results.table <- w1p.shortcut(gen.w1p.results, gen.names, w1p.mods[c(2, 5)])

#### Standardized 
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
gen.w1p.results.std <- format_mplus(w1p.mods[c(2,5)], recodes = w1p.var.names[-7], outcomes = gen.names,
                                    std = "stdyx.standardized", reg = TRUE, r2 = TRUE)

## Create output table
gen.w1p.results.table.std <- w1p.shortcut(gen.w1p.results.std, nms= gen.names, w1p.mods[c(2, 5)])



# -----    Race    --------------

## group names
race.names <- c("Black", "White", "Hispanic") # order must match order of models in w1p.mods

#### Unstandardized
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
race.w1p.results <- format_mplus(w1p.mods[c(1, 7, 4)], recodes = w1p.var.names[-c(8:10)], outcomes = race.names,
                                 std = "unstandardized", reg = TRUE, r2 = TRUE)

## Create output table
race.w1p.results.table <- w1p.shortcut(race.w1p.results, race.names, w1p.mods[c(1, 7, 4)])

#### Standardized 
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
race.w1p.results.std <- format_mplus(w1p.mods[c(1, 7, 4)], recodes = w1p.var.names[-c(8:10)], outcomes = race.names,
                                     std = "stdyx.standardized", reg = TRUE, r2 = TRUE)

## Create output table
race.w1p.results.table.std <- w1p.shortcut(race.w1p.results.std, race.names, w1p.mods[c(1, 7, 4)])



# -----    School Belonging    --------------

#Note. High Belonging slope variance is constrained to 0 so standardized results don't have meaning
# When slope variance was free, the model produced a non-positive definitive matrix (inadmissible solution)

## group names
bel.names <- c("High", "Moderate") # order must match order of models in w1p.mods

#### Unstandardized
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
bel.w1p.results <- format_mplus(w1p.mods[c(3, 6)], recodes = w1p.var.names[-c(11, 13:15)], outcomes = bel.names,
                                std = "unstandardized", reg = TRUE, r2 = TRUE)

## Create output table
bel.w1p.results.table <- w1p.shortcut(bel.w1p.results, bel.names, w1p.mods[c(3, 6)])

#### Standardized 
## Gather dataframe of fits, list of dataframes for parameter estimates, dataframe for R2, and warn/errors
bel.w1p.results.std <- format_mplus(w1p.mods[c(3, 6)], recodes = w1p.var.names[-c(11, 13:15)], outcomes = bel.names,
                                    std = "stdyx.standardized", reg = TRUE, r2 = TRUE)

## Create output table
bel.w1p.results.table.std <- w1p.shortcut(bel.w1p.results.std, bel.names, w1p.mods[c(3, 6)])


#####################################################################



##############################################
####      Additional Plots and Values     ####



# ---- Outcome by Group from Multigroup Analysis  ------

# SHplots <- map2(.x = list(gen.results$Plot_Data, race.results$Plot_Data, bel.results$Plot_Data),
#                 .y = c("Sexual Harassment Perpetration", "", ""),
#                 ~multigroup_plot(.x, ylab = .y, max.y = 6) +
#                   theme(legend.position = "top",
#                         legend.key.width = unit(1,"cm")))
# 
# SHPlot.grid <- cowplot::plot_grid(plotlist = SHplots, align = "h", nrow = 1, labels = "AUTO")

# ----      Interaction Plots     ---------

####  Data Prep ####

## extracting means, intercept, and slope estimates for all models
intx.data <- map2(.x = w1p.mods[-c(3, 6)],
                  .y = c(rep(TRUE, 4), FALSE, TRUE),
                  ~intx_dat_shortcut(.x, grades = .y)) %>%
  set_names(c("Black", "Girl", "Hispanic", "Boy", "White", "Full"))

## calculating trajectories for Belonging interactions
hsb.intx.plot.data <- map_dfr(intx.data, ~intx_plot_prep(.x$HSB, HSB), .id = "Group") %>%
  mutate(HSB = recode(HSB, `0` = "Moderate", `1` = "High"),
         ACE = str_replace(ACE, "_", " "))

## calculating trajectories for Grades interactions
ag.intx.plot.data <- map_dfr(intx.data[-5], ~intx_plot_prep(.x$AG, Grade_Cat), .id = "Group") %>%
  # mutate(Grade_Cat = recode(Grade_Cat, `Means` = "Mean") %>% factor(., levels = c("High", "Mean", "Low")))
  mutate(ACE = str_replace(ACE, "_", " "))

#### Creating Plots ####

## Abuse and SA by Grades for Girls
fem.absa.intx.plot <- ag.intx.plot.data %>%
  filter(Group %in% c("Girl") & ACE %in% c("Sibling Aggression", "Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.2, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "HHE") +
  theme_bw(base_size = 20) +
  facet_grid(ACE ~ Grade_Cat)

## Abuse by Grades for Hispanic youth
hisp.abag.intx.plot <- ag.intx.plot.data %>%
  filter(Group %in% c("Hispanic") & ACE %in% c("Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.1, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Abuse") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ Grade_Cat)

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
  scale_y_continuous(limits = c(-.05, 6), breaks = seq(0, 6, .5)) +
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

## FC by HSB for Whites
white.fc.intx.plot <- hsb.intx.plot.data %>%
  filter(Group %in% c("White") & ACE %in% c("Family Conflict")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-.3, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Family\nConflict") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ HSB)

## Abuse by HSB for Males
male.ab.intx.plot <- hsb.intx.plot.data %>%
  filter(Group %in% c("Boy") & ACE %in% c("Abuse")) %>%
  ggplot(aes(x = Year, y = Estimate, group = Level)) +
  geom_line(aes(linetype = Level), size = 1) +
  scale_linetype_manual(values = c(1, 2, 3, 4)) +
  scale_y_continuous(limits = c(-0.1, 6), breaks = seq(0, 6, .5)) +
  scale_x_continuous(limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
  labs(x = "Year", y = "Sexual Harassment Perpetration", linetype = "Abuse") +
  theme_bw(base_size = 20) +
  facet_grid(. ~ HSB)


##################################################################

# ------  Saving Raw Scale Score Growth Objects ------------------

save(mplusoutcomes, mpluslgmpred, mplusstratgroups,
     lgm.ss.mplus, lgm.ss.mplus.results,
     lgm.strat, lgm.strat.results, hhe.names, hhe.results,
     mg.var.names, gen.results, race.results, bel.results,
     w1p.mods, w1p.var.names, full.w1p.results,
     gen.names, gen.w1p.results, gen.w1p.results.table, gen.w1p.results.std, gen.w1p.results.table.std,
     race.names, race.w1p.results, race.w1p.results.table, race.w1p.results.std, race.w1p.results.table.std,
     bel.names, bel.w1p.results, bel.w1p.results.table, bel.w1p.results.std, bel.w1p.results.table.std,
     #SHplots, SHPlot.grid,
     intx.data, hsb.intx.plot.data, ag.intx.plot.data,
     fem.absa.intx.plot, hisp.abag.intx.plot, black.fcsa.intx.plot,
     black.fc.intx.plot, male.ab.intx.plot,
     white.fc.intx.plot, white.sa.intx.plot, 
     file = "Output/Scale_Score_LGM_Results.RData")

# load("Output/Scale_Score_LGM_Results.RData")
