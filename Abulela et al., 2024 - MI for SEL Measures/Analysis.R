#############################################
#                                           #
#   MI of SEL Measures Across Time Points   #
#                 Analysis                  #
#                                           #
#############################################

# Note on RMSEAd: Using the scaled chisquare does not make sense
# because the constrained model can have a lower chisquare than the unsconstrained

# # g8.pio.results$Models$Threshold
# 54         # df
# .487       # scaling factor
# 43344.972  # scaled chisquare
# 21126.624  # naive chisquare
# 
# # g8.pio.results$Models$Loading
# 69
# .683
# 34902.259
# 23831.226



####  Importing Data, Packages, and Functions ####

source("PnF.R")
mss.orig <- haven::read_sav("MSS2013-2022-MyDrG-20230807.sav")
# names(mss.orig)

# Create_Codebook(mss.orig, export_type = "excel", export_name = "MSS2013-2022-MyDrG-20230807 Codebook")

## subsetting variables needed for analysis
mss.sub <- mss.orig %>%
  select(Year, studentnumber:ONLINE2022, Sex, Grade, Age,
         Race2022, racegroups19, Y4a:A8F, # Race/Ethnicity variables
         Y6a:Y6m, # who do you live with
         FRL = Y12r, Y9:W7, # gender, sexual orientation, and presentation
         Grades, CtL19:FV19,  # Rasch scores
         all_of(c(ctl.items, pio.l1.items, pio.l23.items, sc.items,  # developmental skills
                  emp.items, fcs.items, tss.items))) %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022")),
         CtL_raw = scale_score(., items = ctl.items, type = "mean", min.valid = length(ctl.items)),
         PIO_l1_raw = scale_score(., items = pio.l1.items, type = "mean", min.valid = length(pio.l1.items)),
         PIO_l23_raw = scale_score(., items = pio.l23.items, type = "mean", min.valid = length(pio.l23.items)),
         SC_raw = scale_score(., items = sc.items, type = "mean", min.valid = length(sc.items)),
         EMP_raw = scale_score(., items = emp.items, type = "mean", min.valid = length(emp.items)),
         FCS_raw = scale_score(., items = fcs.items, type = "mean", min.valid = length(fcs.items)),
         TSS_raw = scale_score(., items = tss.items, type = "mean", min.valid = length(tss.items)))

## We are running separate analysis by grade
mss <- map(.x = c(5, 8, 9, 11), ~filter(mss.sub, Grade == .x)) %>%
  set_names(paste0("Grade_", c(5, 8, 9, 11)))

# nrow(mss$Grade_5)
table(mss.sub$Year, useNA = "always")

##########################################################

####  Demographics  ####

demo.data <- mss.sub %>%
  filter(if_any(.cols = c(CtL_raw:TSS_raw), ~!is.na(.))) %>%
  select(Year, Age, Region, Grade, Sex, FRL, Race = racegroups19,
         Y9, P7, A12, # Sexual orientation for 13/16, 19, 22; need to combine
         W6, P5, A10A:A10J) %>% # transgender for 16/19; code A10 into transgender for 22
  mutate(A_NA = rowSums(is.na(.[,paste0("A10", LETTERS[1:10])])),
         t22 = case_when(A10A == 1 ~ 1,
                         A10C == 1 ~ 1,
                         A10D == 1 ~ 1,
                         A10F == 1 ~ 1,
                         A10G == 1 ~ 1,
                         A10H == 1 ~ 1,
                         A10I == 1 ~ 1,
                         A10J == 1 ~ 1,
                         A_NA  == 10 ~ NA_integer_,
                         TRUE ~ 0)) %>%
  mutate(Transgender = case_when(W6 == 2 ~ "Yes",
                                 P5 %in% c(1,3) ~ "Yes",
                                 P5 == 4 ~ NA_character_,
                                 t22 == 1 ~ "Yes",
                                 rowSums(is.na(.[,c("W6", "P5","t22")])) == 3 ~ NA_character_,
                                 TRUE ~ "No"),
         Sex_Or = case_when(!is.na(Y9) ~ as_factor(Y9),
                            !is.na(P7) ~ as_factor(P7),
                            !is.na(A12) ~ as_factor(A12),
                            rowSums(is.na(.[,c("Y9", "P7","A12")])) == 3 ~ NA_character_) %>%
           fct_recode(., `Gay or Lesbian` = "Gay or lesbian",
                      `Straight (heterosexual)` = "Heterosexual (straight)",
                      `Questioning/Not sure` = "Not sure (questioning)", `Questioning/Not sure` = "Questioning/not sure",
                      NULL = "I am not sure what this question means",
                      `None of these ways` = "I don’t describe myself in any of these ways",
                      `None of these ways` = "I don't describe myself in any of these ways"),
         Race = as_factor(Race) %>% fct_recode(NULL = "Missing"))
  
demos.year <- demo.data %>%
  select(Year:Race, Transgender, Sex_Or) %>%
  mutate(across(.cols = c(Region:Sex_Or), .fn = as_factor)) %>%
  gtsummary::tbl_summary(data = ., by = "Year",
                         statistic = list(Age ~ "{mean} ({sd})")) %>%
  gtsummary::add_overall() %>%
  gtsummary::as_flex_table()

demos.grade <- demo.data %>%
  select(Year:Race, Transgender, Sex_Or) %>%
  mutate(across(.cols = c(Year, Region:Sex_Or), .fn = as_factor)) %>%
  gtsummary::tbl_summary(data = ., by = "Grade",
                         statistic = list(Age ~ "{mean} ({sd})")) %>%
  gtsummary::add_overall() %>%
  gtsummary::as_flex_table()


####  Checking Trends in Raw and Rasch Scores ####

## Grabbing scores and converting to long format for plotting
mss.scores <- bind_rows(mss.sub %>%
  select(studentnumber, Year, Grade, CtL19:TSS19) %>%
  tidyr::gather("measure", "score", CtL19:TSS19) %>%
    mutate(Type = "Rasch"),
  mss.sub %>%
    select(studentnumber, Year, Grade, CtL_raw:TSS_raw) %>%
    tidyr::gather("measure", "score", CtL_raw:TSS_raw) %>%
    mutate(Type = "Raw"))

## computing means
mss.means <- mss.scores %>%
  group_by(Grade, Year, Type, measure)  %>%
  summarize(mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(across(.cols = c(Grade, Year), factor))

## Line Plots
score.plots <- map(.x = c("Rasch", "Raw"),
                   ~mss.means %>%
                     filter(Type == .x) %>%
                     ggplot(aes(x = Year, y = mean, group = Grade, color = Grade)) +
                     geom_line(linewidth = 2) +
                     scale_color_brewer(palette = "Set2") +
                     labs(y = "Score Mean") +
                     theme_bw(base_size = 18) +
                     facet_wrap(~measure)) %>%
  set_names(c("Rasch", "Raw"))
  
## Across all measures, something up is with grade 8 in the MGCFA, specifically 2016 and 2019;
## 2022 seems right relative to 2013
## other grades/years/measures seem right
## problem appears to be that in Grade 8, Year factor order was 2013, 2019, 2016, 2022

############################################################


## model identification settings - will be used for all measurement invariance models run with semTools::measEq.syntax
mi.settings <- list(ordered = TRUE, meanstructure = TRUE,
                    parameterization = "delta", # variance of latent item response = 1; residual variance estimated
                    ID.fac = "std.lv",
                    ID.cat = "Wu.Estabrook.2016") #, return.fit = TRUE


# -----  Grade 5  ------

## Defining base structure
g5.base <- map2(.x = abb.scales, .y = l1.scales,
                ~paste(.x, "=~", paste(.y, collapse = " + ")))

####  Commitment to Learning ####

## check response values for miscodes or lack of variance
lapply(ctl.items, function(x) class(mss$Grade_5[[x]]))
lapply(ctl.items, function(x) table(mss$Grade_5[[x]], useNA = "always"))

## Generating model syntax
g5.ctl.syntax <- generate_mi_syntax(g5.base$`Commitment to Learning`, data = mss$Grade_5,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g5.ctl.mods <- list(`2013 Only` = cfa(g5.base$`Commitment to Learning`, data = filter(mss$Grade_5, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g5.base$`Commitment to Learning`, data = filter(mss$Grade_5, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g5.base$`Commitment to Learning`, data = filter(mss$Grade_5, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g5.base$`Commitment to Learning`, data = filter(mss$Grade_5, Year == 2022), ordered = TRUE),
                    Configural = cfa(g5.ctl.syntax$Configural, data = mss$Grade_5, group = "Year"),
                    Threshold = cfa(g5.ctl.syntax$Threshold, data = mss$Grade_5, group = "Year"),
                    Loading = cfa(g5.ctl.syntax$Loading, data = mss$Grade_5, group = "Year"),
                    Means = cfa(g5.ctl.syntax$Means, data = mss$Grade_5, group = "Year"))

## comparing configural, Threshold, and Loading models
g5.ctl.comps <- bind_rows(compare_mods(g5.ctl.mods$Configural, g5.ctl.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g5.ctl.mods$Threshold, g5.ctl.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g5.ctl.fits <- fits_wrapper(g5.ctl.mods) %>%
  left_join(g5.ctl.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g5.ctl.loads <- loadings_wrapper(mods.list = g5.ctl.mods, items = l1.scales$`Commitment to Learning`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g5.ctl.mean.var <- grp_mvs(g5.ctl.mods$Means, "CTL")

## Parameters to investigate partial invariance
g5.ctl.partial <- partial_params(g5.ctl.mods$Loading)

## Compiling results into list object
g5.ctl.results <- list(Base = g5.base$`Commitment to Learning`,
                       Syntax = g5.ctl.syntax,
                       Models = g5.ctl.mods,
                       Fits = g5.ctl.fits,
                       Loadings = g5.ctl.loads,
                       Means_Vars = g5.ctl.mean.var,
                       Partial = g5.ctl.partial)

#######################################################################

####  Positive Identity and Outlook ####

## remove 2013 and just run 3 years

## check response values for miscodes or lack of variance
lapply(pio.l1.items, function(x) class(mss$Grade_5[[x]]))
lapply(pio.l1.items, function(x) table(mss$Grade_5$Year, mss$Grade_5[[x]], useNA = "always"))

## Generating model syntax
g5.pio.syntax <- generate_mi_syntax(g5.base$`Positive Identity and Outlook`, data = filter(mss$Grade_5, Year != 2013),
                                    group = "Year", mi.settings = mi.settings)

## Running models
g5.pio.mods <- list(#`2013 Only` = cfa(g5.base$`Positive Identity and Outlook`, data = filter(mss$Grade_5, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g5.base$`Positive Identity and Outlook`, data = filter(mss$Grade_5, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g5.base$`Positive Identity and Outlook`, data = filter(mss$Grade_5, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g5.base$`Positive Identity and Outlook`, data = filter(mss$Grade_5, Year == 2022), ordered = TRUE),
                    Configural = cfa(g5.pio.syntax$Configural, data = filter(mss$Grade_5, Year != 2013), group = "Year"),
                    Threshold = cfa(g5.pio.syntax$Threshold, data = filter(mss$Grade_5, Year != 2013), group = "Year"),
                    Loading = cfa(g5.pio.syntax$Loading, data = filter(mss$Grade_5, Year != 2013), group = "Year"),
                    Means = cfa(g5.pio.syntax$Means, data = filter(mss$Grade_5, Year != 2013), group = "Year"))


## comparing configural, Threshold, and Loading models
g5.pio.comps <- bind_rows(compare_mods(g5.pio.mods$Configural, g5.pio.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g5.pio.mods$Threshold, g5.pio.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g5.pio.fits <- fits_wrapper(g5.pio.mods) %>%
  left_join(g5.pio.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g5.pio.loads <- loadings_wrapper(mods.list = g5.pio.mods, items = l1.scales$`Positive Identity and Outlook`) %>%
  rename(Y2016 = "1", Y2019 = "2", Y2022 = "3")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g5.pio.mean.var <- grp_mvs(g5.pio.mods$Means, "PIO") %>%
  mutate(Year = c("2016", "2019", "2022"))

## Parameters to investigate partial invariance
g5.pio.partial <- partial_params(g5.pio.mods$Loading)

## Compiling results into list object
g5.pio.results <- list(Base = g5.base$`Positive Identity and Outlook`,
                       Syntax = g5.pio.syntax,
                       Models = g5.pio.mods,
                       Fits = g5.pio.fits,
                       Loadings = g5.pio.loads,
                       Means_Vars = g5.pio.mean.var,
                       Partial = g5.pio.partial)

##################################################################

####  Social Competence ####

## check response values for miscodes or lack of variance
lapply(sc.items, function(x) class(mss$Grade_5[[x]]))
lapply(sc.items, function(x) table(mss$Grade_5$Year, mss$Grade_5[[x]], useNA = "always"))

## Generating model syntax
g5.sc.syntax <- generate_mi_syntax(g5.base$`Social Competence`, data = mss$Grade_5,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g5.sc.mods <- list(`2013 Only` = cfa(g5.base$`Social Competence`, data = filter(mss$Grade_5, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g5.base$`Social Competence`, data = filter(mss$Grade_5, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g5.base$`Social Competence`, data = filter(mss$Grade_5, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g5.base$`Social Competence`, data = filter(mss$Grade_5, Year == 2022), ordered = TRUE),
                    Configural = cfa(g5.sc.syntax$Configural, data = mss$Grade_5, group = "Year"),
                    Threshold = cfa(g5.sc.syntax$Threshold, data = mss$Grade_5, group = "Year"),
                    Loading = cfa(g5.sc.syntax$Loading, data = mss$Grade_5, group = "Year"),
                   Means = cfa(g5.sc.syntax$Means, data = mss$Grade_5, group = "Year"))


## comparing configural, Threshold, and Loading models
g5.sc.comps <- bind_rows(compare_mods(g5.sc.mods$Configural, g5.sc.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g5.sc.mods$Threshold, g5.sc.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g5.sc.fits <- fits_wrapper(g5.sc.mods) %>%
  left_join(g5.sc.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g5.sc.loads <- loadings_wrapper(mods.list = g5.sc.mods, items = l1.scales$`Social Competence`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g5.sc.mean.var <- grp_mvs(g5.sc.mods$Means, "SC")

## Parameters to investigate partial invariance
g5.sc.partial <- partial_params(g5.sc.mods$Loading)

## Compiling results into list object
g5.sc.results <- list(Base = g5.base$`Social Competence`,
                       Syntax = g5.sc.syntax,
                       Models = g5.sc.mods,
                       Fits = g5.sc.fits,
                       Loadings = g5.sc.loads,
                       Means_Vars = g5.sc.mean.var,
                       Partial = g5.sc.partial)

#######################################################################

####  Empowerment ####

## check response values for miscodes or lack of variance
lapply(emp.items, function(x) class(mss$Grade_5[[x]]))
lapply(emp.items, function(x) table(mss$Grade_5$Year, mss$Grade_5[[x]], useNA = "always"))

## Generating model syntax
g5.emp.syntax <- generate_mi_syntax(g5.base$`Empowerment`, data = mss$Grade_5,
                                   group = "Year", mi.settings = mi.settings)

## Running models
g5.emp.mods <- list(`2013 Only` = cfa(g5.base$`Empowerment`, data = filter(mss$Grade_5, Year == 2013), ordered = TRUE),
                   `2016 Only` = cfa(g5.base$`Empowerment`, data = filter(mss$Grade_5, Year == 2016), ordered = TRUE),
                   `2019 Only` = cfa(g5.base$`Empowerment`, data = filter(mss$Grade_5, Year == 2019), ordered = TRUE),
                   `2022 Only` = cfa(g5.base$`Empowerment`, data = filter(mss$Grade_5, Year == 2022), ordered = TRUE),
                   Configural = cfa(g5.emp.syntax$Configural, data = mss$Grade_5, group = "Year"),
                   Threshold = cfa(g5.emp.syntax$Threshold, data = mss$Grade_5, group = "Year"),
                   Loading = cfa(g5.emp.syntax$Loading, data = mss$Grade_5, group = "Year"),
                   Means = cfa(g5.emp.syntax$Means, data = mss$Grade_5, group = "Year"))


## comparing configural, Threshold, and Loading models
g5.emp.comps <- bind_rows(compare_mods(g5.emp.mods$Configural, g5.emp.mods$Threshold) %>% mutate(Model = "Threshold"),
                         compare_mods(g5.emp.mods$Threshold, g5.emp.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g5.emp.fits <- fits_wrapper(g5.emp.mods) %>%
  left_join(g5.emp.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g5.emp.loads <- loadings_wrapper(mods.list = g5.emp.mods, items = l1.scales$`Empowerment`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g5.emp.mean.var <- grp_mvs(g5.emp.mods$Means, "EMP")

## Parameters to investigate partial invariance
g5.emp.partial <- partial_params(g5.emp.mods$Loading)

## Compiling results into list object
g5.emp.results <- list(Base = g5.base$`Empowerment`,
                      Syntax = g5.emp.syntax,
                      Models = g5.emp.mods,
                      Fits = g5.emp.fits,
                      Loadings = g5.emp.loads,
                      Means_Vars = g5.emp.mean.var,
                      Partial = g5.emp.partial)

#######################################################################

####  Family/Community Support ####

## check response values for miscodes or lack of variance
lapply(fcs.items, function(x) class(mss$Grade_5[[x]]))
lapply(fcs.items, function(x) table(mss$Grade_5$Year, mss$Grade_5[[x]], useNA = "always"))

## Generating model syntax
g5.fcs.syntax <- generate_mi_syntax(g5.base$`Family/Community Support`, data = mss$Grade_5,
                                   group = "Year", mi.settings = mi.settings)

## Running models
g5.fcs.mods <- list(`2013 Only` = cfa(g5.base$`Family/Community Support`, data = filter(mss$Grade_5, Year == 2013), ordered = TRUE),
                   `2016 Only` = cfa(g5.base$`Family/Community Support`, data = filter(mss$Grade_5, Year == 2016), ordered = TRUE),
                   `2019 Only` = cfa(g5.base$`Family/Community Support`, data = filter(mss$Grade_5, Year == 2019), ordered = TRUE),
                   `2022 Only` = cfa(g5.base$`Family/Community Support`, data = filter(mss$Grade_5, Year == 2022), ordered = TRUE),
                   Configural = cfa(g5.fcs.syntax$Configural, data = mss$Grade_5, group = "Year"),
                   Threshold = cfa(g5.fcs.syntax$Threshold, data = mss$Grade_5, group = "Year"),
                   Loading = cfa(g5.fcs.syntax$Loading, data = mss$Grade_5, group = "Year"),
                   Means = cfa(g5.fcs.syntax$Means, data = mss$Grade_5, group = "Year"))


## comparing configural, Threshold, and Loading models
g5.fcs.comps <- bind_rows(compare_mods(g5.fcs.mods$Configural, g5.fcs.mods$Threshold) %>% mutate(Model = "Threshold"),
                         compare_mods(g5.fcs.mods$Threshold, g5.fcs.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g5.fcs.fits <- fits_wrapper(g5.fcs.mods) %>%
  left_join(g5.fcs.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g5.fcs.loads <- loadings_wrapper(mods.list = g5.fcs.mods, items = l1.scales$`Family/Community Support`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g5.fcs.mean.var <- grp_mvs(g5.fcs.mods$Means, "FCS")

## Parameters to investigate partial invariance
g5.fcs.partial <- partial_params(g5.fcs.mods$Loading)

## Compiling results into list object
g5.fcs.results <- list(Base = g5.base$`Family/Community Support`,
                      Syntax = g5.fcs.syntax,
                      Models = g5.fcs.mods,
                      Fits = g5.fcs.fits,
                      Loadings = g5.fcs.loads,
                      Means_Vars = g5.fcs.mean.var,
                      Partial = g5.fcs.partial)

#######################################################################

####  Teacher/School Support ####

## check response values for miscodes or lack of variance
lapply(tss.items, function(x) class(mss$Grade_5[[x]]))
lapply(tss.items, function(x) table(mss$Grade_5$Year, mss$Grade_5[[x]], useNA = "always"))

## Generating model syntax
g5.tss.syntax <- generate_mi_syntax(g5.base$`Teacher/School Support`, data = mss$Grade_5,
                                   group = "Year", mi.settings = mi.settings)

## Running models
g5.tss.mods <- list(`2013 Only` = cfa(g5.base$`Teacher/School Support`, data = filter(mss$Grade_5, Year == 2013), ordered = TRUE),
                   `2016 Only` = cfa(g5.base$`Teacher/School Support`, data = filter(mss$Grade_5, Year == 2016), ordered = TRUE),
                   `2019 Only` = cfa(g5.base$`Teacher/School Support`, data = filter(mss$Grade_5, Year == 2019), ordered = TRUE),
                   `2022 Only` = cfa(g5.base$`Teacher/School Support`, data = filter(mss$Grade_5, Year == 2022), ordered = TRUE),
                   Configural = cfa(g5.tss.syntax$Configural, data = mss$Grade_5, group = "Year"),
                   Threshold = cfa(g5.tss.syntax$Threshold, data = mss$Grade_5, group = "Year"),
                   Loading = cfa(g5.tss.syntax$Loading, data = mss$Grade_5, group = "Year"),
                   Means = cfa(g5.tss.syntax$Means, data = mss$Grade_5, group = "Year"))


## comparing configural, Threshold, and Loading models
g5.tss.comps <- bind_rows(compare_mods(g5.tss.mods$Configural, g5.tss.mods$Threshold) %>% mutate(Model = "Threshold"),
                         compare_mods(g5.tss.mods$Threshold, g5.tss.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g5.tss.fits <- fits_wrapper(g5.tss.mods) %>%
  left_join(g5.tss.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g5.tss.loads <- loadings_wrapper(mods.list = g5.tss.mods, items = l1.scales$`Teacher/School Support`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g5.tss.mean.var <- grp_mvs(g5.tss.mods$Means, "TSS")

## Parameters to investigate partial invariance
g5.tss.partial <- partial_params(g5.tss.mods$Loading)

## Compiling results into list object
g5.tss.results <- list(Base = g5.base$`Teacher/School Support`,
                      Syntax = g5.tss.syntax,
                      Models = g5.tss.mods,
                      Fits = g5.tss.fits,
                      Loadings = g5.tss.loads,
                      Means_Vars = g5.tss.mean.var,
                      Partial = g5.tss.partial)

#######################################################################

###########################################################################

# -----  Grade 8  ------

## Defining base structure
g8.base <- map2(.x = abb.scales, .y = l23.scales,
                ~paste(.x, "=~", paste(.y, collapse = " + ")))

####  Commitment to Learning ####

## check response values for miscodes or lack of variance
lapply(ctl.items, function(x) class(mss$Grade_8[[x]]))
lapply(ctl.items, function(x) table(mss$Grade_8[[x]], useNA = "always"))

## Generating model syntax
g8.ctl.syntax <- generate_mi_syntax(g8.base$`Commitment to Learning`, data = mss$Grade_8,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g8.ctl.mods <- list(`2013 Only` = cfa(g8.base$`Commitment to Learning`, data = filter(mss$Grade_8, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g8.base$`Commitment to Learning`, data = filter(mss$Grade_8, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g8.base$`Commitment to Learning`, data = filter(mss$Grade_8, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g8.base$`Commitment to Learning`, data = filter(mss$Grade_8, Year == 2022), ordered = TRUE),
                    Configural = cfa(g8.ctl.syntax$Configural, data = mss$Grade_8, group = "Year"),
                    Threshold = cfa(g8.ctl.syntax$Threshold, data = mss$Grade_8, group = "Year"),
                    Loading = cfa(g8.ctl.syntax$Loading, data = mss$Grade_8, group = "Year"),
                    Means = cfa(g8.ctl.syntax$Means, data = mss$Grade_8, group = "Year"))


## comparing configural, Threshold, and Loading models
g8.ctl.comps <- bind_rows(compare_mods(g8.ctl.mods$Configural, g8.ctl.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g8.ctl.mods$Threshold, g8.ctl.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g8.ctl.fits <- fits_wrapper(g8.ctl.mods) %>%
  left_join(g8.ctl.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g8.ctl.loads <- loadings_wrapper(mods.list = g8.ctl.mods, items = l23.scales$`Commitment to Learning`) %>%
  select(item, Y2013 = "1", Y2016 = "3", Y2019 = "2", Y2022 = "4", Constrained)

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g8.ctl.mean.var <- grp_mvs(g8.ctl.mods$Means, "CTL") %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022"))) %>%
  arrange(Year)

## Parameters to investigate partial invariance
g8.ctl.partial <- partial_params(g8.ctl.mods$Loading)

## Compiling results into list object
g8.ctl.results <- list(Base = g8.base$`Commitment to Learning`,
                       Syntax = g8.ctl.syntax,
                       Models = g8.ctl.mods,
                       Fits = g8.ctl.fits,
                       Loadings = g8.ctl.loads,
                       Means_Vars = g8.ctl.mean.var,
                       Partial = g8.ctl.partial)

#######################################################################

####  Positive Identity and Outlook ####

## check response values for miscodes or lack of variance
lapply(pio.l23.items, function(x) class(mss$Grade_8[[x]]))
lapply(pio.l23.items, function(x) table(mss$Grade_8$Year, mss$Grade_8[[x]], useNA = "always"))

## Generating model syntax
g8.pio.syntax <- generate_mi_syntax(g8.base$`Positive Identity and Outlook`, data = mss$Grade_8,
                                    group = "Year", mi.settings = mi.settings)

## Updating syntax for partial invariance
# g8.pio.syntax$Partial <- g8.pio.syntax$Loading %>%
#   sub("Y60n \\+ c\\(lambda.6_1, lambda.6_1, lambda.6_1, lambda.6_1",
#       "Y60n + c(lambda.6_1.g1, lambda.6_1.g2, lambda.6_1.g3, lambda.6_1.g4",.)
# g8.pio.syntax$Means <- g8.pio.syntax$Means %>%
#   sub("Y60n \\+ c\\(lambda.6_1, lambda.6_1, lambda.6_1, lambda.6_1",
#       "Y60n + c(lambda.6_1.g1, lambda.6_1.g2, lambda.6_1.g3, lambda.6_1.g4",.)


## Running models
g8.pio.mods <- list(`2013 Only` = cfa(g8.base$`Positive Identity and Outlook`, data = filter(mss$Grade_8, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g8.base$`Positive Identity and Outlook`, data = filter(mss$Grade_8, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g8.base$`Positive Identity and Outlook`, data = filter(mss$Grade_8, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g8.base$`Positive Identity and Outlook`, data = filter(mss$Grade_8, Year == 2022), ordered = TRUE),
                    Configural = cfa(g8.pio.syntax$Configural, data = mss$Grade_8, group = "Year"),
                    Threshold = cfa(g8.pio.syntax$Threshold, data = mss$Grade_8, group = "Year"),
                    Loading = cfa(g8.pio.syntax$Loading, data = mss$Grade_8, group = "Year"),
                    # Partial = cfa(g8.pio.syntax$Partial, data = mss$Grade_8, group = "Year"),
                    Means = cfa(g8.pio.syntax$Means, data = mss$Grade_8, group = "Year"))


## comparing configural, Threshold, and Loading models
g8.pio.comps <- bind_rows(compare_mods(g8.pio.mods$Configural, g8.pio.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g8.pio.mods$Threshold, g8.pio.mods$Loading) %>% mutate(Model = "Loading")) %>%
                          # compare_mods(g8.pio.mods$Threshold, g8.pio.mods$Partial) %>% mutate(Model = "Partial")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g8.pio.fits <- fits_wrapper(g8.pio.mods) %>%
  left_join(g8.pio.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g8.pio.loads <- loadings_wrapper(mods.list = g8.pio.mods, items = l23.scales$`Positive Identity and Outlook`) %>%
  select(item, Y2013 = "1", Y2016 = "3", Y2019 = "2", Y2022 = "4", Constrained)

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g8.pio.mean.var <- grp_mvs(g8.pio.mods$Means, "PIO") %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022"))) %>%
  arrange(Year)

## Parameters to investigate partial invariance
g8.pio.partial <- partial_params(g8.pio.mods$Loading)

## Compiling results into list object
g8.pio.results <- list(Base = g8.base$`Positive Identity and Outlook`,
                       Syntax = g8.pio.syntax,
                       Models = g8.pio.mods,
                       Fits = g8.pio.fits,
                       Loadings = g8.pio.loads,
                       Means_Vars = g8.pio.mean.var,
                       Partial = g8.pio.partial)

##################################################################

####  Social Competence ####

## check response values for miscodes or lack of variance
lapply(sc.items, function(x) class(mss$Grade_8[[x]]))
lapply(sc.items, function(x) table(mss$Grade_8$Year, mss$Grade_8[[x]], useNA = "always"))

## Generating model syntax
g8.sc.syntax <- generate_mi_syntax(g8.base$`Social Competence`, data = mss$Grade_8,
                                   group = "Year", mi.settings = mi.settings)

## Updating syntax for partial invariance
g8.sc.syntax$Partial <- g8.sc.syntax$Loading %>%
  sub("Y60q \\+ c\\(lambda.8_1, lambda.8_1, lambda.8_1, lambda.8_1",
      "Y60q + c(lambda.8_1.g1, lambda.8_1.g2, lambda.8_1.g3, lambda.8_1.g4",.) %>%
  sub("Y60m \\+ c\\(lambda.7_1, lambda.7_1, lambda.7_1, lambda.7_1",
      "Y60m + c(lambda.7_1.g1, lambda.7_1.g2, lambda.7_1.g3, lambda.7_1.g4",.)
g8.sc.syntax$Means <- g8.sc.syntax$Means %>%
  sub("Y60q \\+ c\\(lambda.8_1, lambda.8_1, lambda.8_1, lambda.8_1",
      "Y60q + c(lambda.8_1.g1, lambda.8_1.g2, lambda.8_1.g3, lambda.8_1.g4",.) %>%
  sub("Y60m \\+ c\\(lambda.7_1, lambda.7_1, lambda.7_1, lambda.7_1",
      "Y60m + c(lambda.7_1.g1, lambda.7_1.g2, lambda.7_1.g3, lambda.7_1.g4",.)

## Running models
g8.sc.mods <- list(`2013 Only` = cfa(g8.base$`Social Competence`, data = filter(mss$Grade_8, Year == 2013), ordered = TRUE),
                   `2016 Only` = cfa(g8.base$`Social Competence`, data = filter(mss$Grade_8, Year == 2016), ordered = TRUE),
                   `2019 Only` = cfa(g8.base$`Social Competence`, data = filter(mss$Grade_8, Year == 2019), ordered = TRUE),
                   `2022 Only` = cfa(g8.base$`Social Competence`, data = filter(mss$Grade_8, Year == 2022), ordered = TRUE),
                   Configural = cfa(g8.sc.syntax$Configural, data = mss$Grade_8, group = "Year"),
                   Threshold = cfa(g8.sc.syntax$Threshold, data = mss$Grade_8, group = "Year"),
                   Loading = cfa(g8.sc.syntax$Loading, data = mss$Grade_8, group = "Year"),
                   # Partial = cfa(g8.sc.syntax$Partial, data = mss$Grade_8, group = "Year"),
                   Means = cfa(g8.sc.syntax$Means, data = mss$Grade_8, group = "Year"))
# g8.sc.mods$Partial <- cfa(g8.sc.syntax$Partial, data = mss$Grade_8, group = "Year")
# g8.sc.mods$Means <- cfa(g8.sc.syntax$Means, data = mss$Grade_8, group = "Year")


## comparing configural, Threshold, and Loading models
g8.sc.comps <- bind_rows(compare_mods(g8.sc.mods$Configural, g8.sc.mods$Threshold) %>% mutate(Model = "Threshold"),
                         compare_mods(g8.sc.mods$Threshold, g8.sc.mods$Loading) %>% mutate(Model = "Loading")) %>%
                         # compare_mods(g8.sc.mods$Threshold, g8.sc.mods$Partial) %>% mutate(Model = "Partial")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g8.sc.fits <- fits_wrapper(g8.sc.mods) %>%
  left_join(g8.sc.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g8.sc.loads <- loadings_wrapper(mods.list = g8.sc.mods, items = l23.scales$`Social Competence`) %>%
  select(item, Y2013 = "1", Y2016 = "3", Y2019 = "2", Y2022 = "4", Constrained)

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g8.sc.mean.var <- grp_mvs(g8.sc.mods$Means, "SC") %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022"))) %>%
  arrange(Year)

## Parameters to investigate partial invariance
g8.sc.partial <- partial_params(g8.sc.mods$Loading)

## Compiling results into list object
g8.sc.results <- list(Base = g8.base$`Social Competence`,
                      Syntax = g8.sc.syntax,
                      Models = g8.sc.mods,
                      Fits = g8.sc.fits,
                      Loadings = g8.sc.loads,
                      Means_Vars = g8.sc.mean.var,
                      Partial = g8.sc.partial)

#######################################################################

####  Empowerment ####

## check response values for miscodes or lack of variance
lapply(emp.items, function(x) class(mss$Grade_8[[x]]))
lapply(emp.items, function(x) table(mss$Grade_8$Year, mss$Grade_8[[x]], useNA = "always"))

## Generating model syntax
g8.emp.syntax <- generate_mi_syntax(g8.base$`Empowerment`, data = mss$Grade_8,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g8.emp.mods <- list(`2013 Only` = cfa(g8.base$`Empowerment`, data = filter(mss$Grade_8, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g8.base$`Empowerment`, data = filter(mss$Grade_8, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g8.base$`Empowerment`, data = filter(mss$Grade_8, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g8.base$`Empowerment`, data = filter(mss$Grade_8, Year == 2022), ordered = TRUE),
                    Configural = cfa(g8.emp.syntax$Configural, data = mss$Grade_8, group = "Year"),
                    Threshold = cfa(g8.emp.syntax$Threshold, data = mss$Grade_8, group = "Year"),
                    Loading = cfa(g8.emp.syntax$Loading, data = mss$Grade_8, group = "Year"),
                    Means = cfa(g8.emp.syntax$Means, data = mss$Grade_8, group = "Year"))


## comparing configural, Threshold, and Loading models
g8.emp.comps <- bind_rows(compare_mods(g8.emp.mods$Configural, g8.emp.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g8.emp.mods$Threshold, g8.emp.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g8.emp.fits <- fits_wrapper(g8.emp.mods) %>%
  left_join(g8.emp.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g8.emp.loads <- loadings_wrapper(mods.list = g8.emp.mods, items = l23.scales$`Empowerment`) %>%
  select(item, Y2013 = "1", Y2016 = "3", Y2019 = "2", Y2022 = "4", Constrained)

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g8.emp.mean.var <- grp_mvs(g8.emp.mods$Means, "EMP") %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022"))) %>%
  arrange(Year)

## Parameters to investigate partial invariance
g8.emp.partial <- partial_params(g8.emp.mods$Loading)

## Compiling results into list object
g8.emp.results <- list(Base = g8.base$`Empowerment`,
                       Syntax = g8.emp.syntax,
                       Models = g8.emp.mods,
                       Fits = g8.emp.fits,
                       Loadings = g8.emp.loads,
                       Means_Vars = g8.emp.mean.var,
                       Partial = g8.emp.partial)

#######################################################################

####  Family/Community Support ####

## check response values for miscodes or lack of variance
lapply(fcs.items, function(x) class(mss$Grade_8[[x]]))
lapply(fcs.items, function(x) table(mss$Grade_8$Year, mss$Grade_8[[x]], useNA = "always"))

## Generating model syntax
g8.fcs.syntax <- generate_mi_syntax(g8.base$`Family/Community Support`, data = mss$Grade_8,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g8.fcs.mods <- list(`2013 Only` = cfa(g8.base$`Family/Community Support`, data = filter(mss$Grade_8, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g8.base$`Family/Community Support`, data = filter(mss$Grade_8, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g8.base$`Family/Community Support`, data = filter(mss$Grade_8, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g8.base$`Family/Community Support`, data = filter(mss$Grade_8, Year == 2022), ordered = TRUE),
                    Configural = cfa(g8.fcs.syntax$Configural, data = mss$Grade_8, group = "Year"),
                    Threshold = cfa(g8.fcs.syntax$Threshold, data = mss$Grade_8, group = "Year"),
                    Loading = cfa(g8.fcs.syntax$Loading, data = mss$Grade_8, group = "Year"),
                    Means = cfa(g8.fcs.syntax$Means, data = mss$Grade_8, group = "Year"))


## comparing configural, Threshold, and Loading models
g8.fcs.comps <- bind_rows(compare_mods(g8.fcs.mods$Configural, g8.fcs.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g8.fcs.mods$Threshold, g8.fcs.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g8.fcs.fits <- fits_wrapper(g8.fcs.mods) %>%
  left_join(g8.fcs.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g8.fcs.loads <- loadings_wrapper(mods.list = g8.fcs.mods, items = l23.scales$`Family/Community Support`) %>%
  select(item, Y2013 = "1", Y2016 = "3", Y2019 = "2", Y2022 = "4", Constrained)

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g8.fcs.mean.var <- grp_mvs(g8.fcs.mods$Means, "FCS") %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022"))) %>%
  arrange(Year)

## Parameters to investigate partial invariance
g8.fcs.partial <- partial_params(g8.fcs.mods$Loading)

## Compiling results into list object
g8.fcs.results <- list(Base = g8.base$`Family/Community Support`,
                       Syntax = g8.fcs.syntax,
                       Models = g8.fcs.mods,
                       Fits = g8.fcs.fits,
                       Loadings = g8.fcs.loads,
                       Means_Vars = g8.fcs.mean.var,
                       Partial = g8.fcs.partial)

#######################################################################

####  Teacher/School Support ####

## check response values for miscodes or lack of variance
lapply(tss.items, function(x) class(mss$Grade_8[[x]]))
lapply(tss.items, function(x) table(mss$Grade_8$Year, mss$Grade_8[[x]], useNA = "always"))

## Generating model syntax
g8.tss.syntax <- generate_mi_syntax(g8.base$`Teacher/School Support`, data = mss$Grade_8,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g8.tss.mods <- list(`2013 Only` = cfa(g8.base$`Teacher/School Support`, data = filter(mss$Grade_8, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g8.base$`Teacher/School Support`, data = filter(mss$Grade_8, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g8.base$`Teacher/School Support`, data = filter(mss$Grade_8, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g8.base$`Teacher/School Support`, data = filter(mss$Grade_8, Year == 2022), ordered = TRUE),
                    Configural = cfa(g8.tss.syntax$Configural, data = mss$Grade_8, group = "Year"),
                    Threshold = cfa(g8.tss.syntax$Threshold, data = mss$Grade_8, group = "Year"),
                    Loading = cfa(g8.tss.syntax$Loading, data = mss$Grade_8, group = "Year"),
                    Means = cfa(g8.tss.syntax$Means, data = mss$Grade_8, group = "Year"))


## comparing configural, Threshold, and Loading models
g8.tss.comps <- bind_rows(compare_mods(g8.tss.mods$Configural, g8.tss.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g8.tss.mods$Threshold, g8.tss.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g8.tss.fits <- fits_wrapper(g8.tss.mods) %>%
  left_join(g8.tss.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g8.tss.loads <- loadings_wrapper(mods.list = g8.tss.mods, items = l23.scales$`Teacher/School Support`) %>%
  select(item, Y2013 = "1", Y2016 = "3", Y2019 = "2", Y2022 = "4", Constrained)

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g8.tss.mean.var <- grp_mvs(g8.tss.mods$Means, "TSS") %>%
  mutate(Year = factor(Year, levels = c("2013", "2016", "2019", "2022"))) %>%
  arrange(Year)

## Parameters to investigate partial invariance
g8.tss.partial <- partial_params(g8.tss.mods$Loading)

## Compiling results into list object
g8.tss.results <- list(Base = g8.base$`Teacher/School Support`,
                       Syntax = g8.tss.syntax,
                       Models = g8.tss.mods,
                       Fits = g8.tss.fits,
                       Loadings = g8.tss.loads,
                       Means_Vars = g8.tss.mean.var,
                       Partial = g8.tss.partial)

#######################################################################



# -----  Grade 9  ------

## Defining base structure
g9.base <- map2(.x = abb.scales, .y = l23.scales,
                ~paste(.x, "=~", paste(.y, collapse = " + ")))

####  Commitment to Learning ####

## check response values for miscodes or lack of variance
lapply(ctl.items, function(x) class(mss$Grade_9[[x]]))
lapply(ctl.items, function(x) table(mss$Grade_9[[x]], useNA = "always"))

## Generating model syntax
g9.ctl.syntax <- generate_mi_syntax(g9.base$`Commitment to Learning`, data = mss$Grade_9,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g9.ctl.mods <- list(`2013 Only` = cfa(g9.base$`Commitment to Learning`, data = filter(mss$Grade_9, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g9.base$`Commitment to Learning`, data = filter(mss$Grade_9, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g9.base$`Commitment to Learning`, data = filter(mss$Grade_9, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g9.base$`Commitment to Learning`, data = filter(mss$Grade_9, Year == 2022), ordered = TRUE),
                    Configural = cfa(g9.ctl.syntax$Configural, data = mss$Grade_9, group = "Year"),
                    Threshold = cfa(g9.ctl.syntax$Threshold, data = mss$Grade_9, group = "Year"),
                    Loading = cfa(g9.ctl.syntax$Loading, data = mss$Grade_9, group = "Year"),
                    Means = cfa(g9.ctl.syntax$Means, data = mss$Grade_9, group = "Year"))


## comparing configural, Threshold, and Loading models
g9.ctl.comps <- bind_rows(compare_mods(g9.ctl.mods$Configural, g9.ctl.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g9.ctl.mods$Threshold, g9.ctl.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g9.ctl.fits <- fits_wrapper(g9.ctl.mods) %>%
  left_join(g9.ctl.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g9.ctl.loads <- loadings_wrapper(mods.list = g9.ctl.mods, items = l23.scales$`Commitment to Learning`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g9.ctl.mean.var <- grp_mvs(g9.ctl.mods$Means, "CTL")

## Parameters to investigate partial invariance
g9.ctl.partial <- partial_params(g9.ctl.mods$Loading)

## Compiling results into list object
g9.ctl.results <- list(Base = g9.base$`Commitment to Learning`,
                       Syntax = g9.ctl.syntax,
                       Models = g9.ctl.mods,
                       Fits = g9.ctl.fits,
                       Loadings = g9.ctl.loads,
                       Means_Vars = g9.ctl.mean.var,
                       Partial = g9.ctl.partial)

#######################################################################

####  Positive Identity and Outlook ####

## check response values for miscodes or lack of variance
lapply(pio.l23.items, function(x) class(mss$Grade_9[[x]]))
lapply(pio.l23.items, function(x) table(mss$Grade_9$Year, mss$Grade_9[[x]], useNA = "always"))

## Generating model syntax
g9.pio.syntax <- generate_mi_syntax(g9.base$`Positive Identity and Outlook`, data = mss$Grade_9,
                                    group = "Year", mi.settings = mi.settings)

# ## Updating syntax for partial invariance
# g9.pio.syntax$Partial <- g9.pio.syntax$Loading %>%
#   sub("Y60n \\+ c\\(lambda.6_1, lambda.6_1, lambda.6_1, lambda.6_1",
#       "Y60n + c(lambda.6_1.g1, lambda.6_1.g2, lambda.6_1.g3, lambda.6_1.g4",.)
# g9.pio.syntax$Means <- g9.pio.syntax$Means %>%
#   sub("Y60n \\+ c\\(lambda.6_1, lambda.6_1, lambda.6_1, lambda.6_1",
#       "Y60n + c(lambda.6_1.g1, lambda.6_1.g2, lambda.6_1.g3, lambda.6_1.g4",.)

## Running models
g9.pio.mods <- list(`2013 Only` = cfa(g9.base$`Positive Identity and Outlook`, data = filter(mss$Grade_9, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g9.base$`Positive Identity and Outlook`, data = filter(mss$Grade_9, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g9.base$`Positive Identity and Outlook`, data = filter(mss$Grade_9, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g9.base$`Positive Identity and Outlook`, data = filter(mss$Grade_9, Year == 2022), ordered = TRUE),
                    Configural = cfa(g9.pio.syntax$Configural, data = mss$Grade_9, group = "Year"),
                    Threshold = cfa(g9.pio.syntax$Threshold, data = mss$Grade_9, group = "Year"),
                    Loading = cfa(g9.pio.syntax$Loading, data = mss$Grade_9, group = "Year"),
                    # Partial = cfa(g9.pio.syntax$Partial, data = mss$Grade_9, group = "Year"),
                    Means = cfa(g9.pio.syntax$Means, data = mss$Grade_9, group = "Year"))


## comparing configural, Threshold, and Loading models
g9.pio.comps <- bind_rows(compare_mods(g9.pio.mods$Configural, g9.pio.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g9.pio.mods$Threshold, g9.pio.mods$Loading) %>% mutate(Model = "Loading")) %>%
                          # compare_mods(g9.pio.mods$Threshold, g9.pio.mods$Partial) %>% mutate(Model = "Partial")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g9.pio.fits <- fits_wrapper(g9.pio.mods) %>%
  left_join(g9.pio.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g9.pio.loads <- loadings_wrapper(mods.list = g9.pio.mods, items = l23.scales$`Positive Identity and Outlook`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g9.pio.mean.var <- grp_mvs(g9.pio.mods$Means, "PIO")

## Parameters to investigate partial invariance
g9.pio.partial <- partial_params(g9.pio.mods$Loading)

## Compiling results into list object
g9.pio.results <- list(Base = g9.base$`Positive Identity and Outlook`,
                       Syntax = g9.pio.syntax,
                       Models = g9.pio.mods,
                       Fits = g9.pio.fits,
                       Loadings = g9.pio.loads,
                       Means_Vars = g9.pio.mean.var,
                       Partial = g9.pio.partial)

##################################################################

####  Social Competence ####

## check response values for miscodes or lack of variance
lapply(sc.items, function(x) class(mss$Grade_9[[x]]))
lapply(sc.items, function(x) table(mss$Grade_9$Year, mss$Grade_9[[x]], useNA = "always"))

## Generating model syntax
g9.sc.syntax <- generate_mi_syntax(g9.base$`Social Competence`, data = mss$Grade_9,
                                   group = "Year", mi.settings = mi.settings)

# ## Updating syntax for partial invariance
# g9.sc.syntax$Partial <- g9.sc.syntax$Loading %>%
#   sub("Y60m \\+ c\\(lambda.7_1, lambda.7_1, lambda.7_1, lambda.7_1",
#       "Y60m + c(lambda.7_1.g1, lambda.7_1.g2, lambda.7_1.g3, lambda.7_1.g4",.) %>%
#   sub("Y60d \\+ c\\(lambda.2_1, lambda.2_1, lambda.2_1, lambda.2_1",
#       "Y60d + c(lambda.2_1.g1, lambda.2_1.g2, lambda.2_1.g3, lambda.2_1.g4",.)
# g9.sc.syntax$Means <- g9.sc.syntax$Means %>%
#   sub("Y60m \\+ c\\(lambda.7_1, lambda.7_1, lambda.7_1, lambda.7_1",
#       "Y60m + c(lambda.7_1.g1, lambda.7_1.g2, lambda.7_1.g3, lambda.7_1.g4",.) %>%
#   sub("Y60d \\+ c\\(lambda.2_1, lambda.2_1, lambda.2_1, lambda.2_1",
#       "Y60d + c(lambda.2_1.g1, lambda.2_1.g2, lambda.2_1.g3, lambda.2_1.g4",.)

## Running models
g9.sc.mods <- list(`2013 Only` = cfa(g9.base$`Social Competence`, data = filter(mss$Grade_9, Year == 2013), ordered = TRUE),
                   `2016 Only` = cfa(g9.base$`Social Competence`, data = filter(mss$Grade_9, Year == 2016), ordered = TRUE),
                   `2019 Only` = cfa(g9.base$`Social Competence`, data = filter(mss$Grade_9, Year == 2019), ordered = TRUE),
                   `2022 Only` = cfa(g9.base$`Social Competence`, data = filter(mss$Grade_9, Year == 2022), ordered = TRUE),
                   Configural = cfa(g9.sc.syntax$Configural, data = mss$Grade_9, group = "Year"),
                   Threshold = cfa(g9.sc.syntax$Threshold, data = mss$Grade_9, group = "Year"),
                   Loading = cfa(g9.sc.syntax$Loading, data = mss$Grade_9, group = "Year"),
                   # Partial = cfa(g9.sc.syntax$Partial, data = mss$Grade_9, group = "Year"),
                   Means = cfa(g9.sc.syntax$Means, data = mss$Grade_9, group = "Year"))
# g9.sc.mods$Partial <- cfa(g9.sc.syntax$Partial, data = mss$Grade_9, group = "Year")
# g9.sc.mods$Means <- cfa(g9.sc.syntax$Means, data = mss$Grade_9, group = "Year")


## comparing configural, Threshold, and Loading models
g9.sc.comps <- bind_rows(compare_mods(g9.sc.mods$Configural, g9.sc.mods$Threshold) %>% mutate(Model = "Threshold"),
                         compare_mods(g9.sc.mods$Threshold, g9.sc.mods$Loading) %>% mutate(Model = "Loading")) %>%
                         # compare_mods(g9.sc.mods$Threshold, g9.sc.mods$Partial) %>% mutate(Model = "Partial")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g9.sc.fits <- fits_wrapper(g9.sc.mods) %>%
  left_join(g9.sc.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g9.sc.loads <- loadings_wrapper(mods.list = g9.sc.mods, items = l23.scales$`Social Competence`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g9.sc.mean.var <- grp_mvs(g9.sc.mods$Means, "SC")

## Parameters to investigate partial invariance
g9.sc.partial <- partial_params(g9.sc.mods$Loading)

## Compiling results into list object
g9.sc.results <- list(Base = g9.base$`Social Competence`,
                      Syntax = g9.sc.syntax,
                      Models = g9.sc.mods,
                      Fits = g9.sc.fits,
                      Loadings = g9.sc.loads,
                      Means_Vars = g9.sc.mean.var,
                      Partial = g9.sc.partial)

#######################################################################

####  Empowerment ####

## check response values for miscodes or lack of variance
lapply(emp.items, function(x) class(mss$Grade_9[[x]]))
lapply(emp.items, function(x) table(mss$Grade_9$Year, mss$Grade_9[[x]], useNA = "always"))

## Generating model syntax
g9.emp.syntax <- generate_mi_syntax(g9.base$`Empowerment`, data = mss$Grade_9,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g9.emp.mods <- list(`2013 Only` = cfa(g9.base$`Empowerment`, data = filter(mss$Grade_9, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g9.base$`Empowerment`, data = filter(mss$Grade_9, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g9.base$`Empowerment`, data = filter(mss$Grade_9, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g9.base$`Empowerment`, data = filter(mss$Grade_9, Year == 2022), ordered = TRUE),
                    Configural = cfa(g9.emp.syntax$Configural, data = mss$Grade_9, group = "Year"),
                    Threshold = cfa(g9.emp.syntax$Threshold, data = mss$Grade_9, group = "Year"),
                    Loading = cfa(g9.emp.syntax$Loading, data = mss$Grade_9, group = "Year"),
                    Means = cfa(g9.emp.syntax$Means, data = mss$Grade_9, group = "Year"))


## comparing configural, Threshold, and Loading models
g9.emp.comps <- bind_rows(compare_mods(g9.emp.mods$Configural, g9.emp.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g9.emp.mods$Threshold, g9.emp.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g9.emp.fits <- fits_wrapper(g9.emp.mods) %>%
  left_join(g9.emp.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g9.emp.loads <- loadings_wrapper(mods.list = g9.emp.mods, items = l23.scales$`Empowerment`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g9.emp.mean.var <- grp_mvs(g9.emp.mods$Means, "EMP")

## Parameters to investigate partial invariance
g9.emp.partial <- partial_params(g9.emp.mods$Loading)

## Compiling results into list object
g9.emp.results <- list(Base = g9.base$`Empowerment`,
                       Syntax = g9.emp.syntax,
                       Models = g9.emp.mods,
                       Fits = g9.emp.fits,
                       Loadings = g9.emp.loads,
                       Means_Vars = g9.emp.mean.var,
                       Partial = g9.emp.partial)

#######################################################################

####  Family/Community Support ####

## check response values for miscodes or lack of variance
lapply(fcs.items, function(x) class(mss$Grade_9[[x]]))
lapply(fcs.items, function(x) table(mss$Grade_9$Year, mss$Grade_9[[x]], useNA = "always"))

## Generating model syntax
g9.fcs.syntax <- generate_mi_syntax(g9.base$`Family/Community Support`, data = mss$Grade_9,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g9.fcs.mods <- list(`2013 Only` = cfa(g9.base$`Family/Community Support`, data = filter(mss$Grade_9, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g9.base$`Family/Community Support`, data = filter(mss$Grade_9, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g9.base$`Family/Community Support`, data = filter(mss$Grade_9, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g9.base$`Family/Community Support`, data = filter(mss$Grade_9, Year == 2022), ordered = TRUE),
                    Configural = cfa(g9.fcs.syntax$Configural, data = mss$Grade_9, group = "Year"),
                    Threshold = cfa(g9.fcs.syntax$Threshold, data = mss$Grade_9, group = "Year"),
                    Loading = cfa(g9.fcs.syntax$Loading, data = mss$Grade_9, group = "Year"),
                    Means = cfa(g9.fcs.syntax$Means, data = mss$Grade_9, group = "Year"))


## comparing configural, Threshold, and Loading models
g9.fcs.comps <- bind_rows(compare_mods(g9.fcs.mods$Configural, g9.fcs.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g9.fcs.mods$Threshold, g9.fcs.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g9.fcs.fits <- fits_wrapper(g9.fcs.mods) %>%
  left_join(g9.fcs.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g9.fcs.loads <- loadings_wrapper(mods.list = g9.fcs.mods, items = l23.scales$`Family/Community Support`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g9.fcs.mean.var <- grp_mvs(g9.fcs.mods$Means, "FCS")

## Parameters to investigate partial invariance
g9.fcs.partial <- partial_params(g9.fcs.mods$Loading)

## Compiling results into list object
g9.fcs.results <- list(Base = g9.base$`Family/Community Support`,
                       Syntax = g9.fcs.syntax,
                       Models = g9.fcs.mods,
                       Fits = g9.fcs.fits,
                       Loadings = g9.fcs.loads,
                       Means_Vars = g9.fcs.mean.var,
                       Partial = g9.fcs.partial)

#######################################################################

####  Teacher/School Support ####

## check response values for miscodes or lack of variance
lapply(tss.items, function(x) class(mss$Grade_9[[x]]))
lapply(tss.items, function(x) table(mss$Grade_9$Year, mss$Grade_9[[x]], useNA = "always"))

## Generating model syntax
g9.tss.syntax <- generate_mi_syntax(g9.base$`Teacher/School Support`, data = mss$Grade_9,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g9.tss.mods <- list(`2013 Only` = cfa(g9.base$`Teacher/School Support`, data = filter(mss$Grade_9, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g9.base$`Teacher/School Support`, data = filter(mss$Grade_9, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g9.base$`Teacher/School Support`, data = filter(mss$Grade_9, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g9.base$`Teacher/School Support`, data = filter(mss$Grade_9, Year == 2022), ordered = TRUE),
                    Configural = cfa(g9.tss.syntax$Configural, data = mss$Grade_9, group = "Year"),
                    Threshold = cfa(g9.tss.syntax$Threshold, data = mss$Grade_9, group = "Year"),
                    Loading = cfa(g9.tss.syntax$Loading, data = mss$Grade_9, group = "Year"),
                    Means = cfa(g9.tss.syntax$Means, data = mss$Grade_9, group = "Year"))


## comparing configural, Threshold, and Loading models
g9.tss.comps <- bind_rows(compare_mods(g9.tss.mods$Configural, g9.tss.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g9.tss.mods$Threshold, g9.tss.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g9.tss.fits <- fits_wrapper(g9.tss.mods) %>%
  left_join(g9.tss.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g9.tss.loads <- loadings_wrapper(mods.list = g9.tss.mods, items = l23.scales$`Teacher/School Support`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g9.tss.mean.var <- grp_mvs(g9.tss.mods$Means, "TSS")

## Parameters to investigate partial invariance
g9.tss.partial <- partial_params(g9.tss.mods$Loading)

## Compiling results into list object
g9.tss.results <- list(Base = g9.base$`Teacher/School Support`,
                       Syntax = g9.tss.syntax,
                       Models = g9.tss.mods,
                       Fits = g9.tss.fits,
                       Loadings = g9.tss.loads,
                       Means_Vars = g9.tss.mean.var,
                       Partial = g9.tss.partial)

#######################################################################

# -----  Grade 11  ------

## Defining base structure
g11.base <- map2(.x = abb.scales, .y = l23.scales,
                ~paste(.x, "=~", paste(.y, collapse = " + ")))

####  Commitment to Learning ####

## check response values for miscodes or lack of variance
lapply(ctl.items, function(x) class(mss$Grade_11[[x]]))
lapply(ctl.items, function(x) table(mss$Grade_11[[x]], useNA = "always"))

## Generating model syntax
g11.ctl.syntax <- generate_mi_syntax(g11.base$`Commitment to Learning`, data = mss$Grade_11,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g11.ctl.mods <- list(`2013 Only` = cfa(g11.base$`Commitment to Learning`, data = filter(mss$Grade_11, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g11.base$`Commitment to Learning`, data = filter(mss$Grade_11, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g11.base$`Commitment to Learning`, data = filter(mss$Grade_11, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g11.base$`Commitment to Learning`, data = filter(mss$Grade_11, Year == 2022), ordered = TRUE),
                    Configural = cfa(g11.ctl.syntax$Configural, data = mss$Grade_11, group = "Year"),
                    Threshold = cfa(g11.ctl.syntax$Threshold, data = mss$Grade_11, group = "Year"),
                    Loading = cfa(g11.ctl.syntax$Loading, data = mss$Grade_11, group = "Year"),
                    Means = cfa(g11.ctl.syntax$Means, data = mss$Grade_11, group = "Year"))



## comparing configural, Threshold, and Loading models
g11.ctl.comps <- bind_rows(compare_mods(g11.ctl.mods$Configural, g11.ctl.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g11.ctl.mods$Threshold, g11.ctl.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g11.ctl.fits <- fits_wrapper(g11.ctl.mods) %>%
  left_join(g11.ctl.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g11.ctl.loads <- loadings_wrapper(mods.list = g11.ctl.mods, items = l23.scales$`Commitment to Learning`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g11.ctl.mean.var <- grp_mvs(g11.ctl.mods$Means, "CTL")

## Parameters to investigate partial invariance
g11.ctl.partial <- partial_params(g11.ctl.mods$Loading)

## Compiling results into list object
g11.ctl.results <- list(Base = g11.base$`Commitment to Learning`,
                       Syntax = g11.ctl.syntax,
                       Models = g11.ctl.mods,
                       Fits = g11.ctl.fits,
                       Loadings = g11.ctl.loads,
                       Means_Vars = g11.ctl.mean.var,
                       Partial = g11.ctl.partial)

#######################################################################

####  Positive Identity and Outlook ####

## check response values for miscodes or lack of variance
lapply(pio.l23.items, function(x) class(mss$Grade_11[[x]]))
lapply(pio.l23.items, function(x) table(mss$Grade_11$Year, mss$Grade_11[[x]], useNA = "always"))

## Generating model syntax
g11.pio.syntax <- generate_mi_syntax(g11.base$`Positive Identity and Outlook`, data = mss$Grade_11,
                                    group = "Year", mi.settings = mi.settings)

# ## Updating syntax for partial invariance
# g11.pio.syntax$Partial <- g11.pio.syntax$Loading %>%
#   sub("Y60n \\+ c\\(lambda.6_1, lambda.6_1, lambda.6_1, lambda.6_1",
#       "Y60n + c(lambda.6_1.g1, lambda.6_1.g2, lambda.6_1.g3, lambda.6_1.g4",.)
# g11.pio.syntax$Means <- g11.pio.syntax$Means %>%
#   sub("Y60n \\+ c\\(lambda.6_1, lambda.6_1, lambda.6_1, lambda.6_1",
#       "Y60n + c(lambda.6_1.g1, lambda.6_1.g2, lambda.6_1.g3, lambda.6_1.g4",.)

## Running models
g11.pio.mods <- list(`2013 Only` = cfa(g11.base$`Positive Identity and Outlook`, data = filter(mss$Grade_11, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g11.base$`Positive Identity and Outlook`, data = filter(mss$Grade_11, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g11.base$`Positive Identity and Outlook`, data = filter(mss$Grade_11, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g11.base$`Positive Identity and Outlook`, data = filter(mss$Grade_11, Year == 2022), ordered = TRUE),
                    Configural = cfa(g11.pio.syntax$Configural, data = mss$Grade_11, group = "Year"),
                    Threshold = cfa(g11.pio.syntax$Threshold, data = mss$Grade_11, group = "Year"),
                    Loading = cfa(g11.pio.syntax$Loading, data = mss$Grade_11, group = "Year"),
                    # Partial = cfa(g11.pio.syntax$Partial, data = mss$Grade_11, group = "Year"),
                    Means = cfa(g11.pio.syntax$Means, data = mss$Grade_11, group = "Year"))


## comparing configural, Threshold, and Loading models
g11.pio.comps <- bind_rows(compare_mods(g11.pio.mods$Configural, g11.pio.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g11.pio.mods$Threshold, g11.pio.mods$Loading) %>% mutate(Model = "Loading")) %>%
                          # compare_mods(g11.pio.mods$Threshold, g11.pio.mods$Partial) %>% mutate(Model = "Partial")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g11.pio.fits <- fits_wrapper(g11.pio.mods) %>%
  left_join(g11.pio.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g11.pio.loads <- loadings_wrapper(mods.list = g11.pio.mods, items = l23.scales$`Positive Identity and Outlook`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g11.pio.mean.var <- grp_mvs(g11.pio.mods$Means, "PIO")

## Parameters to investigate partial invariance
g11.pio.partial <- partial_params(g11.pio.mods$Loading)

## Compiling results into list object
g11.pio.results <- list(Base = g11.base$`Positive Identity and Outlook`,
                       Syntax = g11.pio.syntax,
                       Models = g11.pio.mods,
                       Fits = g11.pio.fits,
                       Loadings = g11.pio.loads,
                       Means_Vars = g11.pio.mean.var,
                       Partial = g11.pio.partial)

##################################################################

####  Social Competence #### - Could not establish loading invariance

## check response values for miscodes or lack of variance
lapply(sc.items, function(x) class(mss$Grade_11[[x]]))
lapply(sc.items, function(x) table(mss$Grade_11$Year, mss$Grade_11[[x]], useNA = "always"))

## Generating model syntax
g11.sc.syntax <- generate_mi_syntax(g11.base$`Social Competence`, data = mss$Grade_11,
                                   group = "Year", mi.settings = mi.settings)

# ## Updating syntax for partial invariance
# g11.sc.syntax$Partial <- g11.sc.syntax$Loading %>%
#   sub("Y60d \\+ c\\(lambda.2_1, lambda.2_1, lambda.2_1, lambda.2_1",
#       "Y60d + c(lambda.2_1.g1, lambda.2_1.g2, lambda.2_1.g3, lambda.2_1.g4",.) %>%
#   sub("Y60j \\+ c\\(lambda.5_1, lambda.5_1, lambda.5_1, lambda.5_1",
#       "Y60j + c(lambda.5_1.g1, lambda.5_1.g2, lambda.5_1.g3, lambda.5_1.g4",.)
# g11.sc.syntax$Means <- g11.sc.syntax$Means %>%
#   sub("Y60d \\+ c\\(lambda.2_1, lambda.2_1, lambda.2_1, lambda.2_1",
#       "Y60d + c(lambda.2_1.g1, lambda.2_1.g2, lambda.2_1.g3, lambda.2_1.g4",.) %>%
#   sub("Y60j \\+ c\\(lambda.5_1, lambda.5_1, lambda.5_1, lambda.5_1",
#       "Y60j + c(lambda.5_1.g1, lambda.5_1.g2, lambda.5_1.g3, lambda.5_1.g4",.)

## Running models
g11.sc.mods <- list(`2013 Only` = cfa(g11.base$`Social Competence`, data = filter(mss$Grade_11, Year == 2013), ordered = TRUE),
                   `2016 Only` = cfa(g11.base$`Social Competence`, data = filter(mss$Grade_11, Year == 2016), ordered = TRUE),
                   `2019 Only` = cfa(g11.base$`Social Competence`, data = filter(mss$Grade_11, Year == 2019), ordered = TRUE),
                   `2022 Only` = cfa(g11.base$`Social Competence`, data = filter(mss$Grade_11, Year == 2022), ordered = TRUE),
                   Configural = cfa(g11.sc.syntax$Configural, data = mss$Grade_11, group = "Year"),
                   Threshold = cfa(g11.sc.syntax$Threshold, data = mss$Grade_11, group = "Year"),
                   Loading = cfa(g11.sc.syntax$Loading, data = mss$Grade_11, group = "Year"),
                   # Partial = cfa(g11.sc.syntax$Partial, data = mss$Grade_11, group = "Year"),
                   Means = cfa(g11.sc.syntax$Means, data = mss$Grade_11, group = "Year"))
# g11.sc.mods$Partial <- cfa(g11.sc.syntax$Partial, data = mss$Grade_11, group = "Year")
# g11.sc.mods$Means <- cfa(g11.sc.syntax$Means, data = mss$Grade_11, group = "Year")

## comparing configural, Threshold, and Loading models
g11.sc.comps <- bind_rows(compare_mods(g11.sc.mods$Configural, g11.sc.mods$Threshold) %>% mutate(Model = "Threshold"),
                         compare_mods(g11.sc.mods$Threshold, g11.sc.mods$Loading) %>% mutate(Model = "Loading")) %>%
                         # compare_mods(g11.sc.mods$Threshold, g11.sc.mods$Partial) %>% mutate(Model = "Partial")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g11.sc.fits <- fits_wrapper(g11.sc.mods) %>%
  left_join(g11.sc.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g11.sc.loads <- loadings_wrapper(mods.list = g11.sc.mods, items = l23.scales$`Social Competence`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g11.sc.mean.var <- grp_mvs(g11.sc.mods$Means, "SC")

## Parameters to investigate partial invariance
g11.sc.partial <- partial_params(g11.sc.mods$Loading)

## Compiling results into list object
g11.sc.results <- list(Base = g11.base$`Social Competence`,
                      Syntax = g11.sc.syntax,
                      Models = g11.sc.mods,
                      Fits = g11.sc.fits,
                      Loadings = g11.sc.loads,
                      Means_Vars = g11.sc.mean.var,
                      Partial = g11.sc.partial)

#######################################################################

####  Empowerment ####

## check response values for miscodes or lack of variance
lapply(emp.items, function(x) class(mss$Grade_11[[x]]))
lapply(emp.items, function(x) table(mss$Grade_11$Year, mss$Grade_11[[x]], useNA = "always"))

## Generating model syntax
g11.emp.syntax <- generate_mi_syntax(g11.base$`Empowerment`, data = mss$Grade_11,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g11.emp.mods <- list(`2013 Only` = cfa(g11.base$`Empowerment`, data = filter(mss$Grade_11, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g11.base$`Empowerment`, data = filter(mss$Grade_11, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g11.base$`Empowerment`, data = filter(mss$Grade_11, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g11.base$`Empowerment`, data = filter(mss$Grade_11, Year == 2022), ordered = TRUE),
                    Configural = cfa(g11.emp.syntax$Configural, data = mss$Grade_11, group = "Year"),
                    Threshold = cfa(g11.emp.syntax$Threshold, data = mss$Grade_11, group = "Year"),
                    Loading = cfa(g11.emp.syntax$Loading, data = mss$Grade_11, group = "Year"),
                    Means = cfa(g11.emp.syntax$Means, data = mss$Grade_11, group = "Year"))


## comparing configural, Threshold, and Loading models
g11.emp.comps <- bind_rows(compare_mods(g11.emp.mods$Configural, g11.emp.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g11.emp.mods$Threshold, g11.emp.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g11.emp.fits <- fits_wrapper(g11.emp.mods) %>%
  left_join(g11.emp.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g11.emp.loads <- loadings_wrapper(mods.list = g11.emp.mods, items = l23.scales$`Empowerment`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g11.emp.mean.var <- grp_mvs(g11.emp.mods$Means, "EMP")

## Parameters to investigate partial invariance
g11.emp.partial <- partial_params(g11.emp.mods$Loading)

## Compiling results into list object
g11.emp.results <- list(Base = g11.base$`Empowerment`,
                       Syntax = g11.emp.syntax,
                       Models = g11.emp.mods,
                       Fits = g11.emp.fits,
                       Loadings = g11.emp.loads,
                       Means_Vars = g11.emp.mean.var,
                       Partial = g11.emp.partial)

#######################################################################

####  Family/Community Support ####

## check response values for miscodes or lack of variance
lapply(fcs.items, function(x) class(mss$Grade_11[[x]]))
lapply(fcs.items, function(x) table(mss$Grade_11$Year, mss$Grade_11[[x]], useNA = "always"))

## Generating model syntax
g11.fcs.syntax <- generate_mi_syntax(g11.base$`Family/Community Support`, data = mss$Grade_11,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g11.fcs.mods <- list(`2013 Only` = cfa(g11.base$`Family/Community Support`, data = filter(mss$Grade_11, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g11.base$`Family/Community Support`, data = filter(mss$Grade_11, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g11.base$`Family/Community Support`, data = filter(mss$Grade_11, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g11.base$`Family/Community Support`, data = filter(mss$Grade_11, Year == 2022), ordered = TRUE),
                    Configural = cfa(g11.fcs.syntax$Configural, data = mss$Grade_11, group = "Year"),
                    Threshold = cfa(g11.fcs.syntax$Threshold, data = mss$Grade_11, group = "Year"),
                    Loading = cfa(g11.fcs.syntax$Loading, data = mss$Grade_11, group = "Year"),
                    Means = cfa(g11.fcs.syntax$Means, data = mss$Grade_11, group = "Year"))


## comparing configural, Threshold, and Loading models
g11.fcs.comps <- bind_rows(compare_mods(g11.fcs.mods$Configural, g11.fcs.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g11.fcs.mods$Threshold, g11.fcs.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g11.fcs.fits <- fits_wrapper(g11.fcs.mods) %>%
  left_join(g11.fcs.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g11.fcs.loads <- loadings_wrapper(mods.list = g11.fcs.mods, items = l23.scales$`Family/Community Support`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g11.fcs.mean.var <- grp_mvs(g11.fcs.mods$Means, "FCS")

## Parameters to investigate partial invariance
g11.fcs.partial <- partial_params(g11.fcs.mods$Loading)

## Compiling results into list object
g11.fcs.results <- list(Base = g11.base$`Family/Community Support`,
                       Syntax = g11.fcs.syntax,
                       Models = g11.fcs.mods,
                       Fits = g11.fcs.fits,
                       Loadings = g11.fcs.loads,
                       Means_Vars = g11.fcs.mean.var,
                       Partial = g11.fcs.partial)

#######################################################################

####  Teacher/School Support ####

## check response values for miscodes or lack of variance
lapply(tss.items, function(x) class(mss$Grade_11[[x]]))
lapply(tss.items, function(x) table(mss$Grade_11$Year, mss$Grade_11[[x]], useNA = "always"))

## Generating model syntax
g11.tss.syntax <- generate_mi_syntax(g11.base$`Teacher/School Support`, data = mss$Grade_11,
                                    group = "Year", mi.settings = mi.settings)

## Running models
g11.tss.mods <- list(`2013 Only` = cfa(g11.base$`Teacher/School Support`, data = filter(mss$Grade_11, Year == 2013), ordered = TRUE),
                    `2016 Only` = cfa(g11.base$`Teacher/School Support`, data = filter(mss$Grade_11, Year == 2016), ordered = TRUE),
                    `2019 Only` = cfa(g11.base$`Teacher/School Support`, data = filter(mss$Grade_11, Year == 2019), ordered = TRUE),
                    `2022 Only` = cfa(g11.base$`Teacher/School Support`, data = filter(mss$Grade_11, Year == 2022), ordered = TRUE),
                    Configural = cfa(g11.tss.syntax$Configural, data = mss$Grade_11, group = "Year"),
                    Threshold = cfa(g11.tss.syntax$Threshold, data = mss$Grade_11, group = "Year"),
                    Loading = cfa(g11.tss.syntax$Loading, data = mss$Grade_11, group = "Year"),
                    Means = cfa(g11.tss.syntax$Means, data = mss$Grade_11, group = "Year"))


## comparing configural, Threshold, and Loading models
g11.tss.comps <- bind_rows(compare_mods(g11.tss.mods$Configural, g11.tss.mods$Threshold) %>% mutate(Model = "Threshold"),
                          compare_mods(g11.tss.mods$Threshold, g11.tss.mods$Loading) %>% mutate(Model = "Loading")) %>%
  mutate(RMSEA_D_90CI = paste0("[", Lower_CI, ", ", Upper_CI, "]")) %>%
  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr, RMSEA_D, RMSEA_D_90CI)

## gathering model fit for each model and joining model comparison statistics
g11.tss.fits <- fits_wrapper(g11.tss.mods) %>%
  left_join(g11.tss.comps, by = "Model")

## extracting unstandardized factor loadings for each group from threshold model
## and constrained loadings from loadings model
# Note. Constrained standardized factor loadings will not necessarily be equal across groups
g11.tss.loads <- loadings_wrapper(mods.list = g11.tss.mods, items = l23.scales$`Teacher/School Support`) %>%
  rename(Y2013 = "1", Y2016 = "2", Y2019 = "3", Y2022 = "4")

## extracting standardized mean difference and unstandardized variances
## from model constraining thresholds and loadings
g11.tss.mean.var <- grp_mvs(g11.tss.mods$Means, "TSS")

## Parameters to investigate partial invariance
g11.tss.partial <- partial_params(g11.tss.mods$Loading)

## Compiling results into list object
g11.tss.results <- list(Base = g11.base$`Teacher/School Support`,
                       Syntax = g11.tss.syntax,
                       Models = g11.tss.mods,
                       Fits = g11.tss.fits,
                       Loadings = g11.tss.loads,
                       Means_Vars = g11.tss.mean.var,
                       Partial = g11.tss.partial)

#######################################################################

# lapply(list(g5.ctl.results, g5.pio.results,
#             g5.sc.results, g5.emp.results,
#             g5.fcs.results, g5.tss.results),
#        function(x) lavInspect(x$Models$Configural, "group.label"))
# 
# ## group 8 still out of order
# lapply(list(g8.ctl.results, g8.pio.results,
#             g8.sc.results, g8.emp.results,
#             g8.fcs.results, g8.tss.results),
#        function(x) lavInspect(x$Models$Configural, "group.label"))
# 
# lapply(list(g9.ctl.results, g9.pio.results,
#             g9.sc.results, g9.emp.results,
#             g9.fcs.results, g9.tss.results),
#        function(x) lavInspect(x$Models$Configural, "group.label"))
# 
# lapply(list(g11.ctl.results, g11.pio.results,
#             g11.sc.results, g11.emp.results,
#             g11.fcs.results, g11.tss.results),
#        function(x) lavInspect(x$Models$Configural, "group.label"))

# -----  Saving Results to Output  -----
## All together - too big to run properly in Rmarkdown
save(g5.ctl.results, g5.pio.results,
     g5.sc.results, g5.emp.results,
     g5.fcs.results, g5.tss.results,
     g8.ctl.results, g8.pio.results,
     g8.sc.results, g8.emp.results,
     g8.fcs.results, g8.tss.results,
     g9.ctl.results, g9.pio.results,
     g9.sc.results, g9.emp.results,
     g9.fcs.results, g9.tss.results,
     g11.ctl.results, g11.pio.results,
     g11.sc.results, g11.emp.results,
     g11.fcs.results, g11.tss.results,
     file = "MI_Results.RData")

## seperately by Level 1 and 2 (grades 5, 8) and Level 3 (grades 9, 11)

save(g5.ctl.results, g5.pio.results,
     g5.sc.results, g5.emp.results,
     g5.fcs.results, g5.tss.results,
     g8.ctl.results, g8.pio.results,
     g8.sc.results, g8.emp.results,
     g8.fcs.results, g8.tss.results,
     file = "MI_Results_L12.RData")

save(g9.ctl.results, g9.pio.results,
     g9.sc.results, g9.emp.results,
     g9.fcs.results, g9.tss.results,
     g11.ctl.results, g11.pio.results,
     g11.sc.results, g11.emp.results,
     g11.fcs.results, g11.tss.results,
     file = "MI_Results_L3.RData")

#########################################################################

# -----   Output tables and figures  ---------

####  Model Fit Tables ####
g5.table <- map2_dfr(.x = list(g5.ctl.results,g5.pio.results,
                               g5.sc.results,g5.fcs.results, g5.tss.results),
                     .y = names(abb.scales)[-4],
                     ~bind_rows(data.frame(Model = .y, n = NA, ngroups = NA, x2 = NA, df = NA,
                                           p = NA, CFI = NA, RMSEA = NA, SRMR = NA,
                                           diff.x2 = NA, diff.df = NA, diff.p = NA, diff.CFI = NA,
                                           diff.RMSEA = NA, diff.SRMR = NA, RMSEA_D = NA, RMSEA_D_90CI = NA),
                                filter(.x$Fits, Model %in% c("Configural", "Threshold", "Loading")))) %>%
  select(Model:RMSEA, `90CI`,SRMR:RMSEA_D_90CI, -n, -ngroups)

g8.table <- map2_dfr(.x = list(g8.ctl.results,g8.pio.results,
                               g8.sc.results,g8.fcs.results, g8.tss.results),
                     .y = names(abb.scales)[-4],
                     ~bind_rows(data.frame(Model = .y, n = NA, ngroups = NA, x2 = NA, df = NA,
                                           p = NA, CFI = NA, RMSEA = NA, SRMR = NA,
                                           diff.x2 = NA, diff.df = NA, diff.p = NA, diff.CFI = NA,
                                           diff.RMSEA = NA, diff.SRMR = NA, RMSEA_D = NA, RMSEA_D_90CI = NA),
                                filter(.x$Fits, Model %in% c("Configural", "Threshold", "Loading", "Partial")))) %>%
  select(Model:RMSEA, `90CI`,SRMR:RMSEA_D_90CI, -n, -ngroups)

g9.table <- map2_dfr(.x = list(g9.ctl.results,g9.pio.results,
                               g9.sc.results,g9.fcs.results, g9.tss.results),
                     .y = names(abb.scales)[-4],
                     ~bind_rows(data.frame(Model = .y, n = NA, ngroups = NA, x2 = NA, df = NA,
                                           p = NA, CFI = NA, RMSEA = NA, SRMR = NA,
                                           diff.x2 = NA, diff.df = NA, diff.p = NA, diff.CFI = NA,
                                           diff.RMSEA = NA, diff.SRMR = NA, RMSEA_D = NA, RMSEA_D_90CI = NA),
                                filter(.x$Fits, Model %in% c("Configural", "Threshold", "Loading", "Partial")))) %>%
  select(Model:RMSEA, `90CI`,SRMR:RMSEA_D_90CI, -n, -ngroups)

g11.table <- map2_dfr(.x = list(g11.ctl.results,g11.pio.results,
                               g11.sc.results,g11.fcs.results, g11.tss.results),
                     .y = names(abb.scales)[-4],
                     ~bind_rows(data.frame(Model = .y, n = NA, ngroups = NA, x2 = NA, df = NA,
                                           p = NA, CFI = NA, RMSEA = NA, SRMR = NA,
                                           diff.x2 = NA, diff.df = NA, diff.p = NA, diff.CFI = NA,
                                           diff.RMSEA = NA, diff.SRMR = NA, RMSEA_D = NA, RMSEA_D_90CI = NA),
                                filter(.x$Fits, Model %in% c("Configural", "Threshold", "Loading", "Partial")))) %>%
  select(Model:RMSEA, `90CI`,SRMR:RMSEA_D_90CI, -n, -ngroups)

####   Standardized Mean Difference Plots ####
smd.data <- map2_dfr(.x = list(g5.ctl.results,g5.pio.results,
                               g5.sc.results, g5.fcs.results, g5.tss.results),
                     .y = names(abb.scales)[-4],
                     ~.x$Means_Vars %>%
                       mutate(Measure = .y)) %>%
  mutate(Grade = "5") %>%
  bind_rows(map2_dfr(.x = list(g8.ctl.results,g8.pio.results,
                               g8.sc.results, g8.fcs.results, g8.tss.results),
                     .y = names(abb.scales)[-4],
                     ~.x$Means_Vars %>%
                       mutate(Measure = .y)) %>%
              mutate(Grade = "8")) %>%
  bind_rows(map2_dfr(.x = list(g9.ctl.results,g9.pio.results,
                               g9.sc.results, g9.fcs.results, g9.tss.results),
                     .y = names(abb.scales)[-4],
                     ~.x$Means_Vars %>%
                       mutate(Measure = .y)) %>%
              mutate(Grade = "9")) %>%
  bind_rows(map2_dfr(.x = list(g11.ctl.results,g11.pio.results, g11.sc.results, # initially did not establish loading invariance for g11.sc
                                g11.fcs.results, g11.tss.results),
                     .y = names(abb.scales)[-4],
                     ~.x$Means_Vars %>%
                       mutate(Measure = .y)) %>%
              mutate(Grade = "11")) %>%
  mutate(Measure = factor(Measure, levels = names(abb.scales)[-4]),
         Grade = factor(Grade, levels = c("5", "8", "9", "11")))


smd.plot <- ggplot(data = smd.data, aes(x = Year, y = std_mean, group = Grade, color = Grade)) +
  geom_line(linewidth = 2) +
  scale_color_brewer(palette = "Set2") +
  scale_y_continuous(limits = c(-.70, .20), breaks = c(-.6,-.4, -.2, 0, .2)) +
  labs(y = "Standardized Mean Difference from Year 2013") +
  theme_bw(base_size = 18) +
  facet_wrap(~Measure) +
  theme(legend.justification = c(1, 0), legend.position = c(.9, .1),
        legend.key.width = unit(2,"cm"))

smd.line <- ggplot(data = smd.data, aes(x = Year, y = std_mean, group = Grade, linetype = Grade)) +
  geom_line(linewidth = 1) +
  scale_linetype_manual(values = c("solid", "dotted", "twodash", "longdash")) +
  scale_y_continuous(limits = c(-.70, .20), breaks = c(-.6,-.4, -.2, 0, .2)) +
  labs(y = "Standardized Mean Difference from Year 2013") +
  theme_bw(base_size = 18) +
  facet_wrap(~Measure) +
  theme(legend.justification = c(1, 0), legend.position = c(.9, .1),
        legend.key.width = unit(3,"cm"))

save(demos.year, demos.grade,
     g5.table, g8.table, g9.table, g11.table,
     smd.data, smd.plot, smd.line, score.plots,
     file = "MI_Output.RData")


#########################################################################


