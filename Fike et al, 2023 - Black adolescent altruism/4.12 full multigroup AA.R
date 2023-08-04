###########################################
#                                         #
#        Black Adolescent Altruism        #
#               Analysis                  #
#                                         #
###########################################

# --------   Initial Set-up   ------------------

## capturing package versions used for conducting analysis
# renv::init()
# renv::snapshot()
# renv::restore()

source("Alt_PnF.R")


################################################################

# --------   Data and transformations   ------------------

## Load data
data <- read_spss("../MOST RECENT DATASET AA.sav")

## item-construct definitions
hassles.items <- select(data, starts_with("CDF")) %>% names()
sdiscrim.items <- select(data, starts_with("CSBD")) %>% names()
empathy.items <- select(data, CSSQ02,CSSQ05,CSSQ08,CSSQ12,CSSQ21,CSSQ24,CSSQ25,CSSQ28,CSSQ29,CSSQ39) %>% names()
altruism.items <- select(data, starts_with("CALT")) %>% names()


data2 <- data %>%
  filter(CSCHOOL != 6) %>%  # Not sure what school this student is supposed to be in
  filter(!is.na(CSEX)) %>%
  mutate(CSCHOOL = as_factor(CSCHOOL),
         CSEX = as_factor(CSEX),
         Female = ifelse(CSEX == "Female", 1, 0),
         CDISTR = as_factor(CDISTR),
         HASSLES_c = HASSLES-mean(HASSLES, na.rm=TRUE),
         sdiscrim_c = sdiscrim-mean(sdiscrim, na.rm=TRUE),
         Empathy_c = Empathy-mean(Empathy, na.rm=TRUE),
         EmpHASS = HASSLES_c*Empathy_c, # creating the interaction
         Empsdiscrim = Empathy_c*sdiscrim_c) %>%
  fastDummies::dummy_cols(select_columns = "CSCHOOL", ignore_na = TRUE) %>%
  rename_with(.cols = starts_with("CSCHOOL_"),
              .fn = ~str_remove(., "CSCHOOL_") %>% str_replace_all(., " ", "_"))

# prop.table(table(data2$CSEX))
# prop.table(table(data2$CRACE))
# prop.table(table(data2$CRACA))


################################################################

# --------   Check for Unmotivated Responders   ------------------

## updated the sl.check part to mutate all scales of interest and check for straightlining
# Vriesema and Gehlbach (2021) define straightlining as the same response on 10 consecutive items
# We take a stricter definition and will only exclude students who gave uniform responses to all items within each scale for each of the 4 scales in the analysis
## dataset with ID and items of interest only
sl.check <- data2 %>%
  select(CPERID6, all_of(c(hassles.items, sdiscrim.items, empathy.items, altruism.items))) %>%
  mutate(across(-CPERID6, as.numeric))

# creates list for each scale identifying which IDs straightlined (TRUE) or not (FALSE)
sls <- map(.x = list(hassles.items, sdiscrim.items, empathy.items, altruism.items),
           ~apply(sl.check[names(sl.check) %in% .x], 1, function(x) length(unique(x[!is.na(x)])) == 1)) %>%
  set_names("Hassles", "Discrimination", "Empathy", "Altruism")

# gathers the IDs of straightliners
sl.ids <- map(.x = sls, ~sl.check$CPERID6[.x])
# View(sl.check[sl.check$CPERID6 %in% sl.ids$Discrimination, sdiscrim.items])

# How many IDs straightlined for all 4 scales?
sl4 <- table(flatten_chr(sl.ids))
sl4.ids <- sl4[sl4 == 4]
# View(sl.check[sl.check$CPERID6 %in% names(sl4.ids),]) # total of 20 students


################################################################

# --------   Psychometric Checks   ------------------

#### Dimensionality ####
## Prepare items for kfa
item.subsets <- map(.x = list(hassles.items, sdiscrim.items, empathy.items, altruism.items),
                    ~data2 %>%
                      # filter(!CPERID6 %in% sl4.ids) %>%
                      select(all_of(.x)) %>%
                      mutate(across(everything(), as.numeric))) %>%
  set_names("Hassles", "Discrimination", "Empathy", "Altruism")

## power check
map2(item.subsets, c(3, 2, 2, 2), ~find_k(variables = .x, m = .y))

## run kfa
kfas <- map(.x = item.subsets,
            ~kfa(data = .x,
                 k = 3, # number of folds
                 m = 3, # maximum number of factors to test
                 simple = TRUE,  # do not allow items to cross-load
                 ordered = TRUE)) # treat variables as ordered

## generate report
map(.x = names(kfas),
    ~kfa_report(kfas[[.x]],
                index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                file.name = paste(.x, "Psychometric Report"),
                report.title = paste(.x, "Psychometric Report"),
                report.format = "word_document"))

#### Measurement Invariance by Gender ####
basemods <- map(.x = list(hassles.items, sdiscrim.items, empathy.items, altruism.items),
                ~cfa_mod(.x, "f1")) %>%
  set_names("Racial_Hassles", "School_Based_Discrimination", "Empathy", "Altruism")

## reliability on whole sample
relis <- map(.x = basemods, ~cfa(model = .x, data = data2, ordered = TRUE) %>%
               semTools::reliability())

# Note. empathy only has 3 response options so thresholds are already constrained for model identification
mi.mods <- map(.x = basemods,
               ~list(Configural = measEq.syntax(configural.model = .x, ordered = TRUE, data = data2,
                                          parameterization = "delta", ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                                          group = "CSEX", group.equal = "configural", return.fit = TRUE),
                     Metric = measEq.syntax(configural.model = .x, ordered = TRUE, data = data2,
                                               parameterization = "delta", ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                                               group = "CSEX", group.equal = c("thresholds"), return.fit = TRUE),
                     Scalar = measEq.syntax(configural.model = .x, ordered = TRUE, data = data2,
                                              parameterization = "delta", ID.fac = "std.lv", ID.cat = "Wu.Estabrook.2016",
                                              group = "CSEX",  group.equal = c("thresholds", "loadings"), return.fit = TRUE)))

mi.comps <- map(.x = mi.mods,
                ~bind_rows(compare_mods(.x$Configural, .x$Metric) %>% mutate(Model = "Metric"),
                           compare_mods(.x$Metric, .x$Scalar) %>% mutate(Model = "Scalar")) %>%
                  select(Model, diff.x2 = `Chisq diff`, diff.df = `Df diff`, diff.p = `Pr(>Chisq)`,
                         diff.CFI = cfi, diff.RMSEA = rmsea, diff.SRMR = srmr))

mi.fits <- map(.x = mi.mods,
               ~bind_rows(get_lavaan_fits(.x$Configural) %>% mutate(Model = "Configural"),
                          get_lavaan_fits(.x$Metric) %>% mutate(Model = "Metric"),
                          get_lavaan_fits(.x$Scalar) %>% mutate(Model = "Scalar")) %>%
                 rename_with(.cols = ends_with(".scaled"), .fn = ~str_remove(., "\\.scaled")) %>%
                 mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., 2), nsmall = 2))) %>%
                 mutate(CI95 = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
                 select(Model, n = ntotal, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, CI95, SRMR = srmr))

mi.tabs <- map2(.x = mi.fits, .y = mi.comps,
                ~left_join(.x, .y, by = "Model") %>%
                  flextab_format())


################################################################

## ICC
theiccs <- map(.x = c("HASSLES", "sdiscrim", "Empathy", "ALTRUISM"),
               ~performance::icc(lme4::lmer(formula(paste0(.x, " ~ 1 + (1|CSCHOOL)")), data = data2)))



# --------   Table 1   ------------------

study.vars <- data2 %>%
  select(Age = CAGE6, Female,
         Racial_Hassles = HASSLES, School.based_Discrimination = sdiscrim,
         Empathy, Altruism = ALTRUISM)

#### Correlations - Full Sample ####
# ## shortcut for recoding
# corrtop <- 1:ncol(study.vars)
# names(corrtop) <- names(study.vars)
# 
# ## correlations in long format
# corrs <- study.vars %>%
#   correlation::correlation(method = "pearson")
# 
# ## output table
# corrs.tab <- bind_rows(data.frame(column = "Age", top = 1:7, temp = NA) %>%
#                          spread(top, temp),
#                        corrs %>%
#                          mutate(Parameter2 = factor(Parameter2, levels = names(study.vars))) %>%
#                          arrange(Parameter2) %>%
#                          mutate(column = as.character(Parameter2) %>%
#                                   stringr::str_replace_all(., "_", " ") %>%
#                                   stringr::str_replace_all(., "\\.", "-") %>%
#                                   as_factor(.),
#                                 top = dplyr::recode(Parameter1, !!!corrtop),
#                                 tr = format(round(r, 2), nsmall = 2),
#                                 trs = paste0(tr,ifelse(p < .05, "*", " "))) %>%
#                          select(column, top, trs) %>%
#                          as.data.frame() %>%
#                          spread(top, trs)) %>%
#   mutate(`8` = NA) %>%
#   tibble::rownames_to_column("temp") %>%
#   unite(col = "Variable", temp, column, sep = ". ")

#### Correlations - By Gender ####
# separate matrices for females and males
corrs.mf <- map2(.x = c(1, 0), .y = c(TRUE, FALSE),
                 ~study.vars %>%
                   filter(Female == .x) %>%
                   select(-Female) %>%
                   corrr::correlate(method = "pearson", use = "pairwise.complete.obs") %>%
                   corrr::shave(upper = .y) %>%
                   mutate(across(.cols = -term, ~replace_na(., 0))) %>%
                   corrr::as_matrix()) %>%
  set_names(c("Girls", "Boys"))

# joining matrices
corrs.g <- corrs.mf$Girls + corrs.mf$Boys
diag(corrs.g) <- NA
# preparing for output
corrs.df <- as.data.frame(corrs.g) %>%
  tibble::rownames_to_column("v") %>%
  mutate(v = stringr::str_replace_all(v, "_", " ") %>%
           stringr::str_replace_all(., "\\.", "-")) %>%
  mutate(across(.cols = -v, ~format(round(., 2), nsmall = 2)))

## in case we need pvalues/stars
corrs.sig <- study.vars %>%
  group_by(Female) %>%
  correlation::correlation(method = "pearson") %>%
  as.data.frame() %>%
  select(-Method) %>%
  flextable() %>%
  colformat_double(j = c(4,6:10), digits = 3) %>%
  padding(padding = 1, part = "all") %>%
  bold(i = ~p < .05, part = "body") %>%
  autofit()


#### Descriptive Statistics - Full Sample ####
desc <- skimr::skim(study.vars, -Female) %>%
  mutate(missing_rate = format(round(1-complete_rate, 2), nsmall = 2),
         M = format(round(numeric.mean, 2), nsmall = 2),
         SD = format(round(numeric.sd, 2), nsmall = 2),
         Range = paste(format(round(numeric.p0, 1), nsmall = 1), "-",
                       format(round(numeric.p100, 1), nsmall = 1)),
         top = skim_variable) %>% #dplyr::recode(skim_variable, !!!corrtop)) %>%
  select(top, missing_rate, M, SD, Range) %>%
  gather(v, value, -top) %>%
  mutate(v = factor(v, levels = c("missing_rate", "M", "SD", "Range"))) %>%
  spread(top, value)



#### Creating Final Table ####
# Uses grouped correlation matrix and full sample descriptives
tab1 <- bind_rows(corrs.df,
                  data.frame(v = "Descriptive Statistics", top = names(corrs.df)[-1], temp = NA) %>%
                    spread(top, temp),
                  desc) %>%
  rename_with(.cols = everything(), ~stringr::str_replace_all(., "_", " ") %>%
                stringr::str_replace_all(., "\\.", "-")) %>%
  flextab_format() %>%
  hline(i = 5, j = NULL, border = officer::fp_border(width = 1), part = "body")


################################################################

# --------   Mean Differences   ------------------

table(as_factor(data2$CSCHOOL))
table(data2$CAGE6, as_factor(data2$CSCHOOL))
table(data2$DDistr2, as_factor(data2$CSCHOOL)) #District1 = Avondale; Disrict2 = West Bloomfield
# use Oak Park Prep as reference (largest school and was part of district originally used as reference)

mdformula <- "Avondale_Middle_School + Avondale_High_School + Oak_Park_High_School +
Oak_Park_Freshman_Institute + Orchard_Lake_Middle_School + West_Bloomfield_High_School"

# ## Gathering t-tests for each variable into single table
t.tab <- bind_rows(the_t(data2, HASSLES, Female, name = "Racial Hassles", covariates = mdformula),
                   the_t(data2, sdiscrim, Female, name = "School-based Discrimination", covariates = mdformula),
                   the_t(data2, Empathy, Female, name = "Empathy", covariates = mdformula),
                   the_t(data2, ALTRUISM, Female, name = "Altruism", covariates = mdformula))
 
## Output table design
t.map <- data.frame(col_keys = names(t.tab),
                    top = c("Variable", rep(c("Boys", "Girls"), each = 3), "t", "df", "p", "d"),
                    bottom = c("Variable", rep(c("n", "M", "SD"), times = 2), "t", "df", "p", "d"))

t.tab.flex <- t.tab %>%
  flextable() %>%
  two_level_flex(mapping = t.map, vert.cols = c(1,8:11), border = flex.border, digits = 2, dig.cols = c(3,4,6:11))


################################################################

# --------   Analytic Model   ------------------

#### Using Observed Scores ####

## base model
# means and covariances b/t exogenous variables treated as fixed (i.e. uses observed values)
# Oak_Park_Preparatory_Academy

## With self-made interaction variables
# fullmulti<-'#regressions
# ALTRUISM ~ sdiscrim_c + Empathy_c + HASSLES_c + Empsdiscrim + EmpHASS + CAGE6 +
# Avondale_Middle_School + Avondale_High_School + Oak_Park_High_School +
# Oak_Park_Freshman_Institute + Orchard_Lake_Middle_School + West_Bloomfield_High_School'

## with automated interactions - allows for comparison to main effects model below
fullmulti.int <- '#regressions
ALTRUISM ~ sdiscrim_c + Empathy_c + HASSLES_c + sdiscrim_c:Empathy_c + Empathy_c:HASSLES_c  + CAGE6 +
Avondale_Middle_School + Avondale_High_School + Oak_Park_High_School + 
Oak_Park_Freshman_Institute + Orchard_Lake_Middle_School + West_Bloomfield_High_School'

## unconstrained model
multigroup.full<-sem(fullmulti.int, data = data2, group ="CSEX",
                     missing = "FIML", estimator = "MLR")

# lavInspect(multigroup.full, "options")

## constrained model
multigroup.full.con <- sem(fullmulti.int, data = data2, group = "CSEX",
                           missing = "FIML", estimator = "MLR", group.equal = "regressions")

## comparison - likelihood ratio test and difference in fit indices
comp <- compare_mods(multigroup.full, multigroup.full.con, measures = c("cfi", "rmsea", "srmr")) %>%
  select(delta.chisq = "Chisq diff", delta.df = "Df diff", delta.p = "Pr(>Chisq)",
         delta.CFI = "cfi", delta.RMSEA = "rmsea", delta.SRMR = "srmr", delta.AIC = "AIC", delta.BIC = "BIC")

# joining model indices with comparison indices
indices <- map_dfr(.x = list(Free = multigroup.full, Constrained = multigroup.full.con),
                   ~get_lavaan_fits(.x, measures = "robust") %>%
                     round(., 2) %>%
                     mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.robust, nsmall = 2),
                                              ", ", format(rmsea.ci.upper.robust, nsmall = 2), "]")) %>%
                     rename_with(~stringr::str_remove(., "\\.robust|\\.scaled")) %>%
                     select(n = ntotal, x2 = chisq, df, p = pvalue, 
                            CFI = cfi, RMSEA = rmsea, RMSEA_95CI = rmsea.ci, SRMR = srmr,
                            AIC, BIC), .id = "Model") %>%
  bind_cols(bind_rows(comp[NA,], comp))
row.names(indices) <- NULL

indices.flex <- flextab_format(indices)

## extracting standardized parameters
con.params <- parameters::model_parameters(multigroup.full.con, component = "regression", standardize = TRUE) %>%
  filter(Group == 1) %>%
  select(From:p) %>%
  flextab_format(bold = "p") %>%
  flextable::colformat_double(j = -1, digits = 2)

uncon.params <- parameters::model_parameters(multigroup.full, component = "regression", standardize = TRUE) %>%
  # filter(Group == 1) %>%
  select(Group, From:p) %>%
  flextab_format() %>%
  flextable::colformat_double(j = -1, digits = 2)
# lavaanPlot::lavaanPlot(model = multigroup.full.con, coefs = TRUE, stand = FALSE, stars = TRUE)


#### Main Effects only model ####

## base model
# means and covariances b/t exogenous variables treated as fixed (i.e. uses observed values)
# Oak_Park_Preparatory_Academy
maineff <- '#regressions
ALTRUISM ~ sdiscrim_c + Empathy_c + HASSLES_c + 0*sdiscrim_c:Empathy_c + 0*Empathy_c:HASSLES_c + CAGE6 +
Avondale_Middle_School + Avondale_High_School + Oak_Park_High_School +
Oak_Park_Freshman_Institute + Orchard_Lake_Middle_School + West_Bloomfield_High_School'

## unconstrained model
maineff.full<-sem(maineff, data = data2, group ="CSEX",
                     missing = "FIML", estimator = "MLR") # fixed = TRUE is the default

# lavInspect(multigroup.full, "options")

## constrained model
maineff.full.con <- sem(maineff, data = data2, group = "CSEX",
                           missing = "FIML", estimator = "MLR", group.equal = "regressions")

## comparison - likelihood ratio test and difference in fit indices
maineff.comp <- compare_mods(maineff.full, maineff.full.con, measures = c("cfi", "rmsea", "srmr")) %>%
  select(delta.chisq = "Chisq diff", delta.df = "Df diff", delta.p = "Pr(>Chisq)",
         delta.CFI = "cfi", delta.RMSEA = "rmsea", delta.SRMR = "srmr", delta.AIC = "AIC", delta.BIC = "BIC")

# joining model indices with comparison indices
maineff.indices <- map_dfr(.x = list(Free = maineff.full, Constrained = maineff.full.con),
                   ~get_lavaan_fits(.x, measures = "robust") %>%
                     round(., 2) %>%
                     mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.robust, nsmall = 2),
                                              ", ", format(rmsea.ci.upper.robust, nsmall = 2), "]")) %>%
                     rename_with(~stringr::str_remove(., "\\.robust|\\.scaled")) %>%
                     select(n = ntotal, x2 = chisq, df, p = pvalue, 
                            CFI = cfi, RMSEA = rmsea, RMSEA_95CI = rmsea.ci, SRMR = srmr,
                            AIC, BIC), .id = "Model") %>%
  bind_cols(bind_rows(maineff.comp[NA,], maineff.comp))
row.names(maineff.indices) <- NULL

maineff.indices.flex <- flextab_format(maineff.indices)

## extracting standardized parameters
maineff.con.params <- parameters::model_parameters(maineff.full.con, component = "regression", standardize = TRUE) %>%
  filter(Group == 1) %>%
  select(From:p) %>%
  flextab_format(bold = "p") %>%
  flextable::colformat_double(j = -1, digits = 2)

maineff.uncon.params <- parameters::model_parameters(maineff.full, component = "regression", standardize = TRUE) %>%
  # filter(Group == 1) %>%
  select(Group, From:p) %>%
  flextab_format() %>%
  flextable::colformat_double(j = -1, digits = 2)
# lavaanPlot::lavaanPlot(model = multigroup.full.con, coefs = TRUE, stand = FALSE, stars = TRUE)

# empathy::hassle interaction non-significant
anova(multigroup.full.con, maineff.full.con)
anova(multigroup.full, maineff.full)

#### Combined Results Tables for Interaction and Main Effects Models ####
## Constrained
combo.con.params <-  parameters::model_parameters(multigroup.full.con, component = "regression", standardize = TRUE) %>%
  filter(Group == 1) %>%
  mutate(`95% CI` = paste0("[", format(round(CI_low, 2), nsmall = 2),
                           ", ", format(round(CI_high, 2), nsmall = 2), "]")) %>%
  select(From, Coefficient, `95% CI`, p) %>%
  left_join(parameters::model_parameters(maineff.full.con, component = "regression", standardize = TRUE) %>%
              filter(Group == 1) %>%
              mutate(`95% CI` = paste0("[", format(round(CI_low, 2), nsmall = 2),
                                       ", ", format(round(CI_high, 2), nsmall = 2), "]")) %>%
              select(From, Coefficient, `95% CI`, p), by = "From")

## Output table design
combo.con.params.map <- data.frame(col_keys = names(combo.con.params),
                    top = c("Variable", rep(c("Interaction Model", "Main Effects Model"), each = 3)),
                    bottom = c("Variable", rep(c("Beta", "95% CI", "p"), times = 2)))

combo.con.params.flex <- combo.con.params %>%
  flextable() %>%
  two_level_flex(mapping = combo.con.params.map, vert.cols = c(1), border = flex.border, digits = 2, dig.cols = c(2,4,5,7))

## Unconstrained
combo.unc.params <-  parameters::model_parameters(multigroup.full, component = "regression", standardize = TRUE) %>%
  mutate(`95% CI` = paste0("[", format(round(CI_low, 2), nsmall = 2),
                           ", ", format(round(CI_high, 2), nsmall = 2), "]")) %>%
  select(Group, From, Coefficient, `95% CI`, p) %>%
  left_join(parameters::model_parameters(maineff.full, component = "regression", standardize = TRUE) %>%
              mutate(`95% CI` = paste0("[", format(round(CI_low, 2), nsmall = 2),
                                       ", ", format(round(CI_high, 2), nsmall = 2), "]")) %>%
              select(Group, From, Coefficient, `95% CI`, p), by = c("Group", "From"))

## Output table design
combo.unc.params.map <- data.frame(col_keys = names(combo.unc.params),
                                   top = c("Group", "Variable", rep(c("Interaction Model", "Main Effects Model"), each = 3)),
                                   bottom = c("Group", "Variable", rep(c("Beta", "95% CI", "p"), times = 2)))

combo.unc.params.flex <- combo.unc.params %>%
  flextable() %>%
  two_level_flex(mapping = combo.unc.params.map, vert.cols = c(1,2), border = flex.border, digits = 2, dig.cols = c(3,5,6,8))



lavInspect(multigroup.full.con, "r2")
lavInspect(maineff.full.con, "r2")
summary(multigroup.full.con, standardized = TRUE)
summary(multigroup.full.int, standardized = TRUE)
summary(noemphass.full, standardized = TRUE)


################################################################

# save_as_docx(`Table 1` = tab1, `Corr P` = corrs.sig,
#                   `T table` = t.tab.flex, 
#                   `Fit Indices` = indices.flex,
#                   `Constrained Parameters` = con.params,
#              path = "AA multigroup results.docx")



#### Exporting Results #####
save(data2, mi.mods, mi.tabs, theiccs, tab1, corrs.sig,
     t.tab.flex, indices.flex, con.params, uncon.params,
     maineff.indices.flex, maineff.con.params, maineff.uncon.params,
     combo.con.params.flex, combo.unc.params.flex,
     file = "Altruism_Results.RData")

# rmarkdown::render(input = "AA_Altruism_Results.Rmd",
#                   output_format = "word_document",
#                   output_file = "AA multigroup results")



##plotting observed values for each potential interaction with gender and IVs (change x = for each scale)
# data2 %>%
#   filter(!is.na(CSEX)) %>%
#   ggplot(aes(x = sdiscrim_c, y = ALTRUISM, group = CSEX, colour = CSEX)) +
#   geom_smooth(method = "lm", se = TRUE) +
#   theme_bw(base_size = 18)


