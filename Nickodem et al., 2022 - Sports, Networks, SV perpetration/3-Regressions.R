#########################################
#                                       #
#         Paper 2 - Sports & SV         #
#            Outcome Analysis           #
#                                       #
#########################################

## Loading packages and functions
source("Data and Scripts/0-PnF.R")
load("Output/Prep_and_Descriptives.RData")
load("Output/Imputated.RData")
library(performance)
library(lme4)
library(mice)
library(miceadds)
# library(merTools)

# Wave 4 participation rate - from CONSORT diagram in Sources Outcomes paper
(2097 + 1416)/(3086 + 2040)

#### Conditional Distributions ####
## by contact level
dist.plot.contact <- dsconscales %>%
  mutate(Contact_Cat = factor(Contact_Cat, levels = rev(c("No sport", "Non", "Low", "High")))) %>%
  cond_dist_plot(Contact_Cat)

## by gender
dist.plot.gen <- dsconscales %>%
  mutate(Gender = as_factor(Gender)) %>%
  cond_dist_plot(Gender)

##############################
####      Analysis 1      ####

## Variables for analysis 1 and recoded names for output
a1recode <- c("(Intercept)", a1regvars[c(2:4, 7, 8, 10:18, 21:37, 19, 20)], "R2")
names(a1recode) <- c("Intercept", "Non-Contact", "Low-Contact", "High-Contact",
                      "Indegree", "Outdegree", "Coreness", "Egocentric Density",
                      "TA Total Nominations", "TA - Administrator", "TA - Counselor",
                      "TA - Health Worker", "TA - Paraprofessional", "TA - Support Staff",
                      "TA - Teacher", "Substance Use", "SV Dismissiveness", "Girl", "Other Gender",
                      "Hispanic", "Person of Color", "Sexual Minority", "Intervention",
                      "High-Contact x Girl", "Low-Contact x Girl", "Non-Contact x Girl",
                      "High-Contact x SVD", "Low-Contact x SVD", "Non-Contact x SVD",
                      "High-Contact x SU", "Low-Contact x SU", "Non-Contact x SU",
                      "Prior SH Perpetration", "Prior SV Perpetration",
                      "R2")

# ----- Descriptive Statistics --------------

## calculate descriptives for these items
a1descripvars <- dsconscales %>%
  select(SH_Perp_W4_di, SV_Perp_W4_di, all_of(a1regvars[c(19, 20, 7:8, 10:18, 21, 22:28)])) %>% names()
a1contvars <- dsconscales %>%
  select(indegree, outdegree, coreness, egodenout, TotAdultNoms, Substance_Use, Dismissiveness) %>% names() # continuous variables
a1catvars <- a1descripvars[!(a1descripvars %in% a1contvars)] # categorical


#### Listwise deleted descriptive statistics ####
a1descrips.ld <- dsconscales %>%
  select(Contact_Cat, all_of(a1descripvars)) %>%
  mutate(Contact_Cat = fct_rev(Contact_Cat)) %>%
  # mutate(Contact_Cat = fct_explicit_na(Contact_Cat, na_level = "Missing")) %>%
  tbl_summary(by = Contact_Cat, missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              label = map(which(a1recode %in% a1descripvars), ~as.formula(paste0(a1recode[[.x]], " ~ '", names(a1recode)[[.x]], "'"))),
              type = list(all_of(a1contvars) ~ "continuous")) %>%
  add_p() %>%
  add_q() %>%
  # add_overall() %>%
  as_flex_table()

#### Multiply Imputed descriptive statistics ####

## Creating the Contact_Cat variable in the imputed datasets
a1implong <- complete(a1imp, "long")
a1implong <- a1implong %>%
  mutate(Contact_Cat = case_when(No_sports == 1 ~ "No Sports",  
                                 Non_Contact == 1 ~ "Non-Contact",
                                 Low_Contact == 1 ~ "Low Contact",
                                 High_Contact == 1 ~ "High Contact",
                                 TRUE ~ NA_character_), # 1968 out of 175300 cases are NA b/c they are 0 for all variables I believe as a result of imputation
         Contact_Cat = factor(Contact_Cat, levels = c("No Sports", "Non-Contact", "Low Contact", "High Contact")))
prop.table(table(a1implong$Non_Contact, a1implong$Contact_Cat), margin = 1)
table(a1implong$Non_Contact, a1implong$Contact_Cat)

## creating mids object with Contact_Cat variable
a1middy <- map(.x = 1:50, ~a1implong %>%
               filter(.imp == .x)) %>%
  datalist2mids()

## pool ANOVA on continuous variables
# Uses D1 statistic (Li, Raghunathan, and Rubin, 1991); https://stefvanbuuren.name/fimd/sec-multiparameter.html)
# I do not think D1 function uses Reiter's (2007) small sample size adjustment to the degrees of freedom which Grund, Lüdtke, and Robitzsch (2016b) call D1*, though that shouldn't be necessary here
a1aov <- map(a1contvars, ~with(data = a1middy, lm(as.formula(paste0(.x, " ~ Contact_Cat"))))) %>%
  set_names(a1contvars)
a1aovpooldf <- map(.x = a1aov, ~D1(.x)) %>%
  map_dfr(.x = ., ~as.data.frame(.x$result), .id = "Variable") %>%
  mutate(statistic = paste0(format(round(F.value, 2), nsmall = 2),
                            " (", df1, ", ", format(round(df2, 1), nsmall = 1), ")"))

## pool glm on categorical variables also with D1 statistic
a1chi <- map(a1catvars, ~with(data = a1middy, glm(as.formula(paste0(.x, " ~ Contact_Cat")), family = binomial(link = "logit")))) %>%
  set_names(a1catvars)
a1chipooldf <- map(.x = a1chi, ~D1(.x)) %>%
  map_dfr(.x = ., ~as.data.frame(.x$result), .id = "Variable") %>%
  mutate(statistic = paste0(format(round(F.value, 2), nsmall = 2),
                            " (", df1, ", ", format(round(df2, 1), nsmall = 1), ")"))

## Calculating descriptives and joining testing results - average across imputations
# continuous variables - joining anova results
a1descrips.cont <- a1implong %>%
  select(.imp, .id, Contact_Cat, all_of(a1contvars)) %>%
  filter(!is.na(Contact_Cat)) %>%
  gather(Variable, Value, all_of(a1contvars)) %>%
  group_by(Contact_Cat, Variable) %>%
  summarize(M = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE)) %>%
  mutate(across(c(M, SD), ~format(round(.x, 2), nsmall = 2))) %>%
  mutate(estimate = paste0(M, " (", SD, ")")) %>%
  select(Contact_Cat, Variable, estimate) %>%
  spread(Contact_Cat, estimate) %>%
  left_join(a1aovpooldf %>%
              select(Variable, statistic, `P(>F)`),
            by = "Variable")


# average total n by contact category
a1contactcatn <- data.frame(table(a1implong$Contact_Cat) / 50) %>%
  rename(Contact_Cat = Var1, total = Freq)

# categorical variables, then joining D1 statistic, binding continous and rearranging
a1descrips.mi <- a1implong %>%
  select(.imp, .id, Contact_Cat, all_of(a1catvars)) %>%
  filter(!is.na(Contact_Cat)) %>%
  gather(Variable, Value, all_of(a1catvars)) %>%
  group_by(Contact_Cat, Variable) %>%
  summarize(n = sum(as.numeric(Value)) / 50, .groups = "drop") %>%
  left_join(a1contactcatn, by = "Contact_Cat") %>%
  mutate(percent = n / total *100) %>%
  mutate(across(c(n, total, percent), ~format(round(.x, 1), nsmall = 1))) %>%
  mutate(estimate = paste0(n, " (", percent, ("%)"))) %>%
  select(Contact_Cat, Variable, estimate) %>%
  spread(Contact_Cat, estimate) %>%
  left_join(a1chipooldf %>%
              select(Variable, statistic, `P(>F)`),
            by = "Variable") %>%
  bind_rows(a1descrips.cont) %>%
  mutate(p = p.adjust(`P(>F)`, "fdr"),
         p = format(round(p, 3), nsmall = 3),
         Variable = factor(Variable, levels = a1descripvars)) %>%
  arrange(Variable) %>%
  mutate(Variable = fct_recode(Variable, `W4 SH Perpetration` = "SH_Perp_W4_di", `W4 SV Perpetration` = "SV_Perp_W4_di",
                               !!!a1recode[a1recode %in% a1descripvars]))

##################################################

# ----   Correlations    -------------

regcorrs <- select(dsconscales, SH_Perp_W4_di, SV_Perp_W4_di, all_of(a1regvars)) %>%
  correlation::correlation(method = "pearson") # "auto" produces error
# cor(use = "pairwise.complete.obs", method = "pearson")
correlation::cor_test(data = dsconscales, x = "SH_Perp_Prior_di", y = "SH_Perp_W4_di", method = "tetrachoric")
table(dsconscales$SH_Perp_Prior_di, dsconscales$SH_Perp_W4_di, useNA = "always")
correlation::cor_test(data = dsconscales, x = "SV_Perp_Prior_di", y = "SV_Perp_W4_di", method = "tetrachoric")
table(dsconscales$SV_Perp_Prior_di, dsconscales$SV_Perp_W4_di, useNA = "always")

# round(regcorrs[, 1:4], 2)
#   corrplot::corrplot.mixed(tl.pos = "d", tl.col="black",
#                            order = "original")

#############################################

# -----    Running Models - Listwise Deletion  -----------

#### With cluster robust standard errors ####
## Used for parameter estimates
a1.ldc <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
              .y = list(c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20),
                        c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20)),
             ~glm.cluster(formula = as.formula(paste0(.x, " ~ ", paste(a1regvars[.y], collapse = " + "))),
                     data = dsconscales,
                     cluster = dsconscales$School,
                     family = binomial(link = "logit"))) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### Without cluster robust standard errors ####
# used to gather r2 and conduct model checks
# the functions to do so don't have methods for .cluster class
a1.lds <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
              .y = list(c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20),
                        c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20)),
              ~glm(formula = as.formula(paste0(.x, " ~ ", paste(a1regvars[.y], collapse = " + "))),
                           data = dsconscales,
                           family = binomial(link = "logit"))) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))


## Mulilevel model?
# sh.mlm <- glmer(as.formula(paste0("SH_Perp_W4_di ~ ", paste(a1regvars[-c(c(2:4, 7, 8, 10:37))], collapse = " + "), " + (1|School)")), data = dsconscales, family = binomial(link = "logit"))
# sv.mlm <- glmer(as.formula(paste0("SV_Perp_W4_di ~ ", paste(a1regvars[-c(c(2:4, 7, 8, 10:37))], collapse = " + "), " + (1|School)")), data = dsconscales, family = binomial(link = "logit"))


#### Model checking, performance, and parameters ####

# checks and performance metrics run with non-cluster robust SEs
# Note: estimates have been converted to Odds ratio along with the CI, standard error is still for log odds.
a1.ld.checks <- lapply(a1.lds, model_check)
a1.ld.perform <- lapply(a1.lds, performance::model_performance)
a1.ld.summary <- lapply(a1.ldc, summary)
a1.ld.params <- map(a1.ld.summary, ~ as.data.frame(.x)  %>%
                      tibble::rownames_to_column("Variable") %>%
                      mutate(OR = exp(Estimate),
                             CI_low = exp(Estimate - `Std. Error`*1.959964),
                             CI_high = exp(Estimate + `Std. Error`*1.959964)) %>%
                      select(Variable, OR, CI_low, CI_high, everything(), p = `Pr(>|z|)`))

## output table
a1.ld.out <- parameter_table(a1.ld.params) %>%
  mutate(Variable = names(a1recode)[-35])

##########################################################

# ----   Running Models - Multiple Imputation   --------------
a1implist <- mids2datlist(a1imp) # converts mids object to list of dataframes for glm.cluster

#### with cluster robust standard errors ####
## Used for parameter estimates
## 4 models - SH/SV by research questions/prior perpetration
a1.mic <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
              .y = list(c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20),
                        c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20)),
              ~lapply(a1implist, function(data){
                glm.cluster(formula = as.formula(paste0(.x, " ~ ", paste(a1regvars[.y], collapse = " + "))),
                            data = data,
                            cluster = dsconscales$School,
                            family = binomial(link = "logit"))
              })) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### without cluster robust standard errors ####
# used to gather r2 and conduct model checks
# the functions to do so don't have methods for .cluster class
a1.mis <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
               .y = list(c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20),
                         c(2:4, 7, 8, 10:18, 21:37), c(2:4, 7, 8, 10:18, 21:37, 19, 20)),
               ~lapply(a1implist, function(data){
                 glm(formula = as.formula(paste0(.x, " ~ ", paste(a1regvars[.y], collapse = " + "))),
                             data = data,
                             family = binomial(link = "logit"))
               })) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### Model performance and parameters ####
# Note: estimates have been converted to Odds ratio along with the CI, standard error is still for log odds.
a1.mi.pool <- map(a1.mic, ~pool_cluster(.x))
a1.mi.params <- map(a1.mi.pool, ~ .x$glm_stats %>%
                      tibble::rownames_to_column("Variable") %>%
                      mutate(OR = exp(results),
                             CI_low = exp(results - se*1.959964),
                             CI_high = exp(results + se*1.959964)) %>%
                      select(Variable, OR, CI_low, CI_high, everything()))
a1.mi.r2 <- map(.x = a1.mis, ~pool_rsquared(.x)) # takes awhile because performance::r2 is pretty slow


#### Combining Results into output ####
a1.mi.out <- parameter_table(a1.mi.params, a1.mi.r2) %>%
  mutate(Variable = names(a1recode)) # can't use fct_recode b/c the regression adds "1" to the end of each dichotomous variable

########################################

######################################################

##############################
####      Analysis 2      ####

## Variables for analysis 2 and recoded names for output
a2recode <- c("(Intercept)", a1regvars[c(2:4, 21:24, 27,  7)], a2regvars, a1regvars[c(19, 20)], "R2")
names(a2recode) <- c("Intercept", "Non-Contact", "Low-Contact", "High-Contact",
                     "Substance Use", "SV Dismissiveness", "Girl", "Other Gender",
                     "Sexual Minority", "Indegree", "Reciprocation Rate",
                     "%Fr Same Gender", "%Fr Same Race", "%Fr Same Sexual Orientation",
                     "%Fr Prior SH Perpetration", "%Fr Prior SV Perpetration",
                     "%Fr W4 SH Perpetration", "%Fr W4 SV Perpetration",
                     "Avg. Fr SV Dismissiveness", "Avg. Fr Substance Use",
                     "%Fr No Sports", "%Fr Non-Contact", "%Fr Low-Contact", "%Fr High-Contact",
                     "Prior SH Perpetration", "Prior SV Perpetration",
                     "R2")


# ----- Descriptive Statistics --------------

## calculate descriptives for these items
a2descripvars <- dsnoniso %>% select(all_of(a2regvars)) %>% names() # all are continuous

#### Listwise deleted descriptive statistics ####
a2descrips.ld <- dsnoniso %>%
  select(Contact_Cat, all_of(a2descripvars)) %>%
  mutate(Contact_Cat = fct_rev(Contact_Cat)) %>%
  # mutate(Contact_Cat = fct_explicit_na(Contact_Cat, na_level = "Missing")) %>%
  tbl_summary(by = Contact_Cat, missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              label = map(which(a2recode %in% a2descripvars), ~as.formula(paste0(a2recode[[.x]], " ~ '", names(a2recode)[[.x]], "'"))),
              type = list(all_of(a2descripvars) ~ "continuous")) %>%
  add_p() %>%
  add_q() %>%
  # add_overall() %>%
  as_flex_table()


#### Multiply imputed descriptive statistics ####
## Creating the Contact_Cat variable in the imputed datasets
a2implong <- complete(a2imp, "long")
a2implong <- a2implong %>%
  mutate(Contact_Cat = case_when(No_sports == 1 ~ "No Sports",
                                 Non_Contact == 1 ~ "Non-Contact",
                                 Low_Contact == 1 ~ "Low Contact",
                                 High_Contact == 1 ~ "High Contact",
                                 TRUE ~ NA_character_), # 346 out of 83250 cases are NA b/c they are 0 for all variables as a result of imputation
         Contact_Cat = factor(Contact_Cat, levels = c("No Sports", "Non-Contact", "Low Contact", "High Contact")))


## creating mids object with Contact_Cat variable
a2middy <- map(.x = 1:50, ~a2implong %>%
                 filter(.imp == .x)) %>%
  datalist2mids()

## pool ANOVA on continuous variables
# Uses D1 statistic (Li, Raghunathan, and Rubin, 1991); https://stefvanbuuren.name/fimd/sec-multiparameter.html)
# I do not D1 function uses Reiter's (2007) small sample size adjustment to the degrees of freedom which Grund, Lüdtke, and Robitzsch (2016b) call D1*, though that shouldn't be necessary here
a2aov <- map(a2descripvars, ~with(data = a2middy, lm(as.formula(paste0(.x, " ~ Contact_Cat"))))) %>%
  set_names(a2descripvars)
a2aovpooldf <- map(.x = a2aov, ~D1(.x)) %>%
  map_dfr(.x = ., ~as.data.frame(.x$result), .id = "Variable") %>%
  mutate(statistic = paste0(format(round(F.value, 2), nsmall = 2),
                            " (", df1, ", ", format(round(df2, 1), nsmall = 1), ")"))

## Calculating descriptives and joining testing results - average across imputations
# continuous variables - joining anova results
a2descrips.mi <- a2implong %>%
  select(.imp, .id, Contact_Cat, all_of(a2descripvars)) %>%
  filter(!is.na(Contact_Cat)) %>%
  gather(Variable, Value, all_of(a2descripvars)) %>%
  group_by(Contact_Cat, Variable) %>%
  summarize(M = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE)) %>%
  mutate(across(c(M, SD), ~format(round(.x, 2), nsmall = 2))) %>%
  mutate(estimate = paste0(M, " (", SD, ")")) %>%
  select(Contact_Cat, Variable, estimate) %>%
  spread(Contact_Cat, estimate) %>%
  left_join(a2aovpooldf %>%
              select(Variable, statistic, `P(>F)`),
            by = "Variable") %>%
  mutate(p = p.adjust(`P(>F)`, "fdr"),
         p = format(round(p, 3), nsmall = 3),
         Variable = factor(Variable, levels = a2descripvars)) %>%
  arrange(Variable) %>%
  mutate(Variable = fct_recode(Variable, !!!a2recode[a2recode %in% a2descripvars]))


# average total n by contact category
a2contactcatn <- data.frame(table(a2implong$Contact_Cat) / 50) %>%
  rename(Contact_Cat = Var1, total = Freq)

###############################################

# ----- Correlations ------

correlation::cor_test(data = dsnoniso, x = "SH_Perp_Prior_di", y = "SH_Perp_W4_di", method = "tetrachoric")
table(dsnoniso$SH_Perp_Prior_di, dsnoniso$SH_Perp_W4_di)

#################################

# ----   Running Models - Listwise Deletion   --------------

#### With cluster robust standard errors ####
## Used for parameter estimates
a2.ldc <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
               .y = list(c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)]),
                         c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)])),
               ~glm.cluster(formula = as.formula(paste0(.x, " ~ ", paste(.y, collapse = " + "))),
                            data = dsnoniso,
                            cluster = dsnoniso$School,
                            family = binomial(link = "logit"))) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### Without cluster robust standard errors ####
# used to gather r2 and conduct model checks
# the functions to do so don't have methods for .cluster class
a2.lds <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
               .y = list(c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)]),
                         c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)])),
               ~glm(formula = as.formula(paste0(.x, " ~ ", paste(.y, collapse = " + "))),
                    data = dsnoniso,
                    family = binomial(link = "logit"))) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### Model checking, performance, and parameters ####
# checks and performance metrics run with non-cluster robust SEs
# Note: estimates have been converted to Odds ratio along with the CI, standard error is still for log odds.
a2.ld.checks <- lapply(a2.lds, model_check)
a2.ld.perform <- lapply(a2.lds, performance::model_performance)
a2.ld.summary <- lapply(a2.ldc, summary)
a2.ld.params <- map(a2.ld.summary, ~ as.data.frame(.x)  %>%
                      tibble::rownames_to_column("Variable") %>%
                      mutate(OR = exp(Estimate),
                             CI_low = exp(Estimate - `Std. Error`*1.959964),
                             CI_high = exp(Estimate + `Std. Error`*1.959964)) %>%
                      select(Variable, OR, CI_low, CI_high, everything(), p = `Pr(>|z|)`))

## output table
a2.ld.out <- parameter_table(a2.ld.params) %>%
  mutate(Variable = names(a2recode)[-27])



##########################################


# ----   Running Models - Multiple Imputation   --------------
a2implist <- mids2datlist(a2imp) # converts mids object to list of dataframes for glm.cluster


#### with cluster robust standard errors ####
## Used for parameter estimates
## 4 models - SH/SV by research questions/prior perpetration
a2.mic <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
               .y = list(c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)]),
                         c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)])),
               ~lapply(a2implist, function(data){
                 glm.cluster(formula = as.formula(paste0(.x, " ~ ", paste(.y, collapse = " + "))),
                             data = data,
                             cluster = dsnoniso$School,
                             family = binomial(link = "logit"))
               })) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### without cluster robust standard errors ####
# used to gather r2 and conduct model checks
# the functions to do so don't have methods for .cluster class
a2.mis <- map2(.x = c("SH_Perp_W4_di", "SH_Perp_W4_di", "SV_Perp_W4_di", "SV_Perp_W4_di"),
               .y = list(c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)]),
                         c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars), c(a1regvars[c(2:4, 21:24, 27, 7)], a2regvars, a1regvars[c(19, 20)])),
               ~lapply(a2implist, function(data){
                 glm(formula = as.formula(paste0(.x, " ~ ", paste(.y, collapse = " + "))),
                     data = data,
                     family = binomial(link = "logit"))
               })) %>%
  set_names(c("Harassment_RQ", "Harassment_PP", "Violence_RQ", "Violence_PP"))

#### Model performance and parameters ####
# Note: estimates have been converted to Odds ratio along with the CI, standard error is still for log odds.
a2.mi.pool <- map(a2.mic, ~pool_cluster(.x))
a2.mi.params <- map(a2.mi.pool, ~ .x$glm_stats %>%
                      tibble::rownames_to_column("Variable") %>%
                      mutate(OR = exp(results),
                             CI_low = exp(results - se*1.959964),
                             CI_high = exp(results + se*1.959964)) %>%
                      select(Variable, OR, CI_low, CI_high, everything()))
a2.mi.r2 <- map(.x = a2.mis, ~pool_rsquared(.x)) # takes awhile because performance::r2 is pretty slow

#### Combining Results into output ####
a2.mi.out <- parameter_table(a2.mi.params, a2.mi.r2) %>%
  mutate(Variable = names(a2recode)) # can't use fct_recode b/c the regression adds "1" to the end of each dichotomous variable


##########################################

####################################################################

# ----- Saving Results ---------------------
save(a1regvars, a2regvars, a1descripvars, a2descripvars, a1recode, a2recode,
     a1descrips.ld, a1descrips.mi, a2descrips.ld, a2descrips.mi, a1contactcatn, a2contactcatn,
     a1.ldc, a1.lds, a1.ld.checks, a1.ld.perform, a1.ld.params, a1.ld.out,
     a1.mic, a1.mis, a1.mi.pool, a1.mi.r2, a1.mi.params, a1.mi.out,
     a2.ldc, a2.lds, a2.ld.checks, a2.ld.perform, a2.ld.params, a2.ld.out,
     a2.mic, a2.mis, a2.mi.pool, a2.mi.r2, a2.mi.params, a2.mi.out,
     file = "Output/Regression_Results.RData")


# load("Output/Regressions_Results.RData")
