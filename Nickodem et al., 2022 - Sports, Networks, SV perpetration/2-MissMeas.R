#############################################
#                                           #
#         Paper 2 - Sports & SV             #
#   Missing Data and Measurement Validity   #
#                                           #
#############################################

## Loading packages and functions
source("Data and Scripts/0-PnF.R")
load("Output/Prep_and_Descriptives.RData")
library(naniar)
library(mice)

# ----- Specifying predictors ----------------------
## Analysis 1
# Note. at this point, if changes are made to the a1regvars, then need to change all the indexing in the remaining script
a1regvars <- dsconscales %>%
  select(No_sports, Non_Contact, Low_Contact, High_Contact, Contact_Years, Multisport,
         indegree, outdegree, betweennessStd, coreness, egodenout,  #reciprate is NA for isolated (outdegree = 0) students (n = 1183)
         TotAdultNoms, paste0(adulttypevars, "_di"),                          
         SH_Perp_Prior_di, SV_Perp_Prior_di, Substance_Use, Dismissiveness,
         Female, Other, Latinx, POC, LGBQ, Tx,
         High_ContactxFemale, Low_ContactxFemale, Non_ContactxFemale,
         High_ContactxDismiss, Low_ContactxDismiss, Non_ContactxDismiss,
         High_ContactxSU, Low_ContactxSU, Non_ContactxSU) %>%
  names()

## Analysis 2 - adding friend variables
a2regvars <- dsconscales %>%
  select(reciprate, ppfrSameG, ppfrSameR, ppfrSameSO,
         ppfr_SH_Prior:ppfr_High_Contact) %>%
  names()

## combines a1regvars and a2regvars excluding the interactions
allregvars <- c(a1regvars[c(1:28)], a2regvars)


####################################################################

# ---- Correlations ----------

#### All potential predictors ####
predcorrs <- dsconscales %>%
  select(SH_Perp_W4_di, SV_Perp_W4_di, all_of(a1regvars)) %>%
  # cor(use = "pairwise.complete.obs")
  correlation::correlation(method = "pearson") # "auto" produces error
# although all variables are numeric would prefer dichotomous corrs to be polychoric

# lapply(a1regvars, function(x) class(dsconscales[[x]]))

# corrplot::corrplot.mixed(thecorrs, tl.pos = "d", tl.col="black", order = "hclust")
# View(round(thecorrs, 2))


#### Scale Items ####
scalecorrs <- map(.x = Scales,
                  ~cor(dsconscales[.x], use = "pairwise.complete.obs", method = "spearman") %>%
                    corrplot::corrplot.mixed(tl.pos = "d", tl.col = "black"))

## CONCLUSIONS:
# Hard to get a read on the predictors
# Nothing out of the ordinary from the scales

###################################################


# -------- Confirmatory Factor Analysis ---------------

cfa.mod <- map2(.x = c(Scales, list(Scales$Dismissiveness[-1])),
                .y = c("SH", "SH","SV", "SV", "AOD", "Dismiss", "Dismiss"),
                ~cfa_wrapper(data = dsconscales,
                             items = .x, fname = .y,
                             estimator = "WLSMV",
                             missing = "pairwise",
                             mimic = "lavaan"))
                             # cluster = "School")) # new version of lavaan generates error

# high correlations b/t SV perp 8, 9, 10, 11; 11, 12, 13, even after collapsing categories
# dsconscales %>% select(all_of(contactperpvars), SV_Perp_W4) %>% View()
# sv.cor <- lavInspect(perp.cfa[[2]], "sampstat")


lapply(cfa.mod, "[[", "reliabilities")
lapply(cfa.mod, "[[", "fits")
lapply(cfa.mod, "[[", "loadings")
modindices(cfa.mod$Dismiss$cfas, sort. = TRUE)
# item 1 might be problematic; removing it improves chisq tremendously

## CONCLUSIONS:
# SH and AOD are fine
# SV has high residual correlations - drop or combine items? We end up dichotomizing the scale
# Dismiss fits better dropping first item. Theoretical reason?

#############################################################


# ------ ICCs -----------

iccs <- map(.x = c("SH_Perp_Prior", "SH_Perp_W4", "SV_Perp_Prior", "SV_Perp_W4"),
            ~lme4::lmer(formula(paste(.x, " ~ 1 + (1|School)")), data = dsconscales) %>%
              performance::icc(.))

# all < 2%

icc.logs <- map(.x = c("SH_Perp_Prior_di", "SH_Perp_W4_di", "SV_Perp_Prior_di", "SV_Perp_W4_di"),
                ~lme4::glmer(formula(paste(.x, " ~ 1 + (1|School)")), data = dsconscales, family = binomial(link = "logit")) %>%
                  performance::icc(.))

# all < 3%

###############################################################


# ---- Missing Data Analysis ------------------

#### Missingness by case and variable ####
## analysis 1 dataset
a1missoverview <- dsconscales %>%
  select(SH_Perp_W4_di, SV_Perp_W4_di, all_of(a1regvars[c(1:4, 7,8, 10:37)])) %>% # removes contact_years, multisport, and betweeness
  miss_summary()

# by Variable
View(a1missoverview$miss_var_summary[[1]])


# Tx, indegree, and coreness had no missing
# Proxies for missingness:
# No_sports for all sports variables,POC for race, Female for Gender,
# reciprocate for friend variables, TotAdultNoms for adult nominations

## analysis 2 dataset
a2missoverview <- dsnoniso %>%
  select(SH_Perp_W4_di, SV_Perp_W4_di, all_of(allregvars[c(1:4, 19:42)])) %>%
  miss_summary()

# by Variable
View(a2missoverview$miss_var_summary[[1]])
# % friend variables had similar missingness rate to self-report variables (i.e. high for sports)


#### Predicting missingness in response variables (SV_Perp_W4, SH_Perp_W4) ####

## Adding missingness indicators to dataset
withshadow <- dsconscales %>%
  select(StudentID, School, SV_Perp_W4_di, SH_Perp_W4_di,
         SV_Perp_W4, SH_Perp_W4, SV_Perp_Prior, SH_Perp_Prior,
         all_of(allregvars)) %>%
  bind_shadow(only_miss = TRUE)

## variables to predict missingness on outcomes
shadowvars <- withshadow %>%
  select(SV_Perp_Prior_di_NA, SH_Perp_Prior_di_NA, No_sports_NA,
         betweennessStd_NA, egodenout_NA, reciprate_NA, TotAdultNoms_NA,
         Substance_Use_NA, Dismissiveness_NA, Female_NA, POC_NA, LGBQ_NA) %>%
  names()

## predicting outcome by missingness on x variables
ybymiss.mod <- map(.x = c("SV_Perp_W4_di", "SH_Perp_W4_di"),
                ~glm(formula(paste0(.x, " ~ ",
                               paste(shadowvars[-c(4,5,7,11)], collapse = " + "))),
                data = withshadow, family = binomial(link = "logit"))) %>%
  set_names("SV", "SH")


lapply(ybymiss.mod, parameters::model_parameters)
# Students missing on sports, friend vars, had lower SV; missing on gender associated with higher SV
# No differences on SH
# Perpetration did not vary by missingness on previous perp


## predicting missingness by values of x variables
# 1:28
# c(1:8, 10:12, 19:23, 25:28)
## Having trouble finding a decent model (some variables have ridiculous standard errors),
# Thus far the only significant variable is Low_Contact on SV missingness,
# but this is only for some of the models
missbyx.mod <- map(.x = c("SV_Perp_W4_di_NA", "SH_Perp_W4_di_NA"),
                       ~glm(formula(paste0(.x, " ~ ",
                                           paste(a1regvars[c(2:4, 7, 8, 10, 12, 19:23, 25:28)], collapse = " + "))),
                            data = withshadow, family = binomial(link = "logit"))) %>%
  set_names("SV", "SH")

lapply(missbyx.mod, parameters::model_parameters)


## Missingness on sports most closely associated with missing on friend variables
dsconscales %>%
  select(SH_Perp_W4_di, SV_Perp_W4_di, Contact_Cat, all_of(allregvars[-c(1:6)])) %>%
  gg_miss_fct(Contact_Cat)


## Do outcomes differ by missingness on sports variables?
perpbysportmiss <- withshadow %>%
  select(StudentID, SH_Perp_Prior, SV_Perp_Prior, SH_Perp_W4, SV_Perp_W4, Missing_Sports = No_sports_NA) %>%
  gather(Outcome, Value, -StudentID, -Missing_Sports) %>%
  ggplot(aes(x = Value,
             fill = Missing_Sports)) + 
  geom_density(alpha=0.5) +
  theme_bw(base_size = 18) +
  facet_wrap(~Outcome)
# Missing cases might have slightly higher perpetration, ybymiss.mod does not back that up though


## CONCLUSIONS:
# social network vars and demographics < 1% cases missing
# W4 SV, SH, Dismiss, AOD ~5-6% cases missing
# Prior SV/SH ~7-8% cases missing
# sports ~33% missing
# friend variables ~ 52% missing due to outdegree = 0
# Prior perpetration not predictive of W4 Perp, suggesting MAR rather than NMAR
# Concurrent perpetration associated missingness as is Dismissiveness on SH missingness
# Only concerning missingness is sports, so do a listwise model and an imputation model
# For MI, use other scales in dataset (see SoS analysis) as auxilliary variables

################################################


# ----- Multiple Imputation ----------------------

# von Hippel, 2009 - Create interactions and transformations, then impute
#  - van buuren disputes this as the best approach though and ultimately suggests passive imputation - https://stefvanbuuren.name/fimd/sec-knowledge.html
# details on pmm - https://stefvanbuuren.name/fimd/sec-pmm.html
# details on logreg - https://stefvanbuuren.name/fimd/sec-categorical.html
# conditioning on all possible variables and interactions is advised in many cases: https://stefvanbuuren.name/fimd/sec-modelform.html
# - though an upper limit of 30ish variables is also reasonable as the improved explained variance diminishes beyond that
# - also suggest including all variables in the analysis model including the outcome as well as variables related to non-response
# - mice removes predictors (and logs doing so in the mids object) if multicollinearity is an issue
# Cite Nguyen, Carlin, and Lee (2017) for MI model checking

# parallizing with parlmice does not seem to save much time for me


#### Analysis 1 datasets ####
## imputation method matched to variable type
# logreg for binary, polr for ordinal, pmm for continuous
a1imp.method <- data.frame(variable = c("SH_Perp_W4_di", "SV_Perp_W4_di", a1regvars[c(1:4, 7,8, 10:37)]),
           method = c(rep("logreg", 6), #"pmm", "logreg",
                      rep("pmm", 5),
                      rep("logreg", 8), "pmm", "pmm", rep("logreg", 9),
                      rep("pmm", 6)))

a1di.vars <- a1imp.method[a1imp.method$method == "logreg",]$variable

tictoc::tic()
a1imp <- dsconscales %>%
  select(all_of(a1imp.method$variable)) %>%
  mutate(across(.cols = all_of(a1di.vars), as_factor)) %>%
  mice(m = 50, method = a1imp.method$method, seed = 7071)
tictoc::toc()

a1imp.cnvg <- plot(a1imp)
a1imp.dens <- densityplot(a1imp)
# a1imp.strip <- stripplot(a1imp)
a1imp.box <- bwplot(a1imp)


#### Analysis 2 datasets ####
## imputation method matched to variable type
# logreg for binary, polr for ordinal, pmm for continuous
a2imp.method <- data.frame(variable = c("SH_Perp_W4_di", "SV_Perp_W4_di", allregvars[c(1:4,7,8,10:length(allregvars))]),
                           method = c(rep("logreg", 6), rep("pmm", 5),
                                      rep("logreg", 8), "pmm", "pmm", rep("logreg", 6),
                                      rep("pmm", length(a2regvars))))

a2di.vars <- a2imp.method[a2imp.method$method == "logreg",]$variable

tictoc::tic()
a2imp <- dsnoniso %>%
  select(all_of(a2imp.method$variable)) %>%
  mutate(across(.cols = all_of(a2di.vars), as_factor)) %>%
  mice(m = 50, method = a2imp.method$method, seed = 7071)
tictoc::toc()

a2imp.cnvg <- plot(a2imp)
a2imp.dens <- densityplot(a2imp)
# a2imp.strip <- stripplot(a2imp)
a2imp.box <- bwplot(a2imp)


##############################################################

save(predcorrs, scalecorrs, cfa.mod, iccs, icc.logs,
     a1missoverview, a2missoverview, withshadow, shadowvars,
     ybymiss.mod, missbyx.mod, perpbysportmiss,
     a1imp.cnvg, a1imp.dens, a1imp.box, #a1imp.strip,
     a2imp.cnvg, a2imp.dens, a2imp.box, #a2imp.strip,
     file = "Output/MI_and_Measure2.RData")

save(a1regvars, a2regvars, allregvars,
     a1imp.method, a2imp.method, a1imp, a2imp,
     file = "Output/Imputated2.RData")

# load("Output/MI_and_Measure.RData")

