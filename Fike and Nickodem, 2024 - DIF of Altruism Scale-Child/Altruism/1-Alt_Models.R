###############################
#                             #
#    Measuring Altruism       #
#       Model Checks          #
#                             #
###############################

# ------   Setup   -----------------------

## Load packages and custom functions
source("PnF.R")

## import data
aa.orig <- haven::read_sav("../MOST RECENT DATASET AA.sav")

## Defining scale items
alt.items <- paste0("CALT0", 1:9) # response options: never, once or twice, 3 to 5 times, 6 to 10 times, more than 10 times

# table(aa.orig$CCONS02) # all participants have assented
# aa.orig %>% count(CPERID6) %>% View() # each student in the dataset once

## Items to numeric and creating raw scores
aa.temp <- aa.orig %>%
  mutate(across(all_of(alt.items), ~as.numeric(.x) - 1)) %>% # shifting responses to 0 - 4 rather than 1 - 5
  mutate(Altruism = scale_score(., alt.items, type = "mean", min.valid = length(alt.items) - 1))


## Keeping variables for analysis and renaming for eventual output
aa.dat <- aa.temp %>%
  filter(!is.na(CSEX)) %>% # removing cases with missing gender
  mutate(Sex = as_factor(CSEX),
         Grade = as_factor(CGRD6),
         Age_Group = case_when(CAGE6 %in% 11:13 ~ "Early",
                               CAGE6 %in% 14:16 ~ "Middle",
                               CAGE6 %in% 17:19 ~ "Late") %>%
           factor(., levels = c("Early", "Middle", "Late")),
         Hispanic = ifelse(CHLSO == 1, 1, 0),
         Race_All = as_factor(CRACE),
         Race = as_factor(CRACA)) %>%
  mutate(across(CBMUS:CBFRAS, as_factor)) %>%    # convert other demographics to factors
  select(CPERID6, School = CSCH6, CDISTR, CYEAR,
         Grade, Age = CAGE6, Age_Group, Sex,     # demographics
         Hispanic, Race_All, Race, CBMUS:CBFRAS,                    
         all_of(alt.items), Altruism)    # altruism items


###################################################################

# -----    Descriptive Statistics and Demographics    ------------

####   Demographics   ####

prop.table(table(aa.dat$CDISTR)) # district

table(aa.dat$Grade)
(27 + 341 + 260) / nrow(aa.dat) # middle school
(380 + 150 + 86 + 114) / nrow(aa.dat) # high school

## Table 1
demographics <- aa.dat %>%
  mutate(Race = fct_drop(Race)) %>%
  tbl_summary(include = c("Grade", "Age", "Age_Group","Sex", "Race", "Hispanic", "CYEAR"),
              label = list(Age ~ "Age (Years)", Age_Group ~ "Age Group", Sex ~ "Biological Sex",
                           Race ~ "Race", CYEAR ~ "Project Year")) %>%
  as_flex_table()


#### Frequency distribution for scale items ####
item.freqs <- aa.dat %>%
  select(all_of(alt.items)) %>%
  get_item_freqs()

item.freqs.sex <- aa.dat %>%
  select(Sex, all_of(alt.items)) %>%
  get_item_freqs(Sex)

item.freqs.age <- aa.dat %>%
  select(Age_Group, all_of(alt.items)) %>%
  get_item_freqs(Age_Group)

####   Continuous Variable Descriptives ####
descrips <- skimr::skim(aa.dat, Age, Altruism)

## Scales by Sex
descrip.by.sex <- aa.dat %>%
  tbl_summary(include = c("Altruism"),
              by = "Sex",
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min} - {max}]")) %>%
  add_overall() %>%
  add_difference(test = list(all_continuous() ~ "t.test")) %>%
  as_flex_table()

## Scales by Age
descrip.by.age <- aa.dat %>%
  tbl_summary(include = c("Altruism"),
              by = "Age_Group",
              statistic = list(all_continuous() ~ "{mean} ({sd}) [{min} - {max}]")) %>%
  add_overall() %>%
  add_p(test = list(all_continuous() ~ "aov")) %>%
  as_flex_table()


#########################################################

# ------      Missingingness Patterns     ------------------

missing.summary <- aa.dat %>%
  naniar::miss_summary()

missing.summary$miss_df_prop # 2% of all cells missing
missing.summary$miss_case_table # 89% of cases missing 0 or 1 values
missing.summary$miss_var_summary[[1]] # CDF variables missing ~ 5%; CALT < 2%
missing.summary$miss_case_summary # some students missing > 40% of cells

## Indicator for missingness 
aa.shadow <- bind_cols(aa.dat,
                       naniar::as_shadow(select(aa.dat, Altruism))) %>%
  mutate(CDISTR = as_factor(CDISTR))

#### Predicting Missing ####
miss.preds <- aa.shadow %>%
  select(CDISTR, CYEAR, Age_Group, Sex) %>%
  names()

# actually predicts if they have a score
predict.miss <- map(.x = miss.preds,
                     ~glm(formula(paste("Altruism_NA ~ 1 +", paste(.x, collapse = " + "))), data = aa.shadow,
                          family = binomial(link = "logit"))) %>%
  set_names(miss.preds)
# lapply(predict.miss, summary)

ORs <- map_dfr(.x = predict.miss,
           ~parameters::model_parameters(.x, exponentiate = TRUE) %>%
             select(Parameter, Coefficient, CI_low, CI_high, p))

#########################################################

# -------- Dimensionality and Reliability  -------------

## check power
kfa::find_k(variables = select(aa.dat, all_of(alt.items)), m = 2)

## run kfa
alt.kfa <- kfa::kfa(data = aa.dat,
                    variables = alt.items,
                    k = 3, # number of folds
                    m = 2, # maximum number of factors to test
                    ordered = TRUE, # treat variables as ordered
                    missing = "listwise")

## generate report
kfa::kfa_report(alt.kfa,
                index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                file.name = "Altruism Psychometric Report",
                report.title = "Altruism Psychometric Report")

## run kfa - dropping items 7 and 8 - see mirt runs below
alt.kfa.no78 <- kfa::kfa(data = aa.dat,
                         variables = alt.items[-c(7,8)],
                         k = 3, # number of folds
                         m = 2, # maximum number of factors to test
                         ordered = TRUE, # treat variables as ordered
                         missing = "listwise")

## generate report
kfa::kfa_report(alt.kfa.no78,
                index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                file.name = "Altruism Psychometric Report_Drop78",
                report.title = "Altruism Psychometric Report - Dropped items 7 and 8")


#############################################################

# -----   IRT Fit  ---------------

# Note. The PV_Q1 statistic uses bootstrap sampling, so results may vary if a
# different set.seed is used.

############################
####      Altruism      ####

#### Graded response model  ####

## Full Sample

alt.f1 <- aa.dat %>%
  select(all_of(alt.items)) %>%
  mirt(model = 1, itemtype = "graded", SE = TRUE, method = "MHRM")

alt.f2 <- aa.dat %>%
  select(all_of(alt.items)) %>%
  mirt(model = 2, itemtype = "graded", SE = TRUE, method = "MHRM")

set.seed(3420)
alt.irt.results <- scaling_results(alt.f1, alt.items, alt.f2)

# global model fit
alt.irt.results$`Model Summary` # comparisons suggest 2f but only 8 clearly fits on second factor (3 and 8 have better h2)
alt.irt.results$`Global Fit` <- bind_rows(alt.irt.results$`Global Fit`,
                                          M2(alt.f2, na.rm = TRUE, type = "C2"),
                                          .id = "Factor")
# item fit
alt.irt.results$`Item Fit` # 1 and 7 significant

# item parameters
alt.irt.results$`Item Parameters`$mirt.wide

## Dropping item 7

alt.f1.no7 <- aa.dat %>%
  select(all_of(alt.items[-7])) %>%
  mirt(model = 1, itemtype = "graded", SE = TRUE, method = "MHRM")

alt.f2.no7 <- aa.dat %>%
  select(all_of(alt.items[-7])) %>%
  mirt(model = 2, itemtype = "graded", SE = TRUE, method = "MHRM")

set.seed(3420)
alt.irt.results.no7 <- scaling_results(alt.f1.no7, alt.items[-7], alt.f2.no7)

# global model fit
alt.irt.results.no7$`Model Summary` # comparisons suggest 2f but only 8 clearly fits on second factor (3 and 8 have better h2)
alt.irt.results.no7$`Global Fit` <- bind_rows(alt.irt.results.no7$`Global Fit`,
                                              M2(alt.f2.no7, na.rm = TRUE, type = "C2"),
                                              .id = "Factor")
# item fit
alt.irt.results.no7$`Item Fit` # 1 and 8 significant

# item parameters
alt.irt.results.no7$`Item Parameters`$mirt.wide

## Dropping item 7&8
alt.f1.no78 <- aa.dat %>%
  select(all_of(alt.items[-c(7,8)])) %>%
  mirt(model = 1, itemtype = "graded", SE = TRUE, method = "MHRM")

alt.f2.no78 <- aa.dat %>%
  select(all_of(alt.items[-c(7,8)])) %>%
  mirt(model = 2, itemtype = "graded", SE = TRUE, method = "MHRM")

set.seed(3420)
alt.irt.results.no78 <- scaling_results(alt.f1.no78, alt.items[-c(7,8)], alt.f2.no78)

# overall model fit
alt.irt.results.no78$`Model Summary` # comparisons suggest 2f with 1-3 on f1 and 4-6,9 on f2; h2 improves some for 6
alt.irt.results.no78$`Global Fit` <- bind_rows(alt.irt.results.no78$`Global Fit`,
                                               M2(alt.f2.no78, na.rm = TRUE, type = "C2"),
                                               .id = "Factor")

# item fit
alt.irt.results.no78$`Item Fit`

# item parameters
alt.irt.results.no78$`Item Parameters`$mirt.wide

############################################################

#########################################################################

save(alt.items, aa.dat,
     descrips, demographics, descrip.by.age, descrip.by.sex,
     item.freqs, item.freqs.age, item.freqs.sex,
     missing.summary, miss.preds, predict.miss, ORs,
     alt.kfa, alt.kfa.no78,
     alt.irt.results, alt.irt.results.no7, alt.irt.results.no78,
     file = "Altruism/Altruism_Model_Results.RData")
