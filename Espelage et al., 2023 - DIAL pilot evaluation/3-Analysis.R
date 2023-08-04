#########################################################
#                                                       #
#  IES Goal 2: Disability Anti-Bullying (DIAL) Training #      
#             Treatment Effect Analysis                 #
#                                                       #
#########################################################

source("Scripts/IG2_PnF.R")
library(lme4)
library(parameters)
library(lubridate)

#### Importing information ####
## data
dial.all <- haven::read_sav("00 Data/COMBINED T1_T2/OLD/00 - IES_ALL_DATA_SCORES_W1_W2_11-21-22.sav") %>%
  mutate(School = as_factor(SCHOOL_ID))

# ## How close were teacher reports to student reports
# # Race
# table(as_factor(dial.all[!is.na(dial.all$ST_Race) & !is.na(dial.all$TR_Race),]$ST_Race),
#       as_factor(dial.all[!is.na(dial.all$ST_Race) & !is.na(dial.all$TR_Race),]$TR_Race), useNA = "always")
# # Gender
# table(as_factor(dial.all[!is.na(dial.all$ST_Gender) & !is.na(dial.all$TR_Gender),]$ST_Gender),
#       as_factor(dial.all[!is.na(dial.all$ST_Gender) & !is.na(dial.all$TR_Gender),]$TR_Gender), useNA = "always")

## Importing shortcuts for item-scale correspondence
load("Psychometric Reports/Psychometrics_Checks_T1T2.RData")
rm(dial.rn, dial.tslf, dial.sslf, dial.tst, sslf.freqs, tslf.freqs, tst.freqs)

## Names of scales to be examined
tr.names <- names(tst.items)[-6]
t.names <- names(t.items)[c(1:4, 6:9, 11:15, 17)] # shortcut used throughout script
st.names <- names(st.items)

## module completion to survey completion
modcomp <- readxl::read_excel("00 Data/DIAL Module Completion - 2-1-2023.xlsx")

## joining module completion data
dial.all <- dial.all %>%
  left_join(modcomp %>%
              select(TEACHER_ID, LAST_LOGIN, COMPLETED_ALL_FOUR_MODULES, COUNT_MODULES_COMPLETED),
            by = "TEACHER_ID") %>%
  mutate(module_complete_date = sub(" .*", "", LAST_LOGIN) %>% mdy(),
         survey_complete_date = sub(" .*", "", T_DATE_W2) %>% ymd(),
         complete_interval = interval(module_complete_date, survey_complete_date) %/% days(1) %>%
           if_else(DIAL == 0, 0, .) %>%  # all control teachers get a 0
           if_else(. == -1, 0, .)) # one person apparently finished the survey b/f the module; constraining to 0

# ---- Finalizing Datasets and Demographics --------

## Gathering unique teacher data since teacher rows were repeated in combined dataset
dial.t <- dial.all %>%
  select(TEACHER_ID, DIAL, CONDITION, SCHOOL_ID, School,
         T_WAVES_PRESENT, starts_with("T_"), LAST_LOGIN:complete_interval) %>%  # keeping only teacher ID and self-report items
  unique() %>% # dropping duplicates here
  filter(!is.na(T_DATE_W1) | !is.na(T_DATE_W2)) %>% # dropping empty rows (some teachers did not complete self-report either wave)
  # filter(!is.na(Grade)) %>%
  mutate(Race = as_factor(T_Race) %>%
           fct_recode(MultiOther = "Asian", MultiOther = "Multiple", MultiOther = "Other") %>% # collapsing small groups
           fct_relevel('MultiOther', after = Inf),
         Gender = as_factor(T_Gender) %>% fct_recode(., NULL = "Other") %>% fct_drop())

# ## 33 of 36 teachers matched (KLT001_KLT003; KLT006; and KLT011 did not)
# check <- modcomp %>% anti_join(dial.t, by = "TEACHER_ID")
# ## all 32 control teachers did not match
# curious <- dial.t %>%
#   anti_join(modcomp %>%
#               select(TEACHER_ID, LAST_LOGIN, COMPLETED_ALL_FOUR_MODULES, COUNT_MODULES_COMPLETED),
#             by = "TEACHER_ID")
# 
# table(dial.t$COMPLETED_ALL_FOUR_MODULES)
# table(dial.t$COUNT_MODULES_COMPLETED)
# summary(dial.t$complete_interval)

# determine grade range for teachers
grade.t <- dial.all %>%
  select(TEACHER_ID, CONDITION, Grade) %>%
  unique() %>%
  group_by(TEACHER_ID, CONDITION, Grade) %>%
  summarize(one = n()) %>%
  mutate(n = sum(one)) %>%
  filter(!(is.na(Grade) & n > 1)) %>%
  mutate(new_n = n(),
         Grade = as_factor(Grade))

# table(grade.t$Grade, grade.t$CONDITION, useNA = "always")/28 # one control teacher listed for 4th and 5th. Using 4th arbitrary
# prop.table(table(grade.t$Grade, grade.t$CONDITION), margin = 2) # one control teacher listed for 4th and 5th. Using 4th arbitrary
# table(grade.t$Grade)/60

## Student data
dial.s <- dial.all %>%
  filter(!is.na(STUDENT_ID)) %>% # 5 teachers had self-reports without a student
  # select(-starts_with("T_")) %>% # need for mediation
  mutate(S_Race = case_when(ST_Race == 3 ~ as_factor(TR_Race),     # using Teacher code for students' "Dont know" responses
                            !is.na(ST_Race) ~ as_factor(ST_Race),  # use Student code when available
                            !is.na(TR_Race) ~ as_factor(TR_Race)) %>% # otherwise use Teacher code 
           fct_recode(MultiOther = "Asian", MultiOther = "Multiple", MultiOther = "Other") %>% # collapsing small groups
           fct_relevel('MultiOther', after = Inf),
         S_Gender = case_when(!is.na(ST_Gender) ~ as_factor(ST_Gender),     # use Student code when available
                              !is.na(TR_Gender) ~ fct_recode(as_factor(TR_Gender), Boy = "Male", Girl = "Female")), # otherwise use Teacher code
         ELL = case_when(!is.na(TR_ESOL_W2) ~ as_factor(TR_ESOL_W2),
                         !is.na(ST_ENGLISH_LEARNER_W2) ~ as_factor(ST_ENGLISH_LEARNER_W2)) %>%
           fct_drop())

# table(as_factor(dial.s$TR_ESOL_W2), as_factor(dial.s$ST_ENGLISH_LEARNER_W2), useNA = "always")
  
# table(dial.s$S_Race, useNA = "always")
# table(dial.s$S_Gender, useNA = "always")

## teacher report of students
dial.tr <- dial.s %>%
  filter(TR_WAVES_PRESENT !=0) # drops 28 cases that were student self-report only

############################################################


# ----  Is interval between module completion and survey completion associated with outcomes ------
## teachers - No
corrs.t <- dial.t %>%
  select(complete_interval, ends_with("_SCORE_W2")) %>%
  rename_with(.fn = ~str_remove(., "T_") %>% str_remove("_SCORE_W2") %>%
                str_replace("_", " ") %>% str_to_title()) %>%
  psych::corr.test(use = "pairwise")

round(corrs.t$r[1,], 3) # correlation
round(corrs.t$p[1,], 3) # p-value with holm adjustment for multiple comparisons (use [,1] for unadjusted)
corrplot::corrplot.mixed(corrs.t$r, tl.pos = "lt")

## student self-reports - No
corrs.s <- dial.s %>%
  select(-starts_with("TR"), -starts_with("T_")) %>%
  select(complete_interval, ends_with("_SCORE_W2")) %>%
  rename_with(.fn = ~str_remove(., "ST_") %>% str_remove("_SCORE_W2") %>%
                str_replace("_", " ") %>% str_to_title()) %>%
  psych::corr.test(use = "pairwise")

round(corrs.s$r[1,], 3) # correlation
round(corrs.s$p[1,], 3) # p-value with holm adjustment for multiple comparisons (use [,1] for unadjusted)
corrplot::corrplot.mixed(corrs.s$r, tl.pos = "lt")

## teacher report of students - No
corrs.tr <- dial.tr %>%
  select(-starts_with("ST_"), -starts_with("T_")) %>%
  select(complete_interval, ends_with("_SCORE_W2")) %>%
  rename_with(.fn = ~str_remove(., "TR_") %>% str_remove("_SCORE_W2") %>%
                str_replace("_", " ") %>% str_to_title()) %>%
  psych::corr.test(use = "pairwise")

round(corrs.tr$r[1,], 3) # correlation
round(corrs.tr$p[1,], 3) # p-value with holm adjustment for multiple comparisons (use [,1] for unadjusted)
corrplot::corrplot.mixed(corrs.tr$r, tl.pos = "lt")

###########################################################

#################################
####  Missing Data Analysis #####


# -----   Attrition  -------------

## quick look
# table(as_factor(dial.s$TR_WAVES_PRESENT), useNA = "always")
# table(as_factor(dial.s$ST_WAVES_PRESENT), useNA = "always")
# table(as_factor(dial.t$T_WAVES_PRESENT), useNA = "always")


## prepping variables for tabular output
# student variables
att.prep.s <- dial.s %>%
  mutate(Condition = as_factor(CONDITION),
         TR_present = as_factor(TR_WAVES_PRESENT) %>%
           fct_recode(NULL = "No teacher report"),
         ST_present = as_factor(ST_WAVES_PRESENT) %>%
           fct_recode(NULL = "No student report"),
         TR_Dropout = case_when(TR_WAVES_PRESENT == 1 ~ 1,
                                TR_WAVES_PRESENT == 3 ~ 0,
                                TRUE ~ NA_real_),
         ST_Dropout = case_when(ST_WAVES_PRESENT == 1 ~ 1,
                                ST_WAVES_PRESENT == 3 ~ 0,
                                TRUE ~ NA_real_),
         S_Race = fct_relevel(S_Race, "Hispanic")) %>% # Moving Hispanic first to be the reference group
  naniar::bind_shadow(only_miss = TRUE)

# teacher
att.prep.t <- dial.t %>%
  mutate(Condition = as_factor(CONDITION),
         T_present = as_factor(T_WAVES_PRESENT) %>%
           fct_recode(NULL = "No teacher report"),
         T_Dropout = case_when(T_WAVES_PRESENT == 1 ~ 1,
                               T_WAVES_PRESENT == 3 ~ 0,
                               TRUE ~ NA_real_),
         T_Race = as_factor(T_Race) %>% fct_relevel(., "Hispanic")) %>% # Moving Hispanic first to be the reference group)
  naniar::bind_shadow(only_miss = TRUE)

## Creating table for attrition by condition
attrition <- map2(.x = list(att.prep.s, att.prep.s, att.prep.t),
                  .y = c("ST_present", "TR_present", "T_present"),
                  ~.x %>% filter(!is.na(!!sym(.y))) %>%
                    select(Condition, !!sym(.y)) %>%
                    tbl_summary(by = "Condition",
                                label = NULL) %>%
                    add_overall() %>%
                    # add_p(test = everything() ~ "fisher.test") %>% # better with small sample sizes; calculate below
                    as_flex_table() %>%
                    font(fontname = "Times New Roman", part = "all") %>%
                    padding(padding = 0, part = "all") %>%
                    autofit()) %>%
  set_names(c("Student self-report", "Teacher report of students", "Teacher self-reports"))

## fisher's test gives exact p-values
fishers <- map2(.x = list(att.prep.s, att.prep.s, att.prep.t),
                .y = c("ST_present", "TR_present", "T_present"),
               ~fisher.test(.x[[.y]], .x[["Condition"]])) %>%
  set_names(c("Student self-report", "Teacher report of students", "Teacher self-reports"))
  

#### Predicting W2 Attrition with W1 variables ####

## Teacher report of students
# table(att.prep.s$TR_Dropout, useNA = "always")

# variables to include as predictors
w1.vars.tr <- att.prep.s %>%
  select(DIAL, matches("^TR_.*SCORE_W1$"), -TR_ACADEMIC_COMP_SCORE_W1) %>% # AcComp had questionable psychometrics
  names()

# running model
drop.tr <- glm(as.formula(paste0("TR_Dropout ~ ", paste(w1.vars.tr, collapse = " + "), " + S_Race + S_Gender")),
               data = att.prep.s, family = binomial(link = "logit"))

# gathering results into a table
# not sure why the warning appears here, but not when running the model
drop.tr.params <- model_parameters(drop.tr, exponentiate = TRUE) %>% # Haitians less likely to dropout than Hispanic; Higher Social Anxiety and Control condition more likely to dropout
  filter(Parameter != "S_GenderOther") %>%
  mutate(Parameter = str_replace(Parameter, "S_Race", "Student Race - ") %>%
           str_replace(., "S_Gender", "Student Gender - ") %>%
           str_remove(., "TR_") %>% str_replace_all(., "_", " ") %>%
           str_to_title()) %>%
  select(Parameter, OR = Coefficient, CI_low, CI_high, z, p) %>%
  flextab_format()


## Student self-reports
# table(att.prep.s$ST_Dropout, useNA = "always")

# variables to include as predictors
w1.vars.st <- att.prep.s %>%
  select(DIAL, matches("^ST_.*SCORE_W1$"), ) %>%
  names() # ST_STAFF_INTERVENE_SCORE_W1 causes predicted probs of 0 or 1

# running model
drop.st <- glm(as.formula(paste0("ST_Dropout ~ ", paste(w1.vars.st[c(1:7,9)], collapse = " + "), " + S_Race + S_Gender")),
               data = att.prep.s, family = binomial(link = "logit"))

# gathering results into a table
# not sure why the warning appears here, but not when running the model
drop.st.params <- model_parameters(drop.st, exponentiate = TRUE) %>% # Black students more likely to dropout than Hispanic students
  filter(Parameter != "S_GenderOther") %>%
  mutate(Parameter = str_replace(Parameter, "S_Race", "Student Race - ") %>%
           str_replace(., "S_Gender", "Student Gender - ") %>%
           str_remove(., "ST_") %>% str_replace_all(., "_", " ") %>%
           str_to_title()) %>%
  select(Parameter, OR = Coefficient, CI_low, CI_high, z, p) %>%
  flextab_format()


## Teacher self-reports
# table(att.prep.t$T_Dropout, useNA = "always") # only 8

# variables to include as predictors
w1.vars.t <- att.prep.t %>%
  select(DIAL, matches("^T_.*SCORE_W1$"), -T_TEACHER_EFFICACY_SCORE_W1) %>% # linear combination of subscores
  names() # T_NORMATIVE_ATTITUDE_SCORE_W1 & T_STAFF_INTERVENE_SCORE_W1 cause predicted probs of 0 or 1

# running model
drop.t <- glm(as.formula(paste0("T_Dropout ~ ", paste(w1.vars.t[c(1:7, 9:11)], collapse = " + "), " + T_Race + T_Gender")),
               data = att.prep.t, family = binomial(link = "logit"))

# gathering results into a table
drop.t.params <- model_parameters(drop.t, exponentiate = TRUE) %>%
  mutate(Parameter = str_replace(Parameter, "T_Race", "Teacher Race - ") %>%
           str_replace(., "T_Gender", "Teacher Gender - ") %>%
           str_remove(., "T_") %>% str_replace_all(., "_", " ") %>%
           str_to_title()) %>%
  select(Parameter, OR = Coefficient, CI_low, CI_high, z, p) %>%
  flextab_format()


#### Non-response ####
# by wave after removing dropouts

## Teacher self-reports
t.miss.w1 <- dial.t %>%
  filter(T_WAVES_PRESENT %in% c(1,3)) %>%
  select(TEACHER_ID:T_Gender, ends_with("W1")) %>%
  naniar::miss_summary()
# t.miss.w1$miss_var_summary[[1]] %>% View() # most missing (6.4%) on Prevention_PD items

t.miss.w2 <- dial.t %>%
  filter(T_WAVES_PRESENT %in% c(2,3)) %>%
  select(TEACHER_ID:T_Gender, ends_with("W2"),
         -contains("_COACH"), -T_MET_COACH_W2, -starts_with("T_DIAL_TRAINING_QUESTIONAIRE")) %>% # excluding DIAL condition only items
  naniar::miss_summary()
# t.miss.w2$miss_var_summary[[1]] %>% View() # missing on Coaching and DIAL items given the skip logic, otherwise Transgender and positive interaction items 


# predicting missingness
nonresp.t <- map(.x = t.names,
                 ~glm(as.formula(paste0("T_", toupper(.x), "_SCORE_W2_NA ~ ", paste(w1.vars.t[c(1:7, 9:11)], collapse = " + "), " + T_Race + T_Gender")),
              data = filter(att.prep.t, T_WAVES_PRESENT %in% c(1,3)), family = binomial(link = "logit"))) %>%
  set_names(t.names)

nonresp.t.params <- map_dfr(nonresp.t, model_parameters, .id = "Outcome")
# any(nonresp.t.params$p < .05)


## Student self-reports
# Wave 1
st.miss.w1 <- dial.s %>%
  filter(ST_WAVES_PRESENT %in% c(1,3)) %>%
  select(STUDENT_ID, ST_Race, ST_Gender, matches("^ST_.*_W1$")) %>%
  naniar::miss_summary()
# st.miss.w1$miss_var_summary[[1]] %>% View() # almost entirely disability and Ethnicity

# Wave 2
st.miss.w2 <- dial.s %>%
  filter(ST_WAVES_PRESENT %in% c(2,3)) %>%
  select(STUDENT_ID, ST_Race, ST_Gender, matches("^ST_.*_W2$")) %>%
  naniar::miss_summary()
# st.miss.w2$miss_var_summary[[1]] %>% View() # almost entirely disability and EL status

# predicting missingness
nonresp.s <- map(.x = st.names,
                 ~glm(as.formula(paste0("ST_", toupper(.x), "_SCORE_W2_NA ~ ", paste(w1.vars.st[c(1:7,9)], collapse = " + "), " + S_Race + S_Gender")),
                      data = filter(att.prep.s, ST_WAVES_PRESENT %in% c(1,3)), family = binomial(link = "logit"))) %>%
  set_names(st.names)

nonresp.s.params <- map_dfr(nonresp.s, model_parameters, .id = "Outcome")
# any(nonresp.s.params$p < .05)

## Teacher report of Students
# Wave 1
tr.miss.w1 <- dial.s %>%
  filter(TR_WAVES_PRESENT %in% c(1,3)) %>%
  select(STUDENT_ID, TR_Race, TR_Gender, matches("^TR_.*_W1$")) %>%
  naniar::miss_summary()
# tr.miss.w1$miss_var_summary[[1]] %>% View() # behavior risk score (5.1%) and race (4.2%)

# Wave 2
tr.miss.w2 <- dial.s %>%
  filter(TR_WAVES_PRESENT %in% c(2,3)) %>%
  select(STUDENT_ID, TR_Race, TR_Gender, matches("^TR_.*_W2$")) %>%
  naniar::miss_summary()
# tr.miss.w2$miss_var_summary[[1]] %>% View() # behavior risk score (3.9%) and race (3.2%)


save(att.prep.s, att.prep.t, attrition, fishers,
     w1.vars.tr, drop.tr, drop.tr.params,
     w1.vars.t, drop.t, drop.t.params,
     w1.vars.st, drop.st, drop.st.params,
     t.miss.w1, t.miss.w2, nonresp.t, nonresp.t.params,
     st.miss.w1, st.miss.w2, nonresp.s, nonresp.s.params,
     tr.miss.w1, tr.miss.w2,
     file = "Output/Missing_Data_Results.RData")


##########################################


############################
####    Demographics    ####

#### Teachers ####
table(as_factor(dial.t$T_EDUCATION_LEVEL_W1), as_factor(dial.t$T_EDUCATION_LEVEL_W2), useNA = "always")
table(as_factor(dial.t$T_TRANSGENDER_W1), as_factor(dial.t$T_TRANSGENDER_W2), useNA = "always")

t.demos <- dial.t %>%
  mutate(across(.cols = c(CONDITION, T_EDUCATION_LEVEL_W1), ~as_factor(.x) %>% fct_drop())) %>%
  select(CONDITION, Race, Gender, T_EDUCATION_LEVEL_W1) %>%
  tbl_summary(by = "CONDITION",
              label = list(Race = "Race", T_EDUCATION_LEVEL_W1 = "Education Level at W1")) %>%
  add_overall() %>%
  as_flex_table() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding = 0, part = "all")

#### Students ####
st.demos <- dial.s %>%
  filter(ST_WAVES_PRESENT != 0) %>%
  mutate(across(.cols = c(CONDITION, EXCEPTIONALITY_TYPE, Grade), ~as_factor(.x) %>% fct_drop())) %>%
  select(CONDITION, Grade, S_Race, S_Gender, ELL, EXCEPTIONALITY_TYPE) %>%
  tbl_summary(by = "CONDITION",
              label = list(S_Race = "Race", S_Gender = "Gender", ELL = "English Language Learner",
                           EXCEPTIONALITY_TYPE = "Exceptionality Type")) %>%
  add_overall() %>%
  as_flex_table() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding = 0, part = "all")

# dial.s %>%
#   filter(TR_WAVES_PRESENT == 0 & ST_WAVES_PRESENT == 0) %>% View()

tr.demos <- dial.tr %>%
  mutate(across(.cols = c(CONDITION, EXCEPTIONALITY_TYPE, Grade), ~as_factor(.x) %>% fct_drop())) %>%
  select(CONDITION, Grade, S_Race, S_Gender, ELL, EXCEPTIONALITY_TYPE) %>%
  tbl_summary(by = "CONDITION",
              label = list(S_Race = "Race", S_Gender = "Gender", ELL = "English Language Learner",
                           EXCEPTIONALITY_TYPE = "Exceptionality Type")) %>%
  add_overall() %>%
  as_flex_table() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding = 0, part = "all")
  

##########################################


############################################
####       Intraclass Correlations      ####

# ## all outcomes, though only student outcomes will be used from it
# dial.score.long <- select(dial.s, contains("_ID"), contains("SCORE")) %>%
#   LongWave(1, contains("_ID")) %>%
#   bind_rows(LongWave(select(dial.s, contains("_ID"), contains("SCORE")), 2, contains("_ID"))) %>%
#   select(Wave, everything())
# 
# ## teacher outcomes
# dial.tscore.long <- dial.score.long %>%
#   select(Wave, TEACHER_ID, SCHOOL_ID, starts_with("T_")) %>%
#   unique()

## Note: Only looking at variation in Wave 2 given how we are running the outcome models

## teacher report of students ICCs - When school is included 3 models are singular
tr.unc <- map(tr.names, ~lmer(formula(paste0("TR_", toupper(.x), "_SCORE_W2 ~ 1 + (1|TEACHER_ID)")), data = dial.s)) %>%
  set_names(tr.names)
tr.icc <- map_dfr(tr.unc, ~performance::icc(.x, by_group = TRUE), .id = "Measure")


## teacher ICCs
t.unc <- map(t.names, ~lmer(formula(paste0("T_", toupper(.x), "_SCORE_W2 ~ 1 + (1|School)")), data = dial.t)) %>%
  set_names(t.names)
lapply(t.unc, isSingular)
t.icc <- map_dfr(t.unc[-c(6:8, 14)], ~performance::icc(.x, by_group = TRUE), .id = "Measure")

## student ICCs
st.unc <- map(st.names, ~lmer(formula(paste0("ST_", toupper(.x), "_SCORE_W2 ~ 1 + (1|TEACHER_ID)")), data = dial.s)) %>%
  set_names(st.names)
st.icc <- map_dfr(st.unc, ~performance::icc(.x, by_group = TRUE), .id = "Measure")

## What to do about the school level? There are too few schools (6) for a random effect. Is there even variation at the school level?
# Note: when School was added as the 2nd (teacher outcomes) or 3rd (student outcomes) level, many models had singular fit (3 tr; 4 t; 3 st)

# teacher report of student ICCs - 3 models were singular
tr.sch <- map(tr.names, ~lmer(formula(paste0("TR_", toupper(.x), "_SCORE_W2 ~ 1 + (1|School/TEACHER_ID)")), data = dial.s)) %>%
  set_names(tr.names)
lapply(tr.sch, isSingular)
tr.sch.icc <- map_dfr(tr.sch[c(4:5)], ~performance::icc(.x, by_group = TRUE), .id = "Measure")

# student ICCs
st.sch <- map(st.names, ~lmer(formula(paste0("ST_", toupper(.x), "_SCORE_W2 ~ 1 + (1|School/TEACHER_ID)")), data = dial.s)) %>%
  set_names(st.names)
lapply(st.sch, isSingular)
st.sch.icc <- map_dfr(st.sch[-c(1, 5,6)], ~performance::icc(.x, by_group = TRUE), .id = "Measure")

## Summary
map(list(tr.sch.icc, t.icc, st.sch.icc), ~filter(.x, Group == "School"))
#  - Varies by outcome
#    - models were singular for some outcomes (i.e., no variation)
#  - For TR, 4.1% and 6.3% of variation in conflict and bullying, respectively
#  - For ST, ranges from 1.8% (Peer vict) to 15.1% (Fight)
#  - For T, ranges from .9% (students intervene) to 18.1% (classroom management)


########################################################################



####################################################
####       Standardized Mean Differences        ####

# Notes: - using WBdif::hedges2007 which accounts for clustering and missing data (via na.rm = T)
#        - Assuming unequal variances and using control sd in denominator
#        - valency interpretation is Intervention - Control
#        - Looking separately by wave
# As a check that doesn't account for clustering use:
# effectsize::glass_delta(x = TR_BEHAVIOR_RISK_SCORE_W1 ~ DIAL, data = dial.s,
#                         mu = 0, ci = 0.95, alternative = "two.sided", verbose = TRUE)
#        - valency interpretation is Control - Intervention
#        - Meaning it is probs using the Intervention SD to standardize

# -------     teacher report of students outcomes     ----------
tr.es <- map2_dfr(.x = rep(tr.names, each = 2), .y = rep(c("1", "2"), times = length(tr.names)),
                     ~cbind(data.frame(Scale = .x, Wave = .y),
                            as.data.frame(t(WBdif::hedges2007(dial.s[[paste0("TR_", toupper(.x), "_SCORE_W", .y)]],
                                                              tx.group.id = as_factor(dial.s$DIAL),
                                                              std.group = 0,
                                                              cluster.id = as_factor(dial.s$TEACHER_ID)))))) %>%
  mutate(Scale = str_replace_all(Scale, "_", " "))

# psych::describeBy(dial.s$TR_BEHAVIOR_RISK_SCORE_W1, group = as_factor(dial.s$DIAL))

## calculating means
tr.means <- dial.s %>%
  select(Condition = CONDITION, all_of(paste0("TR_", toupper(tr.names), "_SCORE_W1")),
         all_of(paste0("TR_", toupper(tr.names), "_SCORE_W2"))) %>%
  mutate(Condition = as_factor(Condition)) %>%
  gather(Scale, Score, -Condition) %>%
  separate(col = Scale, into = c("Scale", "Wave"), sep = "_W") %>%
  group_by(Condition, Scale, Wave) %>%
  summarize(Mean = mean(Score, na.rm = TRUE),
            SD = sd(Score, na.rm = TRUE)) %>%
  mutate(Scale = str_remove(Scale, "TR_") %>% str_remove("_SCORE") %>%
           str_replace_all("_", " ") %>% str_to_title())

## gathering score distributions
tr.distdata <- dial.s %>%
  select(STUDENT_ID, Condition = CONDITION, all_of(paste0("TR_", toupper(tr.names), "_SCORE_W1")),
         all_of(paste0("TR_", toupper(tr.names), "_SCORE_W2"))) %>%
  mutate(Condition = as_factor(Condition)) %>%
  gather(Scale, Score, -c(STUDENT_ID, Condition)) %>%
  filter(!is.na(Score) & !is.na(Condition)) %>%
  separate(col = Scale, into = c("Scale", "Wave"), sep = "_W") %>%
  mutate(Scale = str_remove(Scale, "TR_") %>% str_remove("_SCORE") %>%
           str_replace_all("_", " ") %>% str_to_title())

## creating distribution plots
tr.distplots <- map(.x = str_replace(tr.names, "_", " "),
                    ~dist_plot(scores = tr.distdata, means = tr.means, es = tr.es, gloc = 1, ncol = 1, scale = .x)) %>%
  set_names(tr.names)


# -------     teacher self-report outcomes     ----------

## clustering by school produces error (can't calculate ICC for certain outcomes); cluster.id = dial.t$School
t.es <- map2_dfr(.x = rep(t.names, each = 2), .y = rep(c("1", "2"), times = length(t.names)),
                ~cbind(data.frame(Scale = .x, Wave = .y), as.data.frame(t(WBdif::hedges2007(dial.t[[paste0("T_", toupper(.x), "_SCORE_W", .y)]],
                                                                                            tx.group.id = as_factor(dial.t$DIAL),
                                                                                            std.group = 0))))) %>%
  mutate(Scale = str_replace_all(Scale, "_", " "))

## calculating means
t.means <- dial.t %>%
  select(Condition = CONDITION, all_of(paste0("T_", toupper(t.names), "_SCORE_W1")),
         all_of(paste0("T_", toupper(t.names), "_SCORE_W2"))) %>%
  mutate(Condition = as_factor(Condition)) %>%
  gather(Scale, Score, -Condition) %>%
  separate(col = Scale, into = c("Scale", "Wave"), sep = "_W") %>%
  group_by(Condition, Scale, Wave) %>%
  summarize(Mean = mean(Score, na.rm = TRUE),
            SD = sd(Score, na.rm = TRUE)) %>%
  mutate(Scale = str_remove(Scale, "T_") %>% str_remove("_SCORE") %>%
           str_replace_all("_", " ") %>% str_to_title() %>%
           str_replace("Pd", "PD"))

## gathering score distributions
t.distdata <- dial.t %>%
  select(TEACHER_ID, Condition = CONDITION, all_of(paste0("T_", toupper(t.names), "_SCORE_W1")),
         all_of(paste0("T_", toupper(t.names), "_SCORE_W2"))) %>%
  mutate(Condition = as_factor(Condition)) %>%
  gather(Scale, Score, -c(TEACHER_ID, Condition)) %>%
  filter(!is.na(Score) & !is.na(Condition)) %>%
  separate(col = Scale, into = c("Scale", "Wave"), sep = "_W") %>%
  mutate(Scale = str_remove(Scale, "T_") %>% str_remove("_SCORE") %>%
           str_replace_all("_", " ") %>% str_to_title() %>%
           str_replace("Pd", "PD"))

## creating distribution plots
t.distplots <- map(.x = str_replace(t.names, "_", " "),
                    ~dist_plot(scores = t.distdata, means = t.means, es = t.es, gloc = 1, ncol = 1, scale = .x)) %>%
  set_names(t.names)

# -------     student self-report outcomes    ----------

st.es <- map2_dfr(.x = rep(st.names, each = 2), .y = rep(c("1", "2"), times = length(st.names)) ,
                  ~cbind(data.frame(Scale = .x, Wave = .y),
                         as.data.frame(t(WBdif::hedges2007(dial.s[[paste0("ST_", toupper(.x), "_SCORE_W", .y)]],
                                                           tx.group.id = as_factor(dial.s$DIAL),
                                                           std.group = 0,
                                                           cluster.id = as_factor(dial.s$TEACHER_ID)))))) %>%
  mutate(Scale = str_replace_all(Scale, "_", " "))

## calculating means
st.means <- dial.s %>%
  select(Condition = CONDITION, all_of(paste0("ST_", toupper(st.names), "_SCORE_W1")),
         all_of(paste0("ST_", toupper(st.names), "_SCORE_W2"))) %>%
  mutate(Condition = as_factor(Condition)) %>%
  gather(Scale, Score, -Condition) %>%
  separate(col = Scale, into = c("Scale", "Wave"), sep = "_W") %>%
  group_by(Condition, Scale, Wave) %>%
  summarize(Mean = mean(Score, na.rm = TRUE),
            SD = sd(Score, na.rm = TRUE)) %>%
  mutate(Scale = str_remove(Scale, "ST_") %>% str_remove("_SCORE") %>%
           str_replace_all("_", " ") %>% str_to_title())

## gathering score distributions
st.distdata <- dial.s %>%
  select(STUDENT_ID, Condition = CONDITION, all_of(paste0("ST_", toupper(st.names), "_SCORE_W1")),
         all_of(paste0("ST_", toupper(st.names), "_SCORE_W2"))) %>%
  mutate(Condition = as_factor(Condition)) %>%
  gather(Scale, Score, -c(STUDENT_ID, Condition)) %>%
  filter(!is.na(Score) & !is.na(Condition)) %>%
  separate(col = Scale, into = c("Scale", "Wave"), sep = "_W") %>%
  mutate(Scale = str_remove(Scale, "ST_") %>% str_remove("_SCORE") %>%
           str_replace_all("_", " ") %>% str_to_title())

## creating distribution plots
st.distplots <- map(.x = str_replace(st.names, "_", " "),
                    ~dist_plot(scores = st.distdata, means = st.means, es = st.es, gloc = 2.5, ncol = 1, scale = .x)) %>%
  set_names(st.names)

#################################################################



#########################################
####       Intervention Effect       ####

# NOTE: Can't use fixed effects for school b/c DIAL is a school-level indicator;
#       Thus, including 5 dummy codes will result in the matrix being rank deficient by 1
#       Could include school as a random effect, which is what we do for the ICC,
#       Though it is known that small level 3 numbers can cause problems. McNeish & Wentzel (2017)
#       show that a KR correction can be suitable in this situation though, so we'll attempt that
#       Except doing so, often leads to models being singular
#       The models below exclude school level
# 

# ------   Preliminary Analysis  -----------------

####   Teacher reports of students   ####

## model parameter names for output
tr.param.names <- map(.x = str_replace_all(tr.names, "_", " "),
                      ~c("Intercept", paste0("W1 ", .x), "DIAL",
                         "Intercept SD", "Residual SD", "n students", "n teachers", "R2"))

## running models
tr.mods <- map(.x = tr.names,
    ~lmer(as.formula(paste0("TR_", toupper(.x), "_SCORE_W2 ~ TR_", toupper(.x), "_SCORE_W1 + DIAL + (1|TEACHER_ID)")),
          data = dial.s, REML = TRUE)) %>%
  set_names(tr.names)

## gathering results for output
tr.results <- map2(.x = tr.mods, .y = tr.param.names,
                   ~list(Parameters = get_mlm_params(.x, param.names = .y, flex = TRUE, hl = 3),
                                      Performance = performance::model_performance(.x),
                                      Model_Check = performance::check_model(.x)))

####   Teacher self-reports   ####
## model parameter names for output
t.param.names <- map(.x = str_replace_all(t.names, "_", " "),
                      ~c("Intercept", paste0("W1 ", .x), "DIAL", 
                         "n teachers", "R2"))

## running models
t.mods <- map(.x = t.names,
              ~lm(as.formula(paste0("T_", toupper(.x), "_SCORE_W2 ~ T_", toupper(.x), "_SCORE_W1 + DIAL")),
                    data = dial.t)) %>%
  set_names(t.names)

## gathering results for output
t.results <- map2(.x = t.mods, .y = t.param.names, 
                  ~list(Parameters = get_mlm_params(.x, param.names = .y, flex = TRUE, hl = 3),
                        Performance = performance::model_performance(.x),
                        Model_Check = performance::check_model(.x)))


####   Student self-reports   ####
## model parameter names for output
st.param.names <- map(.x = str_replace_all(st.names, "_", " "),
                      ~c("Intercept", paste0("W1 ", .x), "DIAL",
                         "Intercept SD", "Residual SD", "n students", "n teachers", "R2"))

## running models
st.mods <- map(.x = st.names,
               ~lmer(as.formula(paste0("ST_", toupper(.x), "_SCORE_W2 ~ ST_", toupper(.x),"_SCORE_W1 + DIAL + (1|TEACHER_ID)")),
                     data = dial.s, REML = TRUE)) %>%
  set_names(st.names)

## gathering results for output
st.results <- map2(.x = st.mods, .y = st.param.names,
                   ~list(Parameters = get_mlm_params(.x, param.names = .y, flex = TRUE, hl = 3),
                         Performance = performance::model_performance(.x),
                         Model_Check = performance::check_model(.x)))


#####################################################################################


# -----      Refined Models      -----
library(semTools)

# NOTE: These models are run using lavaan so we can use FIML and auxiliary variables (must be continuous)
#       to help with missing data and improve power. Uses saturated-correlates approach (Enders, 2008) for auxiliary variables
#       School-level is ignored b/c too many models did not converge in preliminary analysis
#       Cluster (teacher) robust standard errors used for student outcomes and ‘HuberWhite’ robust standard errors for teacher outcomes

####      Trial Run      ####
# 
# dial.tr <- dial.s %>%
#   filter(TR_WAVES_PRESENT !=0) # drops 28 cases that were student self-report only
# 
# ## trial run with multilevel model
# tr.example <- paste0("
# level: 1\nTR_", toupper(tr.names[[1]]), "_SCORE_W2 ~ TR_", toupper(tr.names[[1]]), "_SCORE_W1\nlevel: 2\nTR_", toupper(tr.names[[1]]), "_SCORE_W2 ~ DIAL")
# cat(tr.example)
# tr.ex.fit <- sem(model = tr.example, data = dial.tr, cluster = "TEACHER_ID", estimator = "MLR", missing = "FIML", fixed.x = FALSE)
# summary(tr.ex.fit) # model does not converge properly
# 
# ## trial run with cluster robust standard errors
# # Also using MLR vs. ML doesn't seem to matter even for the test statistic (probs cuz df = 0)
# tr.ex2 <- paste0("TR_", toupper(tr.names[[1]]), "_SCORE_W2 ~ TR_", toupper(tr.names[[1]]), "_SCORE_W1 + DIAL")
# tr.ex2.fit <- sem(model = tr.ex2, data = dial.tr, cluster = "TEACHER_ID", missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)
# summary(tr.ex2.fit, fit.measures = TRUE) # get the not positive definite vcov matrix of estimated params warning that we've gotten before when using the cluster argument
# 
# ## running check suggested by Terrence Jorgenson: https://stackoverflow.com/questions/70183347/can-i-trust-my-cfa-results-if-the-variance-covariance-matrix-does-not-appear-to
# cov <- lavInspect(tr.ex2.fit, "cov.ov")
# mn <- lavaan::lavInspect(tr.ex2.fit, "sampstat")$mean
# check <- sem(model = tr.ex2, sample.cov = cov, sample.nobs = nrow(dial.tr), sample.mean = mn, missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)
# summary(check) # estimates don't change, so we are probably good; SE's predictably small since we don't account for clustering here
# 
# ## using the auxiliary function from semTools 
# # get same not positive definite warning & can't run check since sem.auxiliary requires full dataset
# # but estimates and standard errors not impacted much so I think we're good
# avs <- c(paste0("TR_", toupper(tr.names[-1]), "_SCORE_W1"), paste0("TR_", toupper(tr.names[-1]), "_SCORE_W2"))
# auxfit <- sem.auxiliary(model = tr.ex2, aux = avs, data = dial.tr, cluster = "TEACHER_ID", missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)
# summary(auxfit)



####   Teacher reports of students   ####

## generating model syntax for each outcome
tr.aux.mods <- map(.x = tr.names,
                ~paste0("TR_", toupper(.x), "_SCORE_W2 ~ TR_", toupper(.x), "_SCORE_W1 + DIAL"))
## defining auxiliary variables (i.e., other wave 1 and 2 scores); must be continuous
tr.aux.vars <- map(1:length(tr.names),
                ~c(paste0("TR_", toupper(tr.names[-.x]), "_SCORE_W1"), paste0("TR_", toupper(tr.names[-.x]), "_SCORE_W2")))

## running models
tr.aux.fits <- map2(.x = tr.aux.mods, .y = tr.aux.vars,
                    ~sem.auxiliary(model = .x, aux = .y, data = dial.tr, cluster = "TEACHER_ID", missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)) %>%
  set_names(tr.names)

## gathering output
tr.outs <- data.frame(Outcome = "Teacher Reports of Students (n = 472)") %>%
  bind_rows(map_dfr(.x = tr.aux.fits, ~out_reg(.x)) %>%
              mutate(Outcome = str_replace(tr.names, "_", " ")) %>% # check if Outcome order matches lhs; confirmed
              select(Outcome, everything(), -lhs))


####   Teacher self-reports   ####

## generating model syntax for each outcome
t.names1 <- t.names[-1]
t.aux.mods <- map(.x = t.names1,
                   ~paste0("T_", toupper(.x), "_SCORE_W2 ~ T_", toupper(.x), "_SCORE_W1 + DIAL"))
## defining auxiliary variables (i.e., other wave 1 and 2 scores); must be continuous
t.aux.vars <- map(1:length(t.names1),
                   ~c(paste0("T_", toupper(t.names1[-.x]), "_SCORE_W1"), paste0("T_", toupper(t.names1[-.x]), "_SCORE_W2")))

## running models
t.aux.fits <- map2(.x = t.aux.mods, .y = t.aux.vars,
                    ~sem.auxiliary(model = .x, aux = .y, data = dial.t, estimator = "MLR", missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)) %>%
  set_names(t.names1)

## gathering output
t.outs.temp <- data.frame(Outcome = "Teacher Self-Reports (n = 65)") %>%
  bind_rows(map_dfr(.x = t.aux.fits, ~out_reg(.x)) %>%
              mutate(Outcome = str_replace_all(t.names1, "_", " ")) %>% # check if Outcome order matches lhs; confirmed
              select(Outcome, everything(), -lhs))

# ## Normative Attitudes, Avoidance Attitudes, and Positive Interactions
# # do not have confidence intervals
# # warning that optimizer claimed to converge, but not all elements of gradient are (near) zero
# check.norm <- sem.auxiliary(model = t.aux.mods[[5]], aux = t.aux.vars[[5]],
#                             data = dial.t, missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)

## running those models without auxiliary variables
t.nox.fits <- map2(.x = t.aux.mods[c(4,5,12)], .y = t.aux.vars[c(4,5,12)],
                   ~sem(model = .x, data = dial.t, estimator = "MLR", missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)) %>%
  set_names(t.names1[c(4,5,12)])

## parameter estimates similar to aux models and similar estimate and CI range to listwise models
t.nouxts <- map_dfr(t.nox.fits, ~out_reg(.x)) %>%
  mutate(Outcome = str_replace_all(t.names1[c(4,5,12)], "_", " ")) %>% # check if Outcome order matches lhs; confirmed
  select(Outcome, everything(), -lhs)

t.outs <- t.outs.temp[-c(5, 6, 13), ] %>%
  bind_rows(t.nouxts) %>%
  mutate(Outcome = factor(Outcome, levels = c("Teacher Self-Reports (n = 65)", str_replace_all(t.names1, "_", " ")))) %>%
  arrange(Outcome)
  

####   Student self-reports   ####
dial.s.only <- dial.s %>%
  filter(ST_WAVES_PRESENT != 0)

## generating model syntax for each outcome
st.aux.mods <- map(.x = st.names,
                   ~paste0("ST_", toupper(.x), "_SCORE_W2 ~ ST_", toupper(.x), "_SCORE_W1 + DIAL"))
## defining auxiliary variables (i.e., other wave 1 and 2 scores); must be continuous
st.aux.vars <- map(1:length(st.names),
                   ~c(paste0("ST_", toupper(st.names[-.x]), "_SCORE_W1"), paste0("ST_", toupper(st.names[-.x]), "_SCORE_W2")))

## running models
st.aux.fits <- map2(.x = st.aux.mods, .y = st.aux.vars,
                    ~sem.auxiliary(model = .x, aux = .y, data = dial.s.only, estimator = "MLR", cluster = "TEACHER_ID", missing = "FIML", fixed.x = FALSE, meanstructure = TRUE)) %>%
  set_names(st.names)

## gathering output
st.outs <- data.frame(Outcome = "Student Self-Reports (n = 284)") %>%
  bind_rows(map_dfr(.x = st.aux.fits, ~out_reg(.x)) %>%
              mutate(Outcome = str_replace(st.names, "_", " ")) %>% # check if Outcome order matches lhs; confirmed
              select(Outcome, everything(), -lhs))


#### All results in one table ####
# effect sizes in Wave 2
es <- bind_rows(t.es %>% filter(Wave == 2) %>%
                  mutate(g = format(round(effect.size, 2), nsmall = 2)) %>%
                  select(Outcome = Scale, g),
                st.es %>% filter(Wave == 2) %>%
                  mutate(g = format(round(effect.size, 2), nsmall = 2)) %>%
                  select(Outcome = Scale, g),
                tr.es %>% filter(Wave == 2) %>%
                  mutate(g = format(round(effect.size, 2), nsmall = 2)) %>%
                  select(Outcome = Scale, g))

# all together now
reg.outs <- bind_rows(t.outs, st.outs, tr.outs) %>%
  left_join(es, by = "Outcome")


# checking sample and cluster sizes
map(st.aux.fits, ~.x@Data@Lp[[1]]$nclusters %>% Reduce(c, .))
map(tr.aux.fits, ~.x@Data@Lp[[1]]$nclusters %>% Reduce(c, .))
map(t.aux.fits, ~lavInspect(.x, "nobs"))


## checking estimation options
st.aux.fits[[1]]@Options$se
t.aux.fits[[1]]@Options$se
st.aux.fits[[1]]@Options$estimator
t.aux.fits[[1]]@Options$estimator

# grabbing exact p-values
extract_lavaan_parameters(t.aux.fits$Aggression_Problems, params = "~")

#######################################################################

###########################################
####         Mediation Models          ####

## Note: the parameter estimate vcov warning is due to using robust standard errors (warning is suppressed in run_lcs_med).
## This robust method is favored over ML because it adjusts for possible residual non-normality; no reason to use ML over the robust ML
## lavaan authors have stated that this warning can be ignored if model is correctly identified (https://groups.google.com/g/lavaan/c/4y5pmqRz4nk,
## https://stackoverflow.com/questions/69911717/cfa-in-r-the-variance-covariance-matrix-of-the-estimated-parameters-vcov-does)
## Given that it shows up using Valente, Georgeson, & Gonzalez (2021) syntax, I think we are good.

# #Using the saved regression coefficients and standard errors (Tofighi & MacKinnon, 2011; MacKinnon et al., 2002)
# # to estimate the asymmetric confidence intervals; lavaan/model_parameters produces symmetric CIs 
# lcsdistrprodCI<-RMediation::medci(lcsa,lcsb,lcssea,lcsseb,rho=0,alpha=0.05,plot=FALSE,plotCI=FALSE,type="dop")
# lcsa<-ancova@ParTable[["est"]][20]
# lcsb<-ancova@ParTable[["est"]][24]
# lcssea<-ancova@ParTable[["se"]][20]
# lcsseb<-ancova@ParTable[["se"]][24]
# lcsdistrprodCI


effect.recodes <- c(`x <-> m1` = "sm1x", `x <-> y1` = "sy1x", `m1 <-> y1` = "sm1y1",
                    `m1 -> deltam` = "sm1", `y1 -> deltay` = "sy1", 
                    `m1 -> m2` = "sm", `y1 -> y2` = "sy", 
                    `y1 -> deltam` = "bm2y1",
                    `m1 -> y2` = "by2m1", `m1 -> deltay` = "b", 
                    `deltam -> deltay` = "by2m2", 
                    `x -> deltam` = "am2x", `x -> deltay` = "by2x")

# -------     teacher report of students outcomes     ----------

#### Initial Models ####
## running models
tr.lcs <- map2(.x = rep(t.names, times = length(tr.names)),
              .y = rep(tr.names, each = length(t.names)),
              ~run_lcs_med(data = dial.tr, x = "DIAL",
                           y = .y, m = .x, pre.y = "TR_",
                           missing = "FIML",
                           cluster = "TEACHER_ID")) %>%
  set_names(str_replace_all(paste(rep(t.names, times = length(tr.names)), "to", rep(tr.names, each = length(t.names))), "_", " "))

## gathering unstandardized results
tr.lcs.out <- map(.x = tr.lcs,
                  ~out_lcs_med(.x, effect.recodes, flex = TRUE))

## gathering standardized results
tr.lcs.out.std <- map(.x = tr.lcs,
                  ~out_lcs_med(.x, effect.recodes, standardize = "std.all", flex = TRUE))


####   Specific Models with Auxiliary Variables  ####

t.med.names <- t.names[t.names %in% c("Student_Engagement", "Instructional_Strategies", "Avoidance_Attitude",
                                      "Maladaptive_Attitude", "Aggression_Problems")]


## running models
tr.aux.lcs <- map2(.x = rep(t.med.names, times = length(tr.names)),
               .y = rep(tr.names, each = length(t.med.names)),
               ~run_lcs_med(data = dial.tr, x = "DIAL",
                            y = .y, m = .x, pre.y = "TR_",
                            aux = tr.names,
                            missing = "FIML",
                            cluster = "TEACHER_ID")) %>%
  set_names(str_replace_all(paste(rep(t.med.names, times = length(tr.names)), "to", rep(tr.names, each = length(t.med.names))), "_", " "))

## gathering unstandardized results
tr.aux.lcs.out <- map(.x = tr.aux.lcs,
                  ~out_lcs_med(.x, effect.recodes, flex = TRUE))



# -------     Student self-report outcomes     ----------
## running models
st.lcs <- map2(.x = rep(t.names, times = length(st.names)),
               .y = rep(st.names, each = length(t.names)),
               ~run_lcs_med(data = dial.s.only, x = "DIAL",
                            y = .y, m = .x, pre.y = "ST_",
                            missing = "FIML",
                            cluster = "TEACHER_ID")) %>%
  set_names(str_replace_all(paste(rep(t.names, times = length(st.names)), "to", rep(st.names, each = length(t.names))), "_", " "))

## gathering unstandardized results
st.lcs.out <- map(.x = st.lcs,
                  ~out_lcs_med(.x, effect.recodes, flex = TRUE))

## gathering standardized results
st.lcs.out.std <- map(.x = st.lcs,
                  ~out_lcs_med(.x, effect.recodes, standardize = "std.all", flex = TRUE))


####   Specific Models with Auxiliary Variables  ####

## running models
st.aux.lcs <- map2(.x = rep(t.med.names, times = length(st.names)),
               .y = rep(st.names, each = length(t.med.names)),
               ~run_lcs_med(data = dial.s.only, x = "DIAL",
                            y = .y, m = .x, pre.y = "ST_",
                            aux = st.names,
                            missing = "FIML",
                            cluster = "TEACHER_ID")) %>%
  set_names(str_replace_all(paste(rep(t.med.names, times = length(st.names)), "to", rep(st.names, each = length(t.med.names))), "_", " "))

## gathering unstandardized results
st.aux.lcs.out <- map(.x = st.aux.lcs,
                  ~out_lcs_med(.x, effect.recodes, flex = TRUE))

# checking sample and cluster sizes
map(st.aux.lcs, ~.x$model@Data@Lp[[1]]$nclusters %>% Reduce(c, .))
map(tr.aux.lcs, ~.x$model@Data@Lp[[1]]$nclusters %>% Reduce(c, .))

## checking estimation options
st.aux.lcs[[1]]$model@Options$se
st.aux.lcs[[1]]$model@Options$estimator
st.aux.lcs[[1]]$model@Options$missing


###########################################

# -----   Saving Results Objects  -------------
save(t.demos, st.demos, tr.demos, fishers,
     tr.es, tr.means, tr.distdata, tr.distplots,
     t.es, t.means, t.distdata, t.distplots,
     st.es, st.means, st.distdata, st.distplots,
     tr.mods, tr.results, t.mods, t.results, st.mods, st.results,
     tr.aux.mods, tr.aux.vars, tr.aux.fits, tr.outs,
     t.aux.mods, t.aux.vars, t.aux.fits, t.nox.fits, t.nouxts, t.outs,
     st.aux.mods, st.aux.vars, st.aux.fits, st.outs, es, reg.outs,
     tr.lcs, tr.lcs.out, tr.lcs.out.std,
     st.lcs, st.lcs.out, st.lcs.out.std,
     tr.aux.lcs, tr.aux.lcs.out,
     st.aux.lcs, st.aux.lcs.out,
     file = "Output/Analysis_Results.RData")
