#############################################################
#    Trends in Anxiety, Depression, and Social Isolation    #
#    Among First-Year College Students Before and After     #
#          the onset of the Covid-19 Pandemic               #
#            Last Update: January 2026; KN                  #
#############################################################

# ----   Import Data and Initial Set-Up   -----------
source("scripts/transitions_pnf.R")

## data
dat.orig <- haven::read_sav("data/Transitions unbalanced wide for mplus.sav")
# create_codebook(dat.orig, export.type = "excel", export.name = "Transitions Codebook - Wide")

## Scale items
# Anxiety - GAD7
anx1.items <- paste0("tr1_Q1B_", 9:15)
anx2.items <- paste0("tr2_Q2_", 1:7)

# Depression - PHQ8
dep1.items <- paste0("tr1_Q1B_", 1:8)
dep2.items <- paste0("tr2_Q1_", 1:8)

# Social Isolation
si1.items <- paste0("tr1_Q7B_", 1:4)
si2.items <- paste0("tr2_Q3_", 1:4)

## coding investigations
# table(dat.orig$numberofobs, useNA = "always") # 474 responded to both

# table(dat.orig$asian_alone, dat.orig$asian_only, useNA = "always") # doesn't seem to be a difference b/t alone and only variables

# Scores are summed from item scores and must have answered all items (e.g., ID 692)
# dat.orig %>% select(ID, all_of(anx1.items), GAD_total.1) %>% View()

# rawxxxchange is T1 - T2; we want T2 - T1
# dat.orig %>% select(ID, GAD_total.1, GAD_total.2, rawanxchange) %>% View()

#### Gathering relevant variables and creating binary versions of the coping variables ####
dat <- dat.orig %>%
  select(ID, age, simple_race, hispanic, white = white_alone, black = black_alone, asian = asian_alone,
         sex_at_birth, gender_identity, female = female_identity, male = male_identity, otheri = other_identity,
         sex_orientation, sgm, first_gen, highest_parental_educ, college_year = tr1_Q1A, numberofobs,
         all_of(c(anx1.items, anx2.items, dep1.items, dep2.items, si1.items, si2.items)),
         GAD_int = GAD_total.1, GAD_end = GAD_total.2,
         PHQ_int = PHQ8.1, PHQ_end = PHQ8.2,
         PSI_int = PSI_Total.1, PSI_end = PSI_Total.2,
         cope1:cope15, covid_wary, covid_concern) %>%
  # factor demographic variables for reporting
  mutate(across(c(simple_race, sex_at_birth, gender_identity, sex_orientation,
                  highest_parental_educ, college_year), as_factor)) %>%
  # collapsing demographic groups
  mutate(bo_race = if_else(simple_race %in% c("Black alone", "Other"), 1, 0),
         nonwhite = if_else(white == 1, 0, 1),
         hs_less = if_else(highest_parental_educ %in% c("Less than high school", "High school or GED"), 1, 0),
         some_ba = if_else(highest_parental_educ %in% c("Associate's degree", "Some college, no degree", "Bachelor's degree"), 1, 0),
         hs_ba = if_else(highest_parental_educ == "Beyond Bachelor's degree", 0, 1),
         collapse_race = fct_recode(simple_race, `Black/Other` = "Black alone", `Black/Other` = "Other"),
         collapse_so = case_when(sex_orientation == "Straight" ~ "Straight",
                                 TRUE ~ "LGBQplus"),
         collapse_hpe = case_when(highest_parental_educ %in% c("Less than high school", "High school or GED") ~ "High school/GED or less",
                                  highest_parental_educ %in% c("Associate's degree", "Some college, no degree", "Bachelor's degree") ~ "Some college to Bachelor's",
                                  highest_parental_educ == "Beyond Bachelor's degree" ~ "Beyond Bachelor's",
                                  TRUE ~ NA_character_) %>%
           factor(., levels = c("High school/GED or less", "Some college to Bachelor's", "Beyond Bachelor's"))) %>%
  # dichotomizing coping strategy use
  mutate(across(cope1:cope15, ~if_else(as.numeric(.) == 1, 0, 1), .names = "{col}_b")) %>%
  # converting item and scale variables to numeric
  mutate(across(all_of(c(anx1.items, anx2.items, dep1.items, dep2.items, si1.items, si2.items)),
                as.numeric)) %>%
  mutate(across(c(GAD_int, GAD_end, PHQ_int, PHQ_end, PSI_int, PSI_end), as.numeric)) %>%
  # computing change scores
  mutate(GADc = GAD_end - GAD_int,
         PHQc = PHQ_end - PHQ_int,
         PSIc = PSI_end - PSI_int,
         cope_sum = scale_score(., items = paste0("cope", 1:15, "_b"), type = "sum", min.valid = 1)) %>%
  # final analysis includes participants who are not missing on all coping strategies and T1 and T2 scores
  mutate(include = case_when(!is.na(cope_sum) & rowSums(is.na(.[,c("GAD_int", "PHQ_int", "PSI_int",
                                                                   "GADc", "PHQc", "PSIc")])) != 6 ~ 1,
                             TRUE ~ 0))

## data checks
# table(dat$cope1, dat$cope1_b, useNA = "always")
# table(dat$cope_sum, useNA = "always")
# table(dat$highest_parental_educ, dat$collapse_hge, useNA = "always")

####   Subset dataset for analytic sample   ####

## dataset included students with:
# at least one response to a coping item and
# T1 or T2 scale score (n = 460)
datcc <- dat %>%
  filter(include == 1)


#################################################################################

# ------      Missing Data Analysis     -------

table(dat$numberofobs, useNA = "ifany")
# 650 only present at baseline; 474/1124 = 42.17% retention

## Present at T2, but did not have coping or scale responses 
# table(dat$numberofobs, dat$include) # 14 participants
# dat %>% filter(numberofobs==2 & include ==0) %>% 
#   select(ID:college_year, GAD_int:PSI_end, GADc:include) %>% View()
# # 4 did not respond to coping items; 10 missing T1 scales

## Odds of being non-attrition by demographics
miss.glm <- map_dfr(.x = c("nonwhite", "male", "first_gen", "sgm", "hs_ba"),
                       ~glm(formula(paste("include ~", .x)),
                            data = dat, family = binomial(link = "logit")) %>%
                         parameters::model_parameters(., exponentiate = TRUE)) %>%
  select(Parameter, OR = Coefficient, CI_low, CI_high, z, p)

## mean difference in baseline scores by attrition vs. non-attrition groups
miss.means <- map2_dfr(.x = c("GAD_int", "PHQ_int", "PSI_int"),
                         .y = c("GAD", "PHQ", "PSI"),
                         ~the_t(data = dat, outcome = .x, by = "include", name = .y))

###########################################################################

# -----    Descriptives and Correlations   ------

## shortcuts for selecting variables and converting to presentable names
scale.vars <- dat %>%
  select(GAD_int, GAD_end, GADc,
         PHQ_int, PHQ_end, PHQc,
         PSI_int, PSI_end, PSIc) %>% names()
scale.labels <- c("Anxiety - T1", "Anxiety - T2", "Anxiety - Change",
                "Depression - T1", "Depression - T2", "Depression - Change",
                "Social Isolation - T1", "Social Isolation - T2", "Social Isolation - Change")
names(scale.vars) <- scale.labels

cope.vars <- dat %>%
  select(cope1_b:cope15_b, cope_sum) %>% names()
cope.labels <- c("Turn to work/school", "Hobbies", "Avoid news", "Communicate with friends/family", "Get emotional support", "Help others",
                "Alcohol/drugs", "See in a different light", "Think about it less", "Make fun of situation", "Physical health", "Snacking more",
                "Religion", "Praying/meditating/yoga", "Express my negative feelings", "Total coping strategies used")
names(cope.vars) <- cope.labels

####  Descriptives by included/excluded ####
desc.comp <- dat %>%
  select(include, all_of(scale.vars[c(1,4,7)]),
         simple_race, gender_identity, sex_orientation, first_gen, collapse_hpe) %>%
  mutate(first_gen = as_factor(first_gen),
         include = as_factor(include) %>% fct_recode(., Excluded = "0", Included = "1")) %>%
  tbl_summary(by = "include",
              statistic = list(all_continuous() ~ "{mean} ({sd})\n[{min}-{max}]"),
              label = list(first_gen = "First Generation", 
                           collapse_hpe = "Highest Parent Education")) %>%
  add_p(test = list(all_continuous() ~ "t.test")) %>%
  as_flex_table()

#### Correlations among LPA and demographic variables  ####
score.corr <- datcc %>%
  select(all_of(scale.vars[c(1,3,4,6,7,9)]),
         # hispanic:asian, male, SGM = sgm,
         # `First Generation` = first_gen, `HS or less` = hs_less, `Some College/BA` = some_ba
         ) %>%
  # rename_with(.fn = str_to_title, .cols = hispanic:male) %>%
  psych::corr.test(use = "pairwise") # plot created in transitions_report.Rmd
  
# round(score.corr$p, 3)
  

#########################################################


# ------      Psychometric Checks     -------

####  Anxiety - GAD7  ####
## Time 1
# full sample 
anx1.kfa.0 <- kfa(dat, variables = anx1.items, m = 2, ordered = TRUE, k = 0)
kfa_report(anx1.kfa.0, file.name = "Psychometric reports/GAD7_T1_fullsample",
           report.title = "GAD7 - Time 1 Full Sample - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

# 460 sample
anx1.kfa.460 <- kfa(datcc, variables = anx1.items, m = 2, ordered = TRUE, k = 0)
kfa_report(anx1.kfa.460, file.name = "Psychometric reports/GAD7_T1_n460",
           report.title = "GAD7 - Time 1 (460 sample) - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"),
           plots = FALSE)


## Time 2
# full sample 
anx2.kfa.0 <- kfa(dat, variables = anx2.items, m = 2, ordered = TRUE, k = 0)
kfa_report(anx2.kfa.0, file.name = "Psychometric reports/GAD7_T2_fullsample",
           report.title = "GAD7 - Time 2 Full Sample - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

# 460 sample 
anx2.kfa.460 <- kfa(datcc, variables = anx2.items, m = 2, ordered = TRUE, k = 0)
kfa_report(anx2.kfa.460, file.name = "Psychometric reports/GAD7_T2_n460",
           report.title = "GAD7 - Time 2 (460 sample) - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))



####  Depression - PHQ8  ####
## Time 1
# full sample 
dep1.kfa.0 <- kfa(dat, variables = dep1.items, m = 2, ordered = TRUE, k = 0)
kfa_report(dep1.kfa.0, file.name = "Psychometric reports/PHQ8_T1_fullsample",
           report.title = "PHQ8 - Time 1 Full Sample - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

# 460 sample
dep1.kfa.460 <- kfa(datcc, variables = dep1.items, m = 2, ordered = TRUE, k = 0)
kfa_report(dep1.kfa.460, file.name = "Psychometric reports/PHQ8_T1_n460",
           report.title = "PHQ8 - Time 1 (460 sample) - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"),
           plots = FALSE)


## Time 2
# full sample 
dep2.kfa.0 <- kfa(dat, variables = dep2.items, m = 2, ordered = TRUE, k = 0)
kfa_report(dep2.kfa.0, file.name = "Psychometric reports/PHQ8_T2_fullsample",
           report.title = "PHQ8 - Time 2 Full Sample - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

# 460 sample 
dep2.kfa.460 <- kfa(datcc, variables = dep2.items, m = 2, ordered = TRUE, k = 0)
kfa_report(dep2.kfa.460, file.name = "Psychometric reports/PHQ8_T2_n460",
           report.title = "PHQ8 - Time 2 (460 sample) - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"),
           plots = FALSE)

####  Social Isolation - SI  ####
## Time 1
# full sample 
si1.kfa.0 <- kfa(dat, variables = si1.items, m = 2, ordered = TRUE, k = 0)
kfa_report(si1.kfa.0, file.name = "Psychometric reports/PSI_T1_fullsample",
           report.title = "PSI - Time 1 Full Sample - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

# 460 sample 
si1.kfa.460 <- kfa(datcc, variables = si1.items, m = 2, ordered = TRUE, k = 0)
kfa_report(si1.kfa.460, file.name = "Psychometric reports/PSI_T1_n460",
           report.title = "PSI - Time 1 (460 sample) - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))


## Time 2
# full sample 
si2.kfa.0 <- kfa(dat, variables = si2.items, m = 2, ordered = TRUE, k = 0)
kfa_report(si2.kfa.0, file.name = "Psychometric reports/PSI_T2_fullsample",
           report.title = "PSI - Time 2 Full Sample - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))

# 460 sample 
si2.kfa.460 <- kfa(datcc, variables = si2.items, m = 2, ordered = TRUE, k = 0)
kfa_report(si2.kfa.460, file.name = "Psychometric reports/PSI_T2_n460",
           report.title = "PSI - Time 2 (460 sample) - Psychometric Report",
           index = c("cfi.scaled", "rmsea.scaled", "srmr"))


############################################################################


# -----   Prepping Mplus data  -----
mplusdat <- dat %>%
  select(ID, include, age, hispanic:asian, bo_race, nonwhite,
         female, male, otheri, sgm, fg = first_gen, hs_less, some_ba, hs_ba,
         GAD_int:PSI_end, GADc, PHQc, PSIc,
         cope1_b:cope15_b, cope_sum) %>%
  mutate(across(everything(), as.numeric))


# double-checking length of names and variable types conform with Mplus
table(nchar(names(mplusdat)))
table(sapply(names(mplusdat), function(x) class(mplusdat[[x]])))

# Mplus with 460 sample
mplusdat460 <- mplusdat %>%
  filter(include == 1)

####################################################

#  -------  Step 1 - Enumeration  ---------------------
enum.sntx <- map(.x = 1:7,
                 ~mplusObject(
                   rdata = mplusdat460,
                   usevariables = c("ID", "GAD_int", "PHQ_int", "PSI_int",
                                    "GADc", "PHQc", "PSIc"),
                   TITLE = glue::glue("COVID paper - Mental Health LPA - {.x} profiles;"),
                   VARIABLE =
glue::glue("IDVARIABLE = ID;
CLASSES = c({.x});"),
                   ANALYSIS =
"TYPE = MIXTURE;
 STARTS = 2000 500;
 PROCESSORS = 4;",
                   # MODEL = "%OVERALL%",
                   OUTPUT = "ENTROPY TECH1 TECH11 TECH14;",
                   SAVEDATA =
glue::glue("FILE = step1_{.x}_probs.dat;
SAVE = CPROBABILITIES;"),
                   PLOT =
"TYPE IS PLOT3;
SERIES IS GAD_int (1) GADc (2) PHQ_int (3) PHQc (4) PSI_int (5) PSIc (6);"))

enum.run <- map2(.x = enum.sntx, .y = 1:7,
                 ~mplusModeler(.x,
                               dataout = glue::glue("Mplus BCH/Step 1/Sample 460/step1_{.y}class.dat"),
                               modelout = glue::glue("Mplus BCH/Step 1/Sample 460/step1_{.y}class.inp"),
                               run = 1, check = TRUE, hashfilename = TRUE,
                               writeData = "always"))

####  Gathering Results ####

## Importing models
enum460.mods <- readModels(target = "Mplus BCH/Step 1/Sample 460")

## Profiles counts/proportion
enum460.cp <- map2_dfr(enum460.mods, 1:7,
                       ~left_join(.x$class_counts$modelEstimated,
                                  .x$class_counts$mostLikely,
                                  by = "class", suffix = c("_ModelEstimated", "_MostLikely")) %>%
                         mutate(Model = .y)) %>%
  select(Model, Profile = class, everything())

## Gathering fit information
enum460.fit <- map_dfr(enum460.mods, ~mplus_fit(.x, lpa = TRUE, format = FALSE))

# plotting information criterion (Nylund-Gibson & Choi, 2018)
enum460.fitplot <- enum460.fit %>% select(Profiles, AIC:CAIC) %>%
  tidyr::gather("IC", "Value", AIC:CAIC) %>%
  ggplot(aes(x = Profiles, y = Value, group = IC, linetype = IC)) +
  geom_line() +
  scale_x_continuous(limits = c(1, 7), breaks = seq(1, 7, 1)) +
  theme_bw(base_size = 18)

## Joining and formating fit and profile size
enum460.tab1 <- enum460.fit %>%
  select(K = Profiles, LL, SABIC, CAIC, VLMRT = LMRT_value, p = LMRT_p, Entropy) %>%
  left_join(enum460.cp %>%
              select(K = Model, Profile, count = count_MostLikely) %>%
              pivot_wider(names_from = "Profile", values_from = "count") %>%
              unite(col = "Profile n", `1`:`7`, sep = "/", na.rm = TRUE))
  
  
## Gathering means for each profile
# data
enum460.params <- map2_dfr(.x = enum460.mods[1:6], .y = 1:6,
                          ~.x$parameters$unstandardized %>% mutate(Model = .y)) %>%
  bind_rows(enum460.mods$step1_7class.out$parameters$unstandardized %>%
              mutate(est_se = as.numeric(est_se),
                     Model = 7))

enum460.dat <- enum460.params %>%
  filter(LatentClass != "Categorical.Latent.Variables" & paramHeader == "Means") %>%
  mutate(Indicator = case_when(param == "GAD_INT" ~ "GAD Baseline",
                               param == "GADC" ~ "GAD Change",
                               param == "PHQ_INT" ~ "PHQ Baseline",
                               param == "PHQC" ~ "PHQ Change",
                               param == "PSI_INT" ~ "PSI Baseline",
                               param == "PSIC" ~ "PSI Change") %>%
           factor(., levels = c("GAD Baseline", "PHQ Baseline", "PSI Baseline",
                                "GAD Change", "PHQ Change", "PSI Change"))) %>%
  rename(Profile = LatentClass)

# plot
enum460.plot <-  enum460.dat %>%
  ggplot(aes(x = Indicator, y = est, group = Profile, colour = Profile)) +
  geom_point() +
  geom_line() +
  theme_bw(base_size = 18) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  facet_wrap(~Model, nrow = 3)

####################################################

#  -------  Step 2 - Final Profile Model  ---------------------

# Note: auxiliary variables must be treated as continuous
step2.sntx <- mplusObject(
  rdata = mplusdat460,
  usevariables = c("ID", "GAD_int", "PHQ_int", "PSI_int",
                   "GADc", "PHQc", "PSIc", paste0("cope", 1:15,"_b"),
                   "nonwhite","male", "sgm", "fg", "hs_ba"),
  TITLE = "COVID paper - 6 profiles - Step 2;",
  VARIABLE = "IDVARIABLE = ID;
              CLASSES = c(6);
              AUXILIARY = cope1_b-cope15_b
  nonwhite male sgm fg hs_ba;",
  ANALYSIS = "TYPE = MIXTURE;
              STARTS = 2000 500;
              PROCESSORS = 4;",
  # MODEL = "%OVERALL%",
  OUTPUT = "ENTROPY TECH1 TECH11 TECH14;",
  SAVEDATA = "FILE = step2_6_bchweights.dat;
              SAVE = bchweights;")

step2.run <- mplusModeler(step2.sntx,
                          dataout = "Mplus BCH/Steps 2 and 3/Sample 460/step2_6p.dat",
                          modelout = "Mplus BCH/Steps 2 and 3/Sample 460/step2_6p.inp",
                          run = 1, check = TRUE, hashfilename = TRUE,
                          writeData = "always")

####################################################

#  -------  Step 3 - Estimating associations  ---------------------

#### Import dataset with BCH weights ####
bchw <- readr::read_table("Mplus BCH/Steps 2 and 3/Sample 460/step2_6_bchweights.dat", 
                          col_names = FALSE, na = "*")
# add column names - given at end of Mplus output file
names(bchw) <- c("GAD_INT","PHQ_INT","PSI_INT","GADC","PHQC","PSIC",
                 "COPE1_B","COPE2_B","COPE3_B","COPE4_B","COPE5_B",
                 "COPE6_B","COPE7_B","COPE8_B","COPE9_B","COPE10_B",
                 "COPE11_B","COPE12_B","COPE13_B","COPE14_B","COPE15_B",
                 "NONWHITE","MALE","SGM","FG","HS_BA",
                 "BCHW1","BCHW2","BCHW3","BCHW4","BCHW5","BCHW6","ID")


#### Coping as distal outcome ####
step3out.sntx <- mplusObject(
  rdata = bchw,
  usevariables = c("ID", paste0("COPE", 1:15,"_B"),
                   "NONWHITE","MALE","SGM","FG","HS_BA",
                   paste0("BCHW", 1:6)),
  TITLE = "COVID paper - 6 profiles - Step 3;",
  VARIABLE = "IDVARIABLE = ID;
  CATEGORICAL = COPE1_B-COPE15_B;
  CLASSES = c(6);
  TRAINING = BCHW1-BCHW6 (bch);",
  ANALYSIS = "TYPE = MIXTURE;
  STARTS = 0;",
  MODEL = "%OVERALL%
  C ON NONWHITE MALE SGM FG HS_BA;
  
  %C#1%
  [COPE1_B$1-COPE15_B$1];
  
  %C#2%
[COPE1_B$1-COPE15_B$1];
  
  %C#3%
[COPE1_B$1-COPE15_B$1];
  
  %C#4%
[COPE1_B$1-COPE15_B$1];
  
  %C#5%
[COPE1_B$1-COPE15_B$1];
  
  %C#6%
[COPE1_B$1-COPE15_B$1];",
  OUTPUT = "TECH1 sampstat;")

step3out.run <- mplusModeler(step3out.sntx,
                             dataout = "Mplus BCH/Steps 2 and 3/step3_6p_out.dat",
                             modelout = "Mplus BCH/Steps 2 and 3/step3_6p_out.inp",
                             run = 1, check = TRUE, hashfilename = TRUE,
                             writeData = "always")


prop.table(table(bchw$COPE1_B)) # 1 = Mplus Category 2 = Yes, used strategy;
prop.table(table(datcc$cope1_b))

## Exploring reviewer suggestion to add covariates to distal outcomes
step3out.sntx2 <- mplusObject(
  rdata = bchw,
  usevariables = c("ID", paste0("COPE", 1:15,"_B"),
                   "NONWHITE","MALE","SGM","FG","HS_BA",
                   paste0("BCHW", 1:6)),
  TITLE = "COVID paper - 6 profiles - Step 3;",
  VARIABLE = "IDVARIABLE = ID;
  CATEGORICAL = COPE1_B-COPE15_B;
  CLASSES = c(6);
  TRAINING = BCHW1-BCHW6 (bch);",
  ANALYSIS = "TYPE = MIXTURE;
  STARTS = 0;",
  MODEL = "%OVERALL%
  C ON NONWHITE MALE SGM FG HS_BA;
  COPE1_B-COPE15_B ON NONWHITE;
  
  %C#1%
  [COPE1_B$1-COPE15_B$1];
  
  %C#2%
  [COPE1_B$1-COPE15_B$1];

  %C#3%
[COPE1_B$1-COPE15_B$1];
  
  %C#4%
[COPE1_B$1-COPE15_B$1];

  %C#5%
[COPE1_B$1-COPE15_B$1];

  %C#6%
[COPE1_B$1-COPE15_B$1];",
  OUTPUT = "TECH1 sampstat;")

step3out.run2 <- mplusModeler(step3out.sntx2,
                             dataout = "Mplus BCH/Steps 2 and 3/step3_6p_out2.dat",
                             modelout = "Mplus BCH/Steps 2 and 3/step3_6p_out2.inp",
                             run = 1, check = TRUE, hashfilename = TRUE,
                             writeData = "always")


prop.table(table(bchw$COPE1_B)) # 1 = Mplus Category 2 = Yes, used strategy;
prop.table(table(datcc$cope1_b))

#  -------  Extracting Results  ---------------------

distal.mods <- readModels(target = "Mplus BCH/Steps 2 and 3/Sample 460")

#### probability of selecting each coping mechanism ####
cope.prob <- distal.mods$step3_6p_out.out$parameters$probability.scale %>%
  filter(category == 2) %>% # the yes's
  mutate(param = as_factor(param),
         Strategy = fct_recode(param, !!!toupper(cope.vars[1:15]))) %>%
  select(Strategy, Profile = LatentClass, Probability = est, se)

## Supplemental Table
cope.prob.wide <- cope.prob %>%
  mutate(Strategy = fct_reorder(Strategy, Probability)) %>%
  select(-se) %>%
  tidyr::spread(Profile, Probability) %>%
  arrange(desc(Strategy))

## Point plot
cope.prob.point <-  cope.prob %>%
  ggplot(aes(y = fct_reorder(Strategy, Probability), x = Probability, group = Profile, shape = Profile)) +
  geom_point(size = 4) +
  # geom_col(position = position_dodge(.9)) +
  scale_x_continuous(limits = c(0, 1.01), breaks = seq(0, 1, .2)) +
  labs(y = "Coping Strategy") +
  theme_bw(base_size = 20) +
  theme(legend.position="bottom")

#### Get the latent profile odds ratio results ####

## reading in from Mplus output
OR_comps <- distal.mods$step3_6p_out.out$output[871:1366] #[749:1244]

## separating each set of comparisons (e.g., profile 1 to 2, 2 to 3)
ncomps <- seq(1, length(OR_comps)-1, 33)
comps.list <- map(.x = ncomps,
            ~OR_comps[.x:(.x+32)])

## extracting info into a dataframe and converting to numeric
comps.df <- map_dfr(.x = comps.list,
                ~LP_OR_Results(.x)) %>%
  mutate(across(.cols = everything(), str_trim)) %>%
  mutate(Comp = paste0(P1, " to ", P2),
         Strategy = fct_recode(Strategy, !!!toupper(cope.vars[1:15]))) %>%
  mutate(across(.cols = OR:P, as.numeric)) %>%
  filter(!is.na(OR)) %>% # Cope9 - think about it less was fixed to 1 for profile 1
  rename(Lower_CI = Statistic, Upper_CI = P) %>%  # output changed from Stat and P to 95%CI once we added the covariates
  mutate(z = (OR - 1)/SE,                         # so manually computing Z-statistic and P
         p = if_else(OR > 1, pnorm(z, lower.tail = F)*2,
                     pnorm(z, lower.tail = T)*2)) %>%
  mutate(p.adj = p.adjust(p, method = "BH")) %>%
  mutate(across(z:p.adj, ~round(., 3))) %>%
  select(Comp, Strategy, OR, SE, z, p, p.adj)

comps.sig <- comps.df %>%
  filter(p < .10)

####   Covariate Parameters  ####

# see Parameterization using Reference Class 2 in mplus output
comp <- distal.mods$step3_6p_out.out$output[1678:1715]

ncovs <- 5  # number of covariates in the model
start <- 3  # line in comp identifying class
profiles <- 6 # number of profiles
prof <- profiles - 1

strategies <- data.frame(Profile = rep(NA, ncovs),
                         Covariate = rep(NA, ncovs),
                         OR = rep(NA, ncovs),
                         SE = rep(NA, ncovs),
                         Lower_CI = rep(NA, ncovs),
                         Upper_CI = rep(NA, ncovs))
together <- strategies[1,]

for(p in seq(start, by = ncovs+2, length.out = prof)){
  for(c in 1:ncovs){
  strategies[c, ] <- data.frame(
    Profile = comp[[p]],
    Covariate = str_sub(comp[[p+c]], 5, 22),
    OR = str_sub(comp[[p+c]], 23,28),
    SE = str_sub(comp[[p+c]], 34,39),
    Lower_CI = str_sub(comp[[p+c]], 45,50),
    Upper_CI = str_sub(comp[[p+c]], 56,61))
  
  }
  together <- bind_rows(together, strategies)
}

## cleaning up for output
cov.params <- together[-1,] %>%
  mutate(across(.cols = c(OR:Upper_CI), as.numeric)) %>%
  mutate(Profile = str_extract(Profile, "[:digit:]"),
         Covariate = str_trim(Covariate, side = "both"),
         OR_SE = paste0(format(round(OR, 2), nsmall = 2), " (",
                        format(round(SE, 2), nsmall = 2), ")")) %>%
  select(Profile, Covariate, OR_SE) %>%
  pivot_wider(names_from = Profile, values_from = OR_SE)
# Use p-values from ALTERNATIVE PARAMETERIZATIONS FOR THE CATEGORICAL LATENT VARIABLE REGRESSION section in Mplus output


#### Demographics by Profile ####

## using max bchweight based on most probable profile (or read in step1_6_probs.dat)
C <- max.col(bchw[,paste0("BCHW", 1:6)])
table(C)

datcwdemos <- datcc %>%
  inner_join(data.frame(ID = bchw$ID, C = C), by = "ID") %>%
  select(C, all_of(scale.vars), cope_sum,
         # cope1_b:cope15_b, , age,
         simple_race, sex_at_birth, gender_identity,
         sex_orientation, first_gen, highest_parental_educ, starts_with("collapse")) %>%
  mutate(across(simple_race:collapse_hpe, as_factor)) %>%
  mutate(highest_parental_educ = fct_drop(highest_parental_educ))

# coping sum by profile
skimr::skim(datcwdemos, cope_sum)
datcwdemos %>% group_by(C) %>% skimr::skim(cope_sum)

# by profile
desc460.c <- datcwdemos %>%
  select(C, ends_with("T1"), ends_with("Change"), simple_race, gender_identity, sex_orientation, first_gen, collapse_hpe) %>%
  tbl_summary(by = "C",
              missing = "no",
              statistic = list(all_continuous() ~ "{mean} ({sd})"),
              digits = list(all_continuous() ~ c(2, 2)),
              percent = "column") %>%
  add_overall() %>%
  modify_table_body(~.x %>% mutate(stat_1 = ifelse(readr::parse_number(stat_1) < 6, "xx", stat_1)) %>%
                      mutate(stat_2 = ifelse(readr::parse_number(stat_2) < 6, "xx", stat_2)) %>%
                      mutate(stat_3 = ifelse(readr::parse_number(stat_3) < 6, "xx", stat_3)) %>%
                      mutate(stat_4 = ifelse(readr::parse_number(stat_4) < 6, "xx", stat_4)) %>%
                      mutate(stat_5 = ifelse(readr::parse_number(stat_5) < 6, "xx", stat_5)) %>%
                      mutate(stat_6 = ifelse(readr::parse_number(stat_6) < 6, "xx", stat_6))) %>%
  as_flex_table() %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::padding(padding = 0, part = "all")

# Model estimated means 
desc460.c.change <- enum460.dat %>%
  filter(Model == 6) %>%
  select(Indicator, Profile, est) %>%
  pivot_wider(names_from = Profile, values_from = est) %>%
  flextable::flextable() %>%
  flextable::colformat_double(j = 2:7, digits = 2) %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::padding(padding = 0, part = "all")

###############################################


#### Covariates added to model so this section is not used ####
## X2 tests
datcwdemos <- datcwdemos %>%
  mutate(collapse_race2 = if_else(simple_race %in% c("Black alone", "Hispanic", "Other"), "Other", as.character(simple_race)),
         collapse_geni = ifelse(gender_identity == "Other", NA, gender_identity))

# running tests and gathering results
x2vars <- c("collapse_race", "collapse_geni",
            "collapse_so", "first_gen", "collapse_hpe")
names(x2vars) <- c("race", "gender identity", "sexual orientation",
                   "first generation", "highest parental education")
lalist <- vector("list", length = length(x2vars))
for(i in 1:length(x2vars)){

  out <- chisq.test(datcwdemos[[x2vars[[i]]]], datcwdemos$C)
  out.df <- data.frame(x2 = out$statistic[[1]],
                       df = out$parameter[[1]],
                       p = out$p.value[[1]])
  out.df$group <- names(x2vars)[[i]]
  lalist[[i]] <- out.df
}

x2df <- Reduce(rbind, lalist) %>%
  select(group, everything())


###########################################################

# ---- Saving output -----
save(miss.glm, miss.means, score.corr, desc.comp,
     desc460.c, desc460.c.change, x2df,
     enum460.tab1, enum460.fit, enum460.fitplot, enum460.plot,
     enum460.params, enum460.dat, enum460.cp,
     cope.prob, cope.prob.wide, cope.prob.point,
     comps.df, comps.sig, cov.params,
     file = "data/Covid_results_20260106.RData"
     # file = "Covid_results_2025827.RData"
     )

