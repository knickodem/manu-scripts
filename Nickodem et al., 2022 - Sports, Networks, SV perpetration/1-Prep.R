########################################
#                                      #
#       Paper 2 - Sports & SV          #
#   Data Preparation and Exploration   #
#                                      #
########################################

## Loading packages and functions
source("Data and Scripts/0-PnF.R")

## Importing data
# noms <- read_sav("Data and Scripts/SOS_COACH_NOMINATIONS_W4.sav")
# temp <- read_sav("Data and Scripts/sources-of-strength_w1-w4_with_Demos_Jokesters_Coaches.sav")
# check <- read_sav("Data and Scripts/sos_coach_allvar.sav")
dscombo <- read_sav("Data and Scripts/w1-w4 SoS_Combined.sav")

# dscombo %>% select(StudentID, all_of(paste0("Friend", 1:7, "W4"))) %>% head()

# table(noms$NOMINATED_A_COACH_W4)

## Pausing the addition of coaching variables until I figure out whether we will actually use it
## because converting the SUBJECT_ID to StudentID is gonna be a pain in the ass
## (though ultimately the process needs to be better documented anyway)
# schoolnumb <- temp %>%
#   mutate(sid = str_sub(SUBJECT_ID, 1, 2)) %>%
#   select(sid, SCHOOLS_W1) %>%
#   unique() %>%
#   drop_na() %>%
#   filter(sid != "Gk")

## Examine sports variables and coding 
# sportonly <- read_sav("Data and Scripts/sportonlydataset.sav")
# sportall <- read_sav("Data and Scripts/sports_recoded_+genderSVraceAODdismissiveness.sav")


## Keeping variables for analysis
# all W4 variables except SV variables which need to be combined to create previous perp variable
ds1 <- dscombo %>%
  filter_at(vars(starts_with("Inaccurate_Response")),    # removing inaccurate response cases
            all_vars(is.na(.) | . == 2)) %>%
  rename_with(~str_replace(., "PpFr", "ppfr"), .cols = matches("^PpFr")) %>%  # make friend characteristics variable names consistent (lowercase)
  rename_with(~str_replace(., "PPF", "ppf"), .cols = matches("^PPF")) %>%
  rename_with(~str_replace(., "Pp", "pp"), .cols = matches("^Pp")) %>%
  select(StudentID, School = SCHOOL_NAME_ALL, HasASurvey_W4, one_of(demovars[-c(6,7)]),  # excludes Age and Tx which are defined separately
         Tx = CONDITION_W4, Age = "AGE_W4",
         GENDER_FEMALE_W4, GENDER_MALE_W4, GENDER_OTHER_W4, GENDER_OTHER_SPECIFY_W4,
         RACE_AFRIC_1_W4, RACE_AMER_INDIAN_2_W4, RACE_ASIAN_3_W4, RACE_HISP_4_W4, RACE_MULT_5_W4, RACE_NATIVE_AMER_6_W4, RACE_WHITE_7_W4,
         SEXUAL_BISEXUAL_W4, SEXUAL_GAY_LESBIAN_W4, SEXUAL_QUEST_W4, SEXUAL_STRAIGHT_W4, SEXUAL_OTHER_W4, SEXUAL_OTHER_SPECIFY_W4,
         contains("SPORT"),
         all_of(paste0(c(snvars, friendcharvars, trustedadultvars, adulttypevars), "_W4")),
         SexVioPerp3Cat_W4, SexVioPerp4Cat_W4, all_of(paste0("S_SEX_VIOL_PERP_W", 1:4)), # Categories and scores calculated by Kelly
         all_of(paste0(c(nocontactperpvars, contactperpvars), "_W", rep(1:4, each = 13))), # All outcome items across all waves
         S_DISMISS_SEX_VIOL_W4, all_of(paste0(dismissvars, "_W4")),
         S_AOD_NEXT6_MTHS_W4, all_of(paste0(substanceusevars, "_W4")),
         all_of(paste0("Friend", 1:7, "W4"))) %>% 
  rename_with(~str_remove(., "_W4"), .cols = matches("_W4$"))   # remove _W4 from variable names

## identifying cases to keep
# filter(dscombo, HasASurvey_W4 == 1) %>% nrow() # 3501
# filter(dscombo, !is.na(CONDITION_W4)) %>% nrow() # 3509

# # alternative method
# nacheckvars <- ds1 %>%
#   select(all_of(c("SPORT_1", nocontactperpvars, contactperpvars,
#                    nocontactvictvars,  contactvictvars))) %>% names()
# narows <- apply(ds1[,nacheckvars], 1, function(x){all(is.na(x))})
# ds2 <- ds1[!narows, ] # 3411

## Also creates isolated variable
ds1 <- filter(ds1, !is.na(Tx)) %>% # Tx indicator also shows if they had a survey (3506)
  mutate(isolated = case_when(is.na(outdegree) ~ NA_integer_,
                              outdegree == 0 ~ 1L,
                              TRUE ~ 0L))


#### Investigating Wave 4 gender and sexual orientation identification compared to across wave coding ####
gendervars <- ds1 %>% select(starts_with("GENDER_")) %>% names()
lapply(gendervars[-4], function(x) table(as_factor(ds1$Gender), ds1[[x]], useNA = "always")) %>% set_names(gendervars[-4])
# Coding across waves largely matches W4 coding with the exception of Other, which still include joke responses
table(ds1$GENDER_OTHER_SPECIFY) # joke responses are accounted for in Gender, but not W4
table(ds1$GENDER_FEMALE, ds1$GENDER_MALE) # 16
table(ds1$GENDER_FEMALE, ds1$GENDER_OTHER) # 8

sovars <- ds1 %>% select(starts_with("SEXUAL_")) %>% names()
lapply(sovars[-6], function(x) table(as_factor(ds1$SexOr), ds1[[x]], useNA = "always")) %>% set_names(sovars[-6])
table(ds1$SEXUAL_OTHER_SPECIFY) # joke responses are accounted for in SexOr, but not W4

racevars <- ds1 %>% select(starts_with("RACE_")) %>% names()
lapply(racevars, function(x) table(as_factor(ds1$Race), ds1[[x]], useNA = "always")) %>% set_names(racevars)

## CONCLUSION:
# Will use across wave coding b/c coding not drastically different, across wave accounts for joke responses

# ------------- Sports ------------------

# Defining contact level - Based on Katie's coding
noncontactsport <- c("Badminton", "Biking", "Croquet", "Golf", "Horseback riding",
                     "Jump rope", "Rock climbing", "Rowing crew",
                     "Ski","Swim", "Tennis", "Track/cross country")
lowcontactsport <- c("Baseball", "Cheer","Dance", "Dodgeball",
                     "Fencing", "Field hockey", "Figure skating",
                     "Gymnastics/acrobatics", "Kickball", "Softball", "Volleyball")
highcontactsport <- c("Basketball", "Football", "Hockey", "Lacrosse",
                      "Martial arts", "Soccer", "Wrestling")


## transforming data
# 3 rows for every students since students were allowed to select up to 3 sports
# This step is interim and should not be used for analysis (ex: students listed same sport multiple times)
# use sportspattern instead, which is also in long format
sportslong <- ds1 %>%
  mutate(across(c(Transgender:SexOr,Tx, SPORT_1, SPORT_2, SPORT_3), as_factor)) %>%
  mutate(across(contains("TIME"), as.character)) %>%
  gather(temp, value, SPORT_1:TIME_SPORT_3) %>%
  separate(temp, into = c("var", "Num"), sep = -2) %>%
  spread(var, value) %>%
  rename(Sport = SPORT, Years = TIME_SPORT) %>%
  mutate(Num = str_remove(Num, "_") %>% as.numeric(),
         Years = ifelse(Sport == "No sports", "0", Years) %>%
           as.numeric(),
         Contact = case_when(Sport == "No sports" ~ "No sport",
                             Sport %in% noncontactsport ~ "Non",
                             Sport %in% lowcontactsport ~ "Low",
                             Sport %in% highcontactsport ~ "High") %>%
           factor(., levels = c("No sport", "Non", "Low", "High")))


#### Pattern of Sports Participation ####

## Defining contact category based on participation pattern
# one row for each student x sport combo
# categorizing students by highest contact sport (Contact_Cat)
# "no sport" rows are also included unless student student listed a sport and no sport
sportspattern <- sportslong %>%
  filter(!is.na(Sport)) %>%
  group_by(isolated, StudentID, Contact, Sport) %>%
  summarize(n = n(), .groups = "drop") %>% # i.e., was the same sport listed multiple times?
  group_by(isolated, StudentID) %>%
  mutate(Sports_count = n()) %>% # i.e., how many sports were listed?
  filter(!(Sport == "No sports" & Sports_count > 1)) %>%  # removing 'no sports' cases for students who also listed a sport
  mutate(Contact_Cat = case_when("High" %in% Contact ~ "High",
                                 "Low" %in% Contact ~ "Low",
                                 "Non" %in% Contact ~ "Non",
                                 "No sport" %in% Contact ~ "No sport",
                                 TRUE ~ "What"),
         Sports_count = ifelse(Sport == "No sports", 0, Sports_count))

## Listed same sport multiple times with different years played - 66 students
# for these students, the average years played is calculated
multipleyears <- select(sportslong, StudentID, Sport, Years) %>%
  filter(!is.na(Sport)) %>% unique() %>%
  filter(StudentID %in% sportspattern[sportspattern$n > 1 & sportspattern$Sport != "No sports",]$StudentID) %>%
  group_by(StudentID, Sport) %>%
  summarize(Years = mean(Years, na.rm = TRUE))


## Adding Years variable to sportspattern
sportspattern <- select(sportslong, StudentID, Sport, Years) %>%
  filter(!is.na(Sport)) %>% unique() %>%
  filter(!(StudentID %in% multipleyears$StudentID & Sport %in% multipleyears$Sport)) %>% # removes the 87 multiple cases
  bind_rows(multipleyears) %>% # adds back 66 cases with averaged years
  right_join(sportspattern, by = c("StudentID", "Sport")) %>%
  select(isolated, Contact_Cat, Sports_count, StudentID, # student-level measures
         Sport, Contact, Years, n) # sport-level measures (ie. multiple per student)

## Most common sports participation patterns
commonpatterns <- sportspattern %>%
  mutate(temp = Sport) %>%
  select(StudentID, Sport, temp) %>%
  spread(Sport, temp) %>%
  unite(col = "Pattern", -StudentID, remove = TRUE, na.rm = TRUE) %>%
  group_by(Pattern) %>%
  summarize(n = n(), .groups = "drop") %>%
  arrange(desc(n))

## Total endorsements (student could be counted more than once)
# by sport
endorsedsports <- FreqProp(sportspattern[sportspattern$Sport != "No sports",]$Sport) %>%
  left_join(FreqProp(sportspattern[sportspattern$Sport != "No sports" &
                                     sportspattern$isolated == 1 & 
                                     !is.na(sportspattern$isolated),]$Sport),
            by = "x", suffix = c(".Ov", ".Not")) %>%
  left_join(FreqProp(sportspattern[sportspattern$Sport != "No sports" &
                                     sportspattern$isolated == 0 & 
                                     !is.na(sportspattern$isolated),]$Sport),
            by = "x") %>%
  rename(Sport = x) %>%
  arrange(desc(n.Ov)) %>% flextable() %>% autofit()

## histogram of years participated for each sport
SportYearsPlot <- sportspattern %>%
  filter(!is.na(Sport) & Sport != "No sports") %>%
  ggplot(aes(Years)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  facet_wrap(~Sport)


# by contact level (student could be counted more than once)
endorsedcontact <- FreqProp(sportspattern[sportspattern$Contact != "No sport",]$Contact, varname = "Contact Level") %>%
  filter(n != 0) %>% arrange(desc(n)) %>%
  flextable() %>% autofit()

## histogram of years participated by contact level
ContactYearsPlot <- sportspattern %>%
  filter(!is.na(Contact) & Contact != "No sport") %>% # 2101
  ggplot(aes(Years)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  facet_wrap(~Contact)

#### Combining Sports Variables for Regression ####

## Sport indicator, contact category indicator, years played in contact category, and number of sports played
# one row per student (wide format)
# years played averaged across all sports in contact category
# No_sport acts as reference group for both sport indicators and contact category dummies
# does not include the 1184 students with NAs for all 3 sports (see sportscountNA)
sportindicators <- sportspattern %>%
  mutate(n = 1) %>%
  select(isolated, StudentID, Sports_count, Contact_Cat, Sport, n) %>%
  spread(Sport, n) %>%              # sport indicator
  left_join(sportspattern %>%
              mutate(n = 1) %>%
              select(StudentID, Contact_Cat, n) %>%
              unique() %>%
              spread(Contact_Cat, n) %>%
              select(StudentID, High_Contact = High, Low_Contact = Low, Non_Contact = Non),
            by = "StudentID") %>%  # contact category indicator
  mutate(across(.cols = -StudentID, ~replace_na(., 0))) %>%
  left_join(sportspattern %>%
              filter(Contact == Contact_Cat) %>%
              group_by(StudentID) %>%
              summarize(Contact_Years = mean(Years, na.rm = TRUE)),
            by = "StudentID") %>%   # years played in contact category
  rename_with(~str_replace_all(., " ", "_")) %>%
  rename_with(~str_replace_all(., "/", "."))


# NAs for Sports
sportscountNA <- FreqProp(rowSums(is.na(ds1[, c("SPORT_1", "SPORT_2", "SPORT_3")])) == 3) %>%
  left_join(FreqProp(rowSums(is.na(ds1[ds1$isolated == 0 & !is.na(ds1$isolated), c("SPORT_1", "SPORT_2", "SPORT_3")])) == 3),
            by = "x", suffix = c(".Ov", ".Not")) %>%
  left_join(FreqProp(rowSums(is.na(ds1[ds1$isolated == 1 & !is.na(ds1$isolated), c("SPORT_1", "SPORT_2", "SPORT_3")])) == 3),
            by = "x") %>%
  mutate(Sports = ifelse(x == FALSE, "Total", "Missing"))


## Table of Sports count by Isolated
sportscounttab <- FreqProp(sportindicators$Sports_count) %>%
  left_join(FreqProp(sportindicators[sportindicators$isolated == 0 & !is.na(sportindicators$isolated),]$Sports_count),
            by = "x", suffix = c(".Ov", ".Not")) %>%
  left_join(FreqProp(sportindicators[sportindicators$isolated == 1 & !is.na(sportindicators$isolated),]$Sports_count),
            by = "x") %>%
  rename(Sports = x) %>% 
  bind_rows(select(sportscountNA, -x)) %>%
  flextable() %>% 
  hline(i = 4, border = officer::fp_border()) %>% autofit()


#### Exporting flextables ####
# save_as_docx(Demos = demotable, `Sports Count` = sportscounttab,
#              Endorsed = endorsedsports,
#              path = "Demo and Sports Tables.docx")


#### Joining Sports Variables to dataset ####
dsconsport <- ds1 %>%
  left_join(sportindicators, by = c("StudentID", "isolated"))


# -------- Scale Creation --------------------

## how we construct the outcome determines whether we use regression or SEM
# For regression, the outcome is the mean of the item scores (same for dismiss, AOD, and previous perp)
# - conduct CFAs for measurement validity evidence
# - check icc for whether an multilevel model is required
# - listwise and multiple imputation; not sure if there is a ML approach
# For sem, outcome is a latent variable (same for dismiss, AOD, and previous perp)
# - Still listwise and multiple imputation b/c we would use a WLSMV estimator
# TBH, regression seems to be sufficient.

Scales <- list(SH_Perp_Prior = paste0(rep(nocontactperpvars, each = 3), "_W", 1:3),
               SH_Perp_W4 = nocontactperpvars,
               SV_Perp_Prior = paste0(rep(contactperpvars, each = 3), "_W", 1:3),
               SV_Perp_W4 = contactperpvars,
               Substance_Use = substanceusevars,
               Dismissiveness = dismissvars)


## Response Frequencies
responsefreqs <- map(.x = Scales,
                     ~Get_ItemFreqs(dsconsport[.x], NAto0 = TRUE))

## Dichotomizing SH and SV items
dsconsport <- dsconsport %>%
  mutate(across(all_of(c(Scales$SH_Perp_Prior, Scales$SH_Perp_W4, Scales$SV_Perp_Prior, Scales$SV_Perp_W4)),
                ~case_when(as.numeric(.) >= 1 ~ 1,
                           TRUE ~ as.numeric(.))))


scale.scores <- map2_dfc(.x = Scales, .y = c("sum", "sum", "sum", "sum", "mean", "mean"),
                       ~scale_score(dsconsport, .x, type = .y,
                                    min.valid = 3)) # must answer at least 3 items to receive score

## joining scale scores to dataset and creating regression variables
dsconscales <- dsconsport %>%
  bind_cols(scale.scores) %>%
  mutate(Contact_Cat = factor(Contact_Cat, levels = c("High", "Low", "Non", "No sport")),
         Female = ifelse(Gender == 1, 1, 0),        # Male is the reference group
         Other = case_when(is.na(Gender) ~ NA_real_,
                           Gender %in% c(3, 4) ~ 1,
                           TRUE ~ 0),
         Non_Male = ifelse(Gender != 2, 1, 0),
         Latinx = ifelse(Race == 4, 1, 0),          # White is the reference group
         Non_White = case_when(is.na(Race) ~ NA_real_,
                               Race %in% c(1, 2, 3, 5, 6, 8) ~ 1,
                               TRUE ~ 0),
         POC = ifelse(Race != 7, 1, 0),
         LGBQ = ifelse(SexOr != 5, 1, 0), # Heterosexual is reference group
         Multisport = ifelse(Sports_count > 1, 1, 0),
         Tx = as.numeric(Tx) - 1,          # converting from 1/2 to 0/1 
         Age = as.numeric(Age)) %>%            # 0 = 12 or younger to 6 = 18 or older
         # betweennessLog = log(betweenness)) %>%
  mutate(across(.cols = all_of(adulttypevars), ~ifelse(.x > 0, 1, 0), .names = "{col}_di")) %>%
  mutate(across(.cols = c("SH_Perp_Prior", "SH_Perp_W4","SV_Perp_Prior", "SV_Perp_W4"),
                ~ifelse(.x > 0, 1, 0), .names = "{col}_di")) %>%
  mutate(across(.cols = High_Contact:Non_Contact, ~Female*., .names = "{col}xFemale")) %>%
  mutate(across(.cols = High_Contact:Non_Contact, ~Dismissiveness*., .names = "{col}xDismiss")) %>%
  mutate(across(.cols = High_Contact:Non_Contact, ~Substance_Use*., .names = "{col}xSU")) %>%
  mutate(across(.cols = High_Contact:Non_Contact, ~SH_Perp_Prior_di*., .names = "{col}xSH_Prior")) %>%
  mutate(across(.cols = High_Contact:Non_Contact, ~SV_Perp_Prior_di*., .names = "{col}xSV_Prior"))

dsconscales <- dsconscales %>%
  mutate(ppfr_SH_Prior = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "SH_Perp_Prior_di", outdegree = .$outdegree),
         ppfr_SV_Prior = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "SV_Perp_Prior_di", outdegree = .$outdegree),
         ppfr_SH_W4 = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "SH_Perp_W4_di", outdegree = .$outdegree),
         ppfr_SV_W4 = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "SV_Perp_W4_di", outdegree = .$outdegree),
         avefr_Dismissiveness = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "Dismissiveness", outdegree = .$outdegree),
         avefr_SU = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "Substance_Use", outdegree = .$outdegree),
         ppfr_SH_Prior = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "SH_Perp_Prior_di", outdegree = .$outdegree),
         ppfr_No_Sports = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "No_sports", outdegree = .$outdegree),
         ppfr_Non_Contact = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                variable = "Non_Contact", outdegree = .$outdegree),
         ppfr_Low_Contact = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                   variable = "Low_Contact", outdegree = .$outdegree),
         ppfr_High_Contact = create_friend_variable(., id = "StudentID", paste0("Friend", 1:7, "W4"), matrix = FALSE,
                                                   variable = "High_Contact", outdegree = .$outdegree))

labelled::var_label(dsconscales) <- list(ppfr_SH_Prior = "Proportion of friendship nominations that were sexual harassment perpetrators - Waves 1-3",
                                         ppfr_SV_Prior = "Proportion of friendship nominations that were sexual violence perpetrators - Waves 1-3",
                                         ppfr_SH_W4 = "Proportion of friendship nominations that were sexual harassment perpetrators - Wave 4",
                                         ppfr_SV_W4 = "Proportion of friendship nominations that were sexual violence perpetrators - Wave 4",
                                         avefr_Dismissiveness = "friends average dismissiveness score",
                                         avefr_SU = "friends average substance use score",
                                         ppfr_No_Sports = "Proportion of friendship nominations that did not participate in sports",
                                         ppfr_Non_Contact = "Proportion of friendship nominations that were non-contact sport participants",
                                         ppfr_Low_Contact = "Proportion of friendship nominations that were low-contact sport participants",
                                         ppfr_High_Contact = "Proportion of friendship nominations that were high-contact sport participants")

# lapply(c(adulttypevars), function(x) table(dsconscales[[paste0(x, "_di")]], dsconscales[[x]], useNA = "always"))
# lapply(c("SH_Perp_Prior", "SH_Perp_W4","SV_Perp_Prior", "SV_Perp_W4"),
#        function(x) table(dsconscales[[paste0(x, "_di")]], dsconscales[[x]], useNA = "always"))

# ---- Frequencies and Descriptive Statistics ------------------------

#### Sample Characteristics ####

sample.chars <- dsconscales %>%
  select(all_of(demovars)) %>%
  mutate(across(all_of(demovars[-c(1,6,7)]), ~as_factor(.x) %>% fct_drop())) %>%
  mutate(Gender = fct_recode(Gender, Other = "Multiple"),
         Race = fct_recode(Race, Multiracial = "Multiple", Asian = "Islander"),
         Age = Age + 12,
         Transgender = as.numeric(Transgender)) %>%
  tbl_summary(label = list(Transgender ~ "Transgender"),
              type = list(c(Age) ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              missing = "no") %>%
  as_flex_table()
  

## scale scores by contact level
descrips.contact <- dsconscales %>%
  select(Contact_Cat, all_of(demovars)) %>%
  mutate(across(all_of(demovars[-c(1,6,7)]), ~as_factor(.x) %>% fct_drop())) %>%
  mutate(Gender = fct_recode(Gender, Other = "Multiple"),
         Race = fct_recode(Race, Multiracial = "Multiple", Asian = "Islander"),
         Age = Age + 12,
         Transgender = as.numeric(Transgender),
         Contact_Cat = fct_explicit_na(Contact_Cat, na_level = "Missing")) %>%
  tbl_summary(by = "Contact_Cat",
              label = list(Transgender ~ "Transgender"),
              type = list(c(Age) ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              missing = "no") %>%
  # add_p() %>%  #include = all_continuous()
  add_overall() %>%
  as_flex_table()


## scale scores by isolated
descrips.iso <- dsconscales %>%
  select(all_of(names(Scales)), all_of(demovars), Isolated = isolated) %>%
  mutate(across(all_of(demovars[-c(1,6,7)]), ~as_factor(.x) %>% fct_drop())) %>%
  mutate(Gender = fct_recode(Gender, Other = "Multiple"),
         Race = fct_recode(Race, Multiracial = "Multiple", Asian = "Islander"),
         Age = Age + 12,
         Transgender = as.numeric(Transgender)) %>%
  tbl_summary(by = "Isolated",
              label = list(Transgender ~ "Transgender"),
              type = list(c(SH_Perp_W4, Age) ~ "continuous"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{n} ({p}%)"),
              missing = "no") %>%
  add_p() %>%  #include = all_continuous()
  add_overall() %>%
  as_flex_table()

#########################################################

#### Creating Non-isolated student dataset for analysis 2 ####

dsnoniso <- dsconscales %>%
  filter(isolated == 0) %>% 
  mutate(across(starts_with("ppfr"), ~.x*100)) # puts proportion variables on 0-100 scale; coefficient interpretation becomes a 1% increase in X results in beta change in Y

# ----- Saving and Exporting -----------------

## saving objects
save(dsconscales, dsnoniso, sample.chars,
     descrips.contact, descrips.iso, Scales, responsefreqs,
     noncontactsport, lowcontactsport, highcontactsport,
     sportspattern, commonpatterns, sportscounttab,
     endorsedsports, endorsedcontact, SportYearsPlot, ContactYearsPlot,
     file = "Output/Prep_and_Descriptives.RData")
