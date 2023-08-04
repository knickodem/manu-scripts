#########################################################
#                                                       #
#  IES Goal 2: Disability Anti-Bullying (DIAL) Training #      
#            Psychometric Investigation                 #
#                                                       #
#########################################################

source("Scripts/IG2_PnF.R")

# ------   Initial Data Import and Cleaning --------------------

## Importing data
dial.orig <- haven::read_sav("00 Data/COMBINED T1_T2/00 - IES_ALL_DATA_3-6-23.sav")

item.types <- purrr::map_dfr(.x = names(dial.orig),
                             ~data.frame(Variable = .x,
                                         Classes = paste0(class(dial.orig[[.x]]), collapse = ", ")))

## Identifying names of time invariant variables
idplus.vars <- dial.orig %>% select(-matches("W[0-9]$")) %>% names()
# Placing at front of dataset if not already - should have been done in 0-Code_to_merge_all_data.R
# dial.orig <- dial.orig %>%
#   select(all_of(idplus.vars), everything())

#### Renaming columns to match original construct and reverse scoring ####

## See 'Scale Definitions' section below for the citations which informed the final names and which variables to reverse for scoring
rev.items <- c("T_JOB_SATISFACTION", "TR_ACADEMIC_BEHAVIOR_4", "TR_ACADEMIC_BEHAVIOR_5",
               "TR_SOCIAL_BEHAVIOR_1", "TR_SOCIAL_BEHAVIOR_3", "TR_SOCIAL_BEHAVIOR_4", "TR_SOCIAL_BEHAVIOR_6",
               "TR_EMOTIONAL_BEHAVIOR_1", "TR_EMOTIONAL_BEHAVIOR_2", "TR_EMOTIONAL_BEHAVIOR_5", "TR_EMOTIONAL_BEHAVIOR_6", "TR_EMOTIONAL_BEHAVIOR_7")
dial.rn <- dial.orig %>%
  rename_with(.cols = starts_with("TR_SEL_SOCIAL_AWARENESS_"), .fn = ~stringr::str_replace(., "TR_SEL_SOCIAL_AWARENESS_", "TR_SOCIAL_COGNITION_")) %>%
  rename_with(.cols = starts_with("TR_STUDENT_TEACHER_RELATION_"), .fn = ~stringr::str_replace(., "TR_STUDENT_TEACHER_RELATION_", "TR_STUDENT_TEACHER_CONFLICT_")) %>%
  rename_with(.cols = starts_with("TR_ACADEMIC_PERFORMANCE_"), .fn = ~stringr::str_replace(., "TR_ACADEMIC_PERFORMANCE_", "TR_ACADEMIC_COMP_")) %>%
  rename_with(.cols = starts_with("T_STUD_BULLY_ATTITUDES_"), .fn = ~stringr::str_replace(., "T_STUD_BULLY_ATTITUDES_", "T_BULLY_ATTITUDES_")) %>%
  rename_with(.cols = starts_with("T_SCHOOL_COMMIT_BULLY_PREV_"), .fn = ~stringr::str_replace(., "T_SCHOOL_COMMIT_BULLY_PREV_", "T_SCHOOL_COMMITMENT_")) %>%
  rename_with(.cols = starts_with("T_IDENTITY_BASED_BULLY_"), .fn = ~stringr::str_replace(., "T_IDENTITY_BASED_BULLY_", "T_BULLYING_PREVALENCE_")) %>%
  rename_with(.cols = starts_with("T_BULLY_PREV_PROF_DEV_"), .fn = ~stringr::str_replace(., "T_BULLY_PREV_PROF_DEV_", "T_PREVENTION_PD_")) %>%
  rename_with(.cols = starts_with("T_TEACHER_STAFF_ATT_"), .fn = ~stringr::str_replace(., "T_TEACHER_STAFF_ATT_", "T_POSITIVE_INTERACT_")) %>%
  rename_with(.cols = starts_with("T_POS_SCHOOL_CLIM_"), .fn = ~stringr::str_replace(., "T_POS_SCHOOL_CLIM_", "T_POSITIVE_INTERACT_7_")) %>%
  rename_with(.cols = starts_with("ST_TEACHER_STUDENT_RELATION_"), .fn = ~stringr::str_replace(., "ST_TEACHER_STUDENT_RELATION_", "ST_CLASSROOM_EXPERIENCE_")) %>%
  rename_with(.cols = starts_with("ST_TEACHER_INTERVENTION_"), .fn = ~stringr::str_replace(., "ST_TEACHER_INTERVENTION_", "ST_STAFF_INTERVENE_")) %>%
  rename_with(.cols = starts_with("ST_STUDENT_INTERVENTION_"), .fn = ~stringr::str_replace(., "ST_STUDENT_INTERVENTION_", "ST_STUDENT_INTERVENE_")) %>%
  mutate(across(all_of(paste0(rev.items, "_W", rep(1:2, each = length(rev.items)))),
                ~max(., na.rm = TRUE) - ., .names = "{col}_r")) %>%          # creating reverse coded items
  mutate(across(where(is.character), ~na_if(., "")))                         # converting blanks to NA


#### Identifying instances where student had multiple reports and randomly selecting one to keep ####

# ## checking validity of MULTIPLE_INFORMANTS variable - seems good
# mr.check <- dial.rn %>%
#   group_by(STUDENT_ID, TEACHER_ID) %>%
#   summarize(n_rows = n(), .groups = "drop_last") %>%
#   mutate(mr = n())
# mr.check2 <-inner_join(mr.check, select(dial.rn, STUDENT_ID, TEACHER_ID, mi = MULTIPLE_INFORMANTS)) %>%
#   mutate(mi = as.numeric(mi),
#          diff = mr - mi)

mr <- dial.rn %>%
  filter(MULTIPLE_INFORMANTS > 1) %>%
  select(STUDY_ID, STUDENT_ID, TEACHER_ID, MULTIPLE_INFORMANTS, TR_WAVES_PRESENT) %>% # 142 cases
  mutate(both_waves = ifelse(TR_WAVES_PRESENT == 3, 1, 0))
mr$TEACHER_ID %>% unique() %>% length() #13 teachers
mr$STUDENT_ID %>% unique() %>% length() #67 students

## How many students had teachers report 1 wave, both waves
tr_waves <- mr %>%
  group_by(STUDENT_ID, both_waves) %>%
  summarize(num_teachers = n()) %>%    # number of teachers who reported both or one wave
  mutate(tot_teachers = sum(num_teachers)) # total number of teachers who reported for a student

## How many students had all their teachers report both waves? Need to randomly select 1 from this group
bw2 <- tr_waves[tr_waves$both_waves == 1 & tr_waves$num_teachers == tr_waves$tot_teachers,]$STUDENT_ID # length(bw2) = length(unique(bw2)) = 36

## How many students had all their teachers report only 1 wave? Keep both teachers for this group
ow2 <- tr_waves[tr_waves$both_waves == 0 & tr_waves$num_teachers == tr_waves$tot_teachers,]$STUDENT_ID # length(ow2) = 0

## How many students had 1 (or more) teachers report both waves and 1 (or more) teachers report only 1 wave? Keep the teachers who reported both
split2 <- tr_waves[tr_waves$both_waves == 1 & tr_waves$num_teachers != tr_waves$tot_teachers,]$STUDENT_ID # length(split2) = length(unique(split2)) = 31
# split0 <- tr_waves[tr_waves$both_waves == 0 & tr_waves$num_teachers != tr_waves$tot_teachers,]$STUDENT_ID # length(split0) = length(unique(split0)) = 31
# all.equal(split0,split2)
table(bw2 %in% split2) # all students in either bw2 or split2

## Identifying which cases to include and exclude
split.include <- mr[mr$STUDENT_ID %in% split2 & mr$both_waves == 1,]
split.exclude <- mr[mr$STUDENT_ID %in% split2 & mr$both_waves == 0,]
waldo::compare(unique(split.include$TEACHER_ID), unique(split.exclude$TEACHER_ID))

## From bw2 students, randomly selecting 1 teacher
set.seed(48112) # replicates the random selection
random.include <- mr %>%
  filter(STUDENT_ID %in% bw2) %>%
  group_by(STUDENT_ID) %>%
  slice_sample(n = 1, replace = FALSE)

# testing that randomization produced same cases as previously
testthat::test_that("replicate randomization", {
  testthat::expect_equal(as.numeric(random.include$STUDY_ID[c(1:5, 20:24)]),
                         c(10239, 10274, 10275, 10276, 10277, 10250, 10283, 10208, 10209, 10210))
})

# Identifying cases to exclude based on random selection
random.exclude <- mr[mr$STUDENT_ID %in% bw2 & !mr$STUDY_ID %in% random.include$STUDY_ID,]
# Which teachers were randomly included and excluded?
waldo::compare(sort(unique(random.include$TEACHER_ID)), sort(unique(random.exclude$TEACHER_ID)))

## Removing cases so there is only 1 teacher report per student
dial.rn1 <- dial.rn %>%
  filter(!STUDY_ID %in% c(split.exclude$STUDY_ID, random.exclude$STUDY_ID)) %>% #nrow(dial.orig) - nrow(dial.rn) = 75 cases; nrow(mr) - 75 = length(unique(mr$STUDENT_ID))
  filter(!is.na(TEACHER_ID))  # removes the 25 students with only administrative data

nostudents <- dial.rn1[is.na(dial.rn1$STUDENT_ID),]$TEACHER_ID # 5 teachers with no students in the dataset

#### Transforming data to long format ####
dial.long <- bind_rows(LongWave(data = dial.rn1, wave = 1, all_of(idplus.vars)),
                       LongWave(data = dial.rn1, wave = 2, all_of(idplus.vars))) %>%
  select(Wave, all_of(idplus.vars), starts_with("TR_"), starts_with("ST_"), starts_with("T_"), everything())
# warning is ignorable (label is "don't" in w1 and "dont" in w2; uses w1 in combined dataset)

##################################################################################################


# -----  Scale Definitions  --------------

# surveys stored here: https://app.box.com/folder/124792076623?s=um0r4ws2l21pbz3rkp7j43nlnjo084np
# THE ISSUE: Many of the scales were shortened and/or adapted in ways that the original structure is obscured or irrelevant
#            Even when existing scale was used, little psychometric evidence exists for their use
# Below I attempt to document where the items came from and when they have been adapted

#### Teacher student report scales ####
# SAEBRS (Kilgus et al. 2013; von der Embse et al., 2016) - higher scores indicate better behavior
socbhv.items <- paste0("TR_SOCIAL_BEHAVIOR_", 1:6)    # reverse code 1, 3, 4, and 6
acabhv.items <- paste0("TR_ACADEMIC_BEHAVIOR_", 1:6)  # reverse code 4 and 5
emobhv.items <- paste0("TR_EMOTIONAL_BEHAVIOR_", 1:7) # reverse code 1, 2, 5, 6, 7
saebrs.items <- c(socbhv.items, acabhv.items, emobhv.items)
saebrs.to.r <- c(1,3,4,6,10,11,13,14,17,18,19) # items to reverse
saebrs.r.items <- saebrs.items
saebrs.r.items[saebrs.to.r] <- paste0(saebrs.r.items[saebrs.to.r], "_r") # scale with reversed items
# CLDQ (Willcutt et al., 2011) - higher scores indicate greater difficulties (social cognition and social anxiety)
# investigate as social functioning or 2 factor
soccog.items <- paste0("TR_SOCIAL_COGNITION_", 1:4)
socanx.items <- paste0("TR_SOCIAL_ANXIETY_", 1:3)
cldq.items <- c(soccog.items, socanx.items) 
# STRS-Short Form; Conflict subscale (Pianta, 1992) - higher score indicates worse relationship
strscon.items <- paste0("TR_STUDENT_TEACHER_CONFLICT_", 1:7)
# TASB (Brown et al., 2011) - higher score indicates higher bullying/academics
# investigate bullying as 1- or 2-factor
physbly.items <- paste0("TR_BULLY_PERP_", 1:4)
nophbly.items <- paste0("TR_BULLY_PERP_", 5:8)
tasbbly.items <- c(physbly.items, nophbly.items)
tasbac.items <- paste0("TR_ACADEMIC_COMP_", 1:4) # academic competency

## All items - scales informed by psychometrics in subsequent script
tst.items <- list(Behavior_Risk = saebrs.r.items,
                 Social_Cognition = soccog.items,
                 Social_Anxiety = socanx.items,
                 Conflict = strscon.items,
                 Bullying = tasbbly.items,
                 Academic_Competency = tasbac.items)


#### Teacher self-report scales ####
## scale adaptation notes: https://app.box.com/file/808491865422?s=4xbs7y8ji7q70xhoojkywmb915j026qz

# Ohio State Teacher Efficacy scale (Tschannen-Moran & Woolfolk Hoy, 2001) - higher score indicates greater confidence
# short form is 12 items, long form is 24
# student engagement (2, 3, 4, 11); classroom management (1, 6, 7, 8); instructional strategies (5, 9, 10, 12) # investigate 1 and 3-factors
ostes.items <- paste0("T_TEACHER_EFFICACY_", 1:12)
# Special Needs confidence scale - higher scores indicate greater confidence
# adapted from Jung, Cho, & Ambrosetti, 2011 (13 items), which was shortened from LePage, Lewis, & Casella, 1995 (22 items)
sncs.items <- paste0("T_SPED_CONFIDENCE_", 1:11) 
# SSBQ (Troop-Gordon and Ladd, 2015) used 10 of 13 items (Kochenderfer-Ladd & Pelletier, 2008 only used 12)
# originally designed as 4 factor structure - higher scores indicate poor/maladaptive views on bullying
# normative beliefs (1 - 3 changing fighting to bullying and dropping 1 item), advocate avoidance (4, 5 [drop 1 item]), advocate assertion (6 - 8 [all items]), dimissive beliefs (9, 10 [both adapted and drop 1 item])
# The 4 factors had positive correlations in (Troop-Gordon and Ladd, 2015)
ssbq.items <- paste0("T_BULLY_ATTITUDES_", 1:10)
# CMPQ (Troop-Gordon and Ladd, 2015) used 14 of 26 (25 in Kochenderfer-Ladd & Pelletier, 2008) - higher scores indicate better management?
# The 6 factors are not highly correlated in T-G & L (2015) so I'm not sure how these are expected to hang together. Additionally,
# Table 1 indicates negative corrs of Coping ~~ Contact + Sep + Punish; Ignore ~~ Contact + Punish; Assert only corr w/ Ignore
cmpq.items <- c(paste0("T_CLASS_MANAG_CONTACT_PARENTS_", 1:4), # Contact Parents
                paste0("T_CLASS_MANAG_SEP_STUD_", 1:2), # separate students
                paste0("T_CLASS_MANAG_IGNORE_", 1:3), # advocate avoidance
                paste0("T_CLASS_MANAG_ASSERT_", 1:2), # advise assertion
                paste0("T_CLASS_MANAG_PUNISH_AGG_", 1:2), # punish aggressors
                "T_CLASS_MANAG_INDEP_COPING") # independent coping
# MOAQ (Camman et al., 1979) - higher score indicates greater satisfaction, but we will reverse it to dissatisfaction
moaq.items <- c("T_JOB_SATISFACTION", "T_INTENT_LEAVE_1", "T_INTENT_LEAVE_2") # reverse satisfaction variable for scale
moaq.r.items <- moaq.items
moaq.r.items[1] <- paste0(moaq.r.items[1], "_r") # scale with reversed items
# Colorado Trust Bullying Prevention Staff Survey - higher score indicates higher of thing
trsint.items <- paste0("T_STUDENTS_INTERVENE_", 1:5) # Students at your school ...
trtint.items <- paste0("T_STAFF_INTERVENE_", 1:5)    # Staff at your school ....
aggprob.items <- paste0("T_AGGRESSION_PROBLEM_", 1:5) # How much of a problem is...
schcom.items <- paste0("T_SCHOOL_COMMITMENT_", 1:10) # Think about programs and policies at your school (2 items added [PYD and  cultural comp])
stfatt.items <- paste0("T_POSITIVE_INTERACT_", 1:7) # Think about how strongly ... (excludes items starting with "Students in this school")
ctbpt.items <- c(trsint.items, trtint.items, aggprob.items, schcom.items, stfatt.items)
# no clue where these came from: "SCHOOL_GENDER_EQUAL_1", "SCHOOL_GENDER_EQUAL_2" - A
# Bullying Prevalence from Authoritative School Climate Teacher Version (Cornell, 2015) - higher scores indicate more bullying
# Also see Huang, Cornell et al (2015)
blyprev.items <- paste0("T_BULLYING_PREVALENCE_", 1:7) # Items 1, 2, & 6 from Cornell, 2015, the others are adapted.
# BPS (Kennedy et al., 2012) - higher scores indicate more positive attitude toward bully prevention/greater confidence
# 2 items dropped (same as item 2, but referred to middle and high school) with 2 confidence items added
# Kennedy claims original 10 items yielded 4 factors and directs us to a figure that doesn't exist
blypercep.items <- paste0("T_PREVENTION_PD_", 1:9)
blyconf.items <- paste0("T_CONFIDENCE_PARENTS_BULLY_", 1:4) # 2 items added for child with disabilities
bps.items <- c(blypercep.items, blyconf.items)

## All items - scales informed by psychometrics in subsequent script
t.items <- list(Teacher_Efficacy = ostes.items,
                Student_Engagement = ostes.items[c(2,3,4,11)],
                Classroom_Management = ostes.items[c(1,6,7,8)],
                Instructional_Strategies = ostes.items[c(5,9,10,12)],
                SPED_Confidence = sncs.items,
                # Bullying_Attitudes = ssbq.items,
                Normative_Attitude = ssbq.items[1:3],
                Avoidance_Attitude = ssbq.items[4:5],
                Maladaptive_Attitude = ssbq.items[6:10],
                Job_Dissatisfaction = moaq.r.items,
                CMPQ = cmpq.items,
                Students_Intervene = trsint.items,
                Staff_Intervene = trtint.items,
                Aggression_Problems = aggprob.items,
                School_Commitment = schcom.items,
                Positive_Interactions = stfatt.items,
                Bullying_Prevalence = blyprev.items,
                Prevention_PD = bps.items[c(1,2,5,6,7)],
                Confidence = c(bps.items[8:9], blyconf.items))



#### Student self-report scales ####
# Student Perception Survey (Colorado Education Initiative, 2013) - higher scores indicate more positive classroom experiences
# classroom community (1, 6, 8, 9), student centered environment (2, 4, 5, 7), classroom management (3)
sps.items <- paste0("ST_CLASSROOM_EXPERIENCE_", 1:9)
# University of Illinois Bully, Fight, and Victimization Scales (Espelage & Holt, 2001) - Higher score indicates more bullying/fighting/victimization
stbp.items <- paste0("ST_BULLYING_PERP_", 1:9)
fight.items <- paste0("ST_FIGHT_", 1:5)
peervict.items <- paste0("ST_PEER_VICT_", 1:4) # also present in the getting along with others section of the Colorado Trust Survey
# Colorado Trust Bullying Prevention Student Survey - higher scores indicate higher on the thing
gawo.items <- paste0("ST_PROSOCIAL_", 1:4) # the positive valence items from the getting along with others section (item 1 is adapted)
srtint.items <- paste0("ST_STAFF_INTERVENE_", 1:5) # Teachers and Staff at your school would help out if... (adds "if hurt or upset" item; the other 4 items match teacher version)
srsint.items <- paste0("ST_STUDENT_INTERVENE_", 1:5) # Students at your school would help out if... (adds "if hurt or upset" item; the other 4 items match teacher version)
classclim.items <- paste0("ST_CLASSROOM_CLIMATE_", 1:6) # adapted from My School section (6 of 16 items)
ctbps.items <- c(gawo.items, srtint.items, srsint.items, classclim.items)

st.items <- list(Classroom_Experience = sps.items[-3],
                 Bullying = stbp.items,
                 Fight = fight.items,
                 Peer_Victimization = peervict.items,
                 Prosocial = gawo.items,
                 Staff_Intervene = srtint.items,
                 Student_Intervene = srsint.items,
                 Class_Climate = classclim.items)

#########################################################

# ----  Item Coding and Frequencies  ----------

## Note. Coding and examining frequencies is an iterative process
## Collapsed variables will be used for CFAs and investigating dimensionality

#### Teacher student report scales ####

dial.tst <- dial.long %>%
  select(Wave, all_of(idplus.vars), starts_with("TR_")) %>%  # dropping teacher and student self-report variables
  unique() %>%
  filter(!is.na(TR_DATE)) %>% # dropping empty rows
  mutate(across(all_of(c(saebrs.items, saebrs.r.items, cldq.items,   
                         strscon.items, tasbbly.items, tasbac.items)),
                ~haven::zap_labels(.))) %>%                            # converting items from haven_labelled to numeric
  mutate(across(all_of(saebrs.r.items), ~if_else(. == 0, 1, .), .names = "{col}_c")) %>% # collapsing 0 & 1 categories
  mutate(across(all_of(cldq.items), ~if_else(. == 4, 3, .), .names = "{col}_c")) %>%     # collapsing 4 & 3
  mutate(across(all_of(tasbbly.items), ~if_else(. == 4, 3, .), .names = "{col}_c")) # collapsing 4 & 3


tst.freqs <- map(.x = list(SAEBRS = saebrs.r.items, CLDQ = cldq.items,
                           `STRS-Conflict` = strscon.items, `TASB-Bully` = tasbbly.items,
                           `TASB-Academic` = tasbac.items),
                 ~dial.tst %>% select(all_of(.x)) %>%
                   get_item_freqs(NAto0 = TRUE) %>%
                   mutate(Item = factor(Item, levels = .x)) %>%
                   arrange(Item))
# lapply(tst.freqs, View)

## Notes (written for W1; still apply for combined time points):
# - SAEBRS: Probs collapse 0 & 1, particularly emotional behavior
# - CLDQ: Probs collapse 4 & 3
# - STRS-Conflict: Probs okay, maybe collapse 3&2
# - TASB-Bully: Definitely collapse 4 & 3, maybe even 2
# - TASB-Academic: Good!

#### Teacher self-report scales ####

dial.tslf <- dial.long %>%
  select(Wave, TEACHER_ID, CONDITION, SCHOOL_ID, T_WAVES_PRESENT, starts_with("T_")) %>%  # keeping only teacher ID and self-report items
  unique() %>% # teacher rows were repeated in combined dataset, so dropping duplicates here
  filter(!is.na(T_DATE)) %>% # dropping empty rows
  mutate(across(all_of(c(ostes.items, sncs.items, ssbq.items,
                         cmpq.items, moaq.items, ctbpt.items,
                         blyprev.items, bps.items)),
                ~haven::zap_labels(.))) %>%                            # converting items from haven_labelled to numeric
  mutate(across(all_of(ostes.items), ~if_else(. == 3, 1, 0), .names = "{col}_c")) %>% # dichotmoized to 3 = 1; 0, 1, 2 = 0
  mutate(across(all_of(sncs.items), ~if_else(. == 0, 1, .), .names = "{col}_c")) %>%     # collapsing 0 & 1
  mutate(across(all_of(ssbq.items), ~if_else(. == 3, 2, .), .names = "{col}_c")) %>%     # collapsing 3 & 2
  mutate(across(all_of(cmpq.items[c(1:6, 12:13)]), ~if_else(. == 0, 1, .), .names = "{col}_c")) %>%     # collapsing 0 & 1
  mutate(across(all_of(c(moaq.items[-1], "T_JOB_SATISFACTION_r")), ~if_else(. == 3, 2, .), .names = "{col}_c")) %>%     # collapsing 3 & 2
  mutate(across(all_of(ctbpt.items[11:15]), ~if_else(. == 3, 2, .), .names = "{col}_c")) %>%     # collapsing 3 & 2
  mutate(across(all_of(ctbpt.items[c(1:10, 16:32)]), ~if_else(. == 0, 1, .), .names = "{col}_c")) %>%     # collapsing 0 & 1
  mutate(across(all_of(blyprev.items), ~if_else(. == 3, 2, .), .names = "{col}_c")) %>%     # collapsing 3 & 2
  mutate(across(all_of(bps.items), ~if_else(. == 0, 1, .), .names = "{col}_c"))    # collapsing 0 & 1


tslf.freqs <- map(.x = list(OSTES = ostes.items, SNCS = sncs.items,
                            SSBQ = ssbq.items, CMPQ = cmpq.items,
                            MOAQ = moaq.items, CTBP = ctbpt.items,
                            Bully_Prevelance = blyprev.items, BPS = bps.items),
                  ~dial.tslf %>% select(all_of(.x)) %>%
                    get_item_freqs(NAto0 = TRUE) %>%
                    mutate(Item = factor(Item, levels = .x)) %>%
                    arrange(Item))

# lapply(tslf.freqs, View)

## Notes (written for W1; still apply for combined time points):
# - OSTES: Collapse 0, 1 & 2 (i.e. dichotomize to 3 or not)
# - SNCS: Collapse 0 & 1, probs 2
# - SSBQ: Collapse 3 & 2
# - CMPQ: Collapse 0 & 1 though may vary by scale
# - MOAQ: Collapse 3 & 2
# - CTBP: Collapse 0 & 1 (3 & 2 for aggression)
# - Bully: Collapse 3 & 2
# - BPS: Collapse 0 & 1 

#### Student self-report scales ####

dial.sslf <- dial.long %>%
  select(Wave, STUDENT_ID, CONDITION, SCHOOL_ID, MULTIPLE_INFORMANTS, ST_WAVES_PRESENT, starts_with("ST_")) %>%  # keeping only student ID and self-report items
  unique() %>%       # Some students were rated by multiple teachers and will have redundant rows here
  filter(!is.na(ST_DATE)) %>% # dropping empty rows
  mutate(across(all_of(c(sps.items, stbp.items, fight.items,
                         peervict.items, ctbps.items)),
                ~haven::zap_labels(.))) %>%                            # converting items from haven_labelled to numeric
  mutate(across(all_of(stbp.items), ~if_else(. == 4, 3, .), .names = "{col}_c")) %>%  # collapsing 4 & 3
  mutate(across(all_of(fight.items), ~if_else(. == 4, 3, .), .names = "{col}_c"))  # collapsing 4 & 3

sslf.freqs <- map(.x = list(SPS = sps.items, Bully_Perp = stbp.items,
                            Fight = fight.items, Peer_Victim = peervict.items,
                            CTBP = ctbps.items),
                  ~dial.sslf %>% select(all_of(.x)) %>%
                    get_item_freqs(NAto0 = TRUE) %>%
                    mutate(Item = factor(Item, levels = .x)) %>%
                    arrange(Item))

table(dial.sslf$MULTIPLE_INFORMANTS)

# lapply(sslf.freqs, View)

## Notes (written for W1; still apply for combined time points):
# - SPS: Good!
# - Bully: Maybe collapse 3 & 4 due to low 3s
# - Fight: Maybe collapse 3 & 4 due to low 3s
# - PV: Good!
# - CTBP: Good!

###################################################
####     Investigating Scale Dimensionality    ####

# NOTE: Analyses ignore the clustering of time/students/teachers

# ----- Teacher student report scales ----------

####       SAEBRS       ####

# all collapsed items
saebrs.c.items <- paste0(saebrs.r.items, "_c")

# check power
find_k(variables = select(dial.tst, all_of(saebrs.c.items)), m = 3)
524/119

saebrs.cfas <- list(original3 = paste0("social =~ ", paste(saebrs.c.items[1:6], collapse = " + "),
                                       "\nacademic =~ ", paste(saebrs.c.items[7:12], collapse = " + "),
                                       "\nemotional =~ ", paste(saebrs.c.items[13:19], collapse = " + ")),
                    bifactor = paste0("total =~ ", paste(saebrs.c.items, collapse = " + "),
                                      "\nsocial =~ ", paste(saebrs.c.items[1:6], collapse = " + "),
                                      "\nacademic =~ ", paste(saebrs.c.items[7:12], collapse = " + "),
                                      "\nemotional =~ ", paste(saebrs.c.items[13:19], collapse = " + "),
                                      "\ntotal ~~ 0*social + 0*academic + 0*emotional\nsocial ~~ 0*academic + 0*emotional\nacademic ~~ 0*emotional"))


## Investigating 3 factor model
# run kfa
saebrs.kfa <- kfa(data = dial.tst, variables = saebrs.c.items,
                  k = 4, # number of folds
                  m = 4, # maximum number of factors to test
                  ordered = TRUE, # treat variables as ordered
                  missing = "listwise",
                  custom.cfas = saebrs.cfas)

# generate report
kfa_report(saebrs.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/SAEBRS_Psychometric_Report_T1T2",
           report.title = "SAEBRS Psychometric Report - Time 1 and 2")

## CONCLUSION:
# Bifactor model only one with sufficient fit (.98, .08, 08)


####    CLDQ     ####

# all collapsed items
cldq.c.items <- paste0(cldq.items, "_c")

# check power
find_k(variables = select(dial.tst, all_of(cldq.c.items)), m = 2)

## Investigating 2 factor model
# run kfa
cldq.kfa <- kfa(data = dial.tst, variables = cldq.c.items,
                k = 2, # number of folds
                m = 3, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise",
                custom.cfas = list(original2 = paste0("cognition =~ ", paste(cldq.c.items[1:4], collapse = " + "),
                                                      "\nanxiety =~ ", paste(cldq.c.items[5:7], collapse = " + "))))

# generate report
kfa_report(cldq.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/CLDQ_Psychometric_Report_T1T2",
           report.title = "CLDQ Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - 1-factor model has high error (.99, .17, .09) as does the custom 2-factor model (.99, .16, .09)
# - empirical 2-factor has good fit (1.00, .09, .04), but 2nd factor is only 2 items with soc-anx1 loading on the first factor

## Investigating separate 1 factor Cognition model (Anxiety is only 3 items)
cldq.kfa.cog <- kfa(data = dial.tst, variables = cldq.c.items[1:4],
                        k = 2, # number of folds
                        m = 2, # maximum number of factors to test (defaults to 2)
                        ordered = TRUE, # treat variables as ordered
                        missing = "listwise")

kfa_report(cldq.kfa.cog,
                 index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                 file.name = "Psychometric Reports/CLDQ_Cog_Psychometric_Report__T1T2",
                 report.title = "CLDQ Cognition Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - Cognition: Good enough - 1.00, .09, .01; reliability = .93
# - Anxiety: Only 3 items so fit is perfect; Item 1 loading lower (.58 vs. .90+); reliability = .78
# - Overall: Used original items from scale with previous validity evidence and fit is good enough here so keep as is


## Final model run on each wave for supplemental materials
cldq.mod <- "cognition =~ TR_SOCIAL_COGNITION_1_c + TR_SOCIAL_COGNITION_2_c + TR_SOCIAL_COGNITION_3_c + TR_SOCIAL_COGNITION_4_c
anxiety =~ TR_SOCIAL_ANXIETY_1_c + TR_SOCIAL_ANXIETY_2_c + TR_SOCIAL_ANXIETY_3_c"

cldq.cfa <- map(1:2,
                ~cfa(model = cldq.mod,
                     data = dial.tst[dial.tst$Wave == .x, ],
                     ordered = TRUE, missing = "listwise", estimator = "WLSMV"))

cldq.out <- list(Fit = fits_wrapper(cldq.cfa),
                 Reliability = map_dfr(cldq.cfa, ~get_relis(.x), .id = "Model"),
                 Loadings = map2_dfr(cldq.cfa, c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                   tidyr::spread(Wave, est.std))

cldq.out$Fit

#### STRS-Short Form; Conflict subscale ####

# check power
find_k(variables = select(dial.tst, all_of(strscon.items)), m = 2)

## Investigating 1 factor model
# run kfa
strs.kfa <- kfa(data = dial.tst, variables = strscon.items,
                k = 2, # number of folds
                m = 3, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise")

# generate report
kfa_report(strs.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/STRS-Conflict_Psychometric_Report_T1T2",
           report.title = "STRS-Conflict Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - 1-factor works (1.00, .10, .02) and only model extracted

####    TASB     ####

## Academic

# run kfa
tasbac.kfa <- kfa(data = dial.tst, variables = tasbac.items,
                  k = 2, # number of folds
                  m = 2, # maximum number of factors to test
                  ordered = TRUE, # treat variables as ordered
                  missing = "listwise")

# generate report
kfa_report(tasbac.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/TASB_Psychometric_Report_Academic_T1T2",
           report.title = "TASB Academic Psychometric Report - Time 1 and 2")

lavaan::modificationindices(tasbac.kfa$cfas[[1]]$`1-factor`, sort. = TRUE)

# CONCLUSION:
# - 1-factor RMSEA is horrible (1.00, .25, .01)
# - All loadings are > .90; MIs suggest a variety of correlated residuals


## Bullying

# all collapsed items
tasbbly.c.items <- paste0(tasbbly.items, "_c")

## Investigating 2 factor model
# run kfa
tasbbly.kfa <- kfa(data = dial.tst, variables = tasbbly.c.items,
                   k = 2, # number of folds
                   m = 3, # maximum number of factors to test
                   ordered = TRUE, # treat variables as ordered
                   missing = "listwise")

# generate report
kfa_report(tasbbly.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/TASB_Psychometric_Report_Bullying_T1T2",
           report.title = "TASB Bullying Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - Only 1-factor model was extracted (2 & 3 had single item factors)
# - 1-factor model has good fit (1.00, .06, .05)


# #### Teacher Report of Students Summary ####
# # didn't end up using this format for reporting
# tst.summary <- list(`Behavior Risk Screener` = list(Items = saebrs.r.items,
#                                                     Structure = model_structure(saebrs.kfa)[5, 1],
#                                                     Fit = agg_model_fit(k_model_fit(saebrs.kfa, index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr")))[4,],
#                                                     Reliability = agg_rels(saebrs.kfa)$reliabilities$bifactor))


############################################################

#---------- Teacher self-report scales ----------

####    OSTES     ####

# check power
find_k(variables = select(dial.tslf, all_of(ostes.items)), m = 3)

# all collapsed items
ostes.c.items <- paste0(ostes.items, "_c")

ostes.mod <- "f1 =~ T_TEACHER_EFFICACY_11_c + T_TEACHER_EFFICACY_2_c + T_TEACHER_EFFICACY_3_c + T_TEACHER_EFFICACY_4_c
f2 =~ T_TEACHER_EFFICACY_1_c + T_TEACHER_EFFICACY_6_c + T_TEACHER_EFFICACY_7_c + T_TEACHER_EFFICACY_8_c
f3 =~ T_TEACHER_EFFICACY_5_c + T_TEACHER_EFFICACY_9_c + T_TEACHER_EFFICACY_10_c + T_TEACHER_EFFICACY_12_c"

## Investigating up to 3 factor model
# run kfa
ostes.kfa <- kfa(data = dial.tslf, variables = ostes.c.items,
                 k = 0, # number of folds
                 m = 3, # maximum number of factors to test
                 ordered = TRUE, # treat variables as ordered
                 missing = "listwise",
                 custom.cfas = list(Original = ostes.mod))


# generate report
kfa_report(ostes.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/OSTES Psychometric Report_T1_T2",
           report.title = "OSTES Psychometric Report - Time 1 and 2")

## Final model run on each wave for supplemental materials
ostes.cfa <- map(1:2,
                ~cfa(model = ostes.mod,
                     data = dial.tslf[dial.tslf$Wave == .x, ],
                     ordered = TRUE, missing = "listwise", estimator = "WLSMV"))

ostes.out <- list(Fit = fits_wrapper(ostes.cfa),
                 Reliability = map_dfr(ostes.cfa, ~get_relis(.x), .id = "Model"),
                 Loadings = map2_dfr(ostes.cfa, c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                   tidyr::spread(Wave, est.std))

ostes.out$Fit

# CONCLUSION:
# - Original model fit (.99, .06, .07); as did the empirical 2 and 3 factor


####    SNCS     ####

# all collapsed items
sncs.c.items <- paste0(sncs.items, "_c")

## Investigating up to 3 factor model
# run kfa
sncs.kfa <- kfa(data = dial.tslf, variables = sncs.c.items,
                k = 0, # number of folds
                m = 3, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise",
                custom.cfas = list(Resid12 = paste0(write_efa(1, sncs.c.items), "T_SPED_CONFIDENCE_1_c ~~ T_SPED_CONFIDENCE_2_c")))

lapply(sncs.kfa$cfas[[1]], lavaan::modificationindices, sort. = TRUE)


# generate report
kfa_report(sncs.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/SNCS Psychometric Report_T1T2",
           report.title = "SNCS Psychometric Report - Time 1 and 2")


# CONCLUSION:
# - 3 factor model actually fits reasonably enough (.99, .10, .06); doubtful the factors hold individually though
#   - Additionally, I don't know how you interpret the structure
# - 1-factor model has high rmsea and srmr (.95, .19, .13); adding residual correlation does not help much (.96, .17, .12)


####    SSBQ     ####

# all collapsed items
ssbq.c.items <- paste0(ssbq.items, "_c")

## Investigating up to 3 factor model
# run kfa
ssbq.kfa <- kfa(data = dial.tslf, variables = ssbq.c.items,
                k = 0, # number of folds
                m = 4, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise",
                custom.cfas = list(Resid23 = paste0(write_efa(1, ssbq.c.items), "T_BULLY_ATTITUDES_2_c ~~ T_BULLY_ATTITUDES_3_c")))
# bifactor = paste0("general =~ ", paste(ssbq.c.items, collapse = " + "),
#              "\nnormative =~ ", paste(ssbq.c.items[1:3], collapse = " + "),
#              "\navoidance =~ ", paste(ssbq.c.items[4:5], collapse = " + "),
#              "\nassertion =~ ", paste(ssbq.c.items[6:8], collapse = " + "),
#              "\ndismissive =~ ", paste(ssbq.c.items[9:10], collapse = " + "),
#              "\ngeneral ~~ 0*normative + 0*avoidance + 0*assertion + 0*dismissive\nnormative ~~ 0*avoidance + 0*assertion + 0*dismissive\navoidance ~~ 0*assertion + 0*dismissive\nassertion ~~ 0*dismissive")))

lavaan::modificationindices(ssbq.kfa$cfas[[1]]$`1-factor`, sort. = TRUE)
lapply(ssbq.kfa$cfas[[1]], lavaan::modificationindices, sort. = TRUE)


# generate report
kfa_report(ssbq.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/SSBQ Psychometric Report_T1T2",
           report.title = "SSBQ Psychometric Report - Time 1 and 2")


## Final model run on each wave for supplemental materials
ssbq.mod <- "f1 =~ T_BULLY_ATTITUDES_1_c + T_BULLY_ATTITUDES_2_c + T_BULLY_ATTITUDES_3_c
f2 =~ T_BULLY_ATTITUDES_4_c + T_BULLY_ATTITUDES_5_c
f3 =~ T_BULLY_ATTITUDES_6_c + T_BULLY_ATTITUDES_7_c + T_BULLY_ATTITUDES_8_c + T_BULLY_ATTITUDES_9_c + T_BULLY_ATTITUDES_10_c"

ssbq.cfa <- map(1:2,
                ~cfa(model = ssbq.mod,
                     data = dial.tslf[dial.tslf$Wave == .x, ],
                     ordered = TRUE, missing = "listwise", estimator = "WLSMV"))

ssbq.out <- list(Fit = fits_wrapper(ssbq.cfa),
                  Reliability = map_dfr(ssbq.cfa, ~get_relis(.x), .id = "Model"),
                  Loadings = map2_dfr(ssbq.cfa, c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                    tidyr::spread(Wave, est.std))

ssbq.out$Fit

# CONCLUSION:
# - 3 factor model is reasonable (.98, .08, .09); combines the advocate assertion and dismissive beliefs factors from original scale


####  MOAQ  ####

# all collapsed items
moaq.c.items <- paste0(moaq.r.items, "_c")

## Investigating 2 factor model
# run kfa
moaq.kfa <- kfa(data = dial.tslf, variables = moaq.c.items,
                k = 0, # number of folds
                m = 1, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise")

# generate report
kfa_report(moaq.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/MOAQ_Psychometric_Report_T1T2",
           report.title = "MOAQ Psychometric Report - Time 1 and 2")
# - 



####    CMPQ     ####

# all collapsed items
cmpq.c.items <- cmpq.items
cmpq.c.items[c(1:6, 12:13)] <- paste0(cmpq.items[c(1:6, 12:13)], "_c")


## Investigating up to 3 factor model
# run kfa
cmpq.kfa <- kfa(data = dial.tslf, variables = cmpq.c.items,
                k = 0, # number of folds
                m = 3, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise")


# generate report
kfa_report(cmpq.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/CMPQ Psychometric Report_T1T2",
           report.title = "CMPQ Psychometric Report - Time 1 and 2")


# k_model_fit(cmpq.kfa, index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr")) %>%
#   agg_model_fit()

# CONCLUSION:
# - None of the models fit; I'm not surprised given the items



####    CTBPT     ####

# all collapsed items
ctbpt.c.items <- paste0(ctbpt.items, "_c")

## Investigating up to 6 factor model
# run kfa
ctbpt.kfa <- kfa(data = dial.tslf, variables = ctbpt.c.items,
                 k = 0, # number of folds
                 m = 6, # maximum number of factors to test
                 ordered = TRUE, # treat variables as ordered
                 missing = "listwise")

# generate report
kfa_report(ctbpt.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/CTBPT Psychometric Report_T1T2",
           report.title = "CTBPT Psychometric Report - Time 1 and 2")

ctbpt.strux <- model_structure(ctbpt.kfa)
ctbpt.fits <- k_model_fit(ctbpt.kfa, index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr")) %>%
  agg_model_fit()

cat(ctbpt.kfa$cfa.syntax[[1]]$`5-factor`)

# CONCLUSION:
# - 5-factor model actually fits well enough (.99, .06, .10) and with the expected structure 

## Final model run on each wave for supplemental materials
ctbpt.mod <- paste0("Students =~ ", paste(ctbpt.items[1:5], collapse = " + "),"\nStaff =~ ", paste(ctbpt.items[6:10], collapse = " + "),
                    "\nAggression =~ ", paste(ctbpt.items[11:15], collapse = " + "), "\nCommitment =~ ", paste(ctbpt.items[16:25], collapse = " + "),
                    "\nPositive =~ ", paste(ctbpt.items[26:32], collapse = " + "))

ctbpt.cfa <- map(1:2,
                ~cfa(model = ctbpt.mod,
                     data = dial.tslf[dial.tslf$Wave == .x, ],
                     ordered = TRUE, missing = "listwise", estimator = "WLSMV"))

ctbpt.out <- list(Fit = fits_wrapper(ctbpt.cfa),
                 Reliability = map_dfr(ctbpt.cfa, ~get_relis(.x), .id = "Model"),
                 Loadings = map2_dfr(ctbpt.cfa, c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                   tidyr::spread(Wave, est.std))

ctbpt.out$Fit

## Investigating separate 1 factor models
ctbpt.kfa.1d <- map(.x = list(`Students Intervene` = ctbpt.c.items[1:5],
                              `Staff Intervene` = ctbpt.c.items[6:10],
                              Aggression = ctbpt.c.items[11:15],
                              Policy = ctbpt.c.items[16:25],
                              `Staff Climate` = ctbpt.c.items[26:32]),
                    ~kfa(data = dial.tslf, variables = .x,
                         k = 0, # number of folds
                         m = 2, # maximum number of factors to test (defaults to 2)
                         ordered = TRUE, # treat variables as ordered
                         missing = "listwise"))

map2(.x = ctbpt.kfa.1d, .y = names(ctbpt.kfa.1d),
     ~kfa_report(.x,
                 index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                 file.name = paste0("Psychometric Reports/CTBPT_Psychometric_Report_", .y, "_T1T2"),
                 report.title = paste0("CTBPT Psychometric Report - ", .y, " - Time 1 and 2")))

# CONCLUSION:
# - 1-factor models are not great


####   Bullying Prevalence from Authoritative School Climate   ####

# all collapsed items
blyprev.c.items <- paste0(blyprev.items, "_c")

## Investigating up to 2 factor model
# run kfa
blyprev.kfa <- kfa(data = dial.tslf, variables = blyprev.c.items,
                   k = 0, # number of folds
                   m = 2, # maximum number of factors to test
                   ordered = TRUE, # treat variables as ordered
                   missing = "listwise")


# generate report
kfa_report(blyprev.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/Bullying Prevalence Psychometric Report_T1T2",
           report.title = "Bullying Prevalence Psychometric Report - Time 1 and 2")


# CONCLUSION:
# - Neither 1-factor (.98, .18, .08) nor 2-factor (.99, .14, .06) have great fit


####    BPS    ####

# all collapsed items
bps.c.items <- paste0(bps.items, "_c")

## Investigating up to 4 factor model
# run kfa
bps.kfa <- kfa(data = dial.tslf, variables = bps.c.items,
               k = 0, # number of folds
               m = 4, # maximum number of factors to test
               ordered = TRUE, # treat variables as ordered
               missing = "listwise")

# generate report
kfa_report(bps.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/BPS Psychometric Report_T1T2",
           report.title = "BPS Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - 1-factor is trash
# - Item 6 is heywood

## Investigating separate 1 factor models
bps.kfa.1d <- map(.x = list(Prevention = bps.c.items[c(1,2,5:7)],
                            `Prevention 1257` = bps.c.items[c(1,2,5,7)], 
                            `Prevention w34` = bps.c.items[1:7],
                            Confidence = bps.c.items[10:13],
                            `Confidence w89` = bps.c.items[8:13],
                            `Confidence w3489` = bps.c.items[c(3,4, 8:13)]),
                  ~kfa(data = dial.tslf, variables = .x,
                       k = 0, # number of folds
                       m = 2, # maximum number of factors to test (defaults to 2)
                       ordered = TRUE, # treat variables as ordered
                       missing = "listwise"))

map2(.x = bps.kfa.1d, .y = names(bps.kfa.1d),
     ~kfa_report(.x,
                 index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                 file.name = paste0("Psychometric Reports/BPS_Psychometric_Report_", .y, "_T1T2"),
                 report.title = paste0("BPS Psychometric Report - ", .y, " - Time 1 and 2")))

# CONCLUSION:
# - 1257 has good fit; item 6 was problematic here, but item 7 in wave 1; all of these items are from original survey
# - items 3 and 4 dont fit anywhere
# - Not sure what to do about confidence


##############################################################

# ---------- Student self-report scales ----------

####    SPS    ####

## Investigating up to 2 factor model
# run kfa
sps.kfa <- kfa(data = dial.sslf, variables = sps.items,
               k = 2, # number of folds
               m = 2, # maximum number of factors to test
               ordered = TRUE, # treat variables as ordered
               missing = "listwise")


# generate report
kfa_report(sps.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/SPS Psychometric Report_T1T2",
           report.title = "SPS Psychometric Report - Drop 3 - Time 1 and 2")

# CONCLUSION:
# - 1-factor model had poor cfi (.89, .07, .07), and items 3 and 4 had low loadings
# - Dropping 3 (the lone class management item) improved model (.96, .05, .05); item 4 still had low loading

####    Bullying Perpetration    ####

# all collapsed items
stbp.c.items <- paste0(stbp.items, "_c")

## Investigating up to 2 factor model
# run kfa
stbp.kfa <- kfa(data = dial.sslf, variables = stbp.c.items,
                k = 2, # number of folds
                m = 2, # maximum number of factors to test
                ordered = TRUE, # treat variables as ordered
                missing = "listwise")


# generate report
kfa_report(stbp.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/STBP Psychometric Report_T1T2",
           report.title = "STBP Psychometric Report - Time 1 and 2")


# CONCLUSION:
# - 1-factor model had good fit (.99, .05, .07)


####    Fight    ####

# all collapsed items
fight.c.items <- paste0(fight.items, "_c")

## Investigating up to 2 factor model
# run kfa
fight.kfa <- kfa(data = dial.sslf, variables = fight.c.items,
                 k = 2, # number of folds
                 m = 2, # maximum number of factors to test
                 ordered = TRUE, # treat variables as ordered
                 missing = "listwise")


# generate report
kfa_report(fight.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/Fight Psychometric Report_T1T2",
           report.title = "Fight Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - 1-factor model had good fit (.99, .06, .05)

####    Peer Victimization    ####

## Investigating up to 2 factor model
# run kfa
peervict.kfa <- kfa(data = dial.sslf, variables = peervict.items,
                    k = 2, # number of folds
                    m = 2, # maximum number of factors to test
                    ordered = TRUE, # treat variables as ordered
                    missing = "listwise")


# generate report
kfa_report(peervict.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/Peer Victimization Psychometric Report_T1T2",
           report.title = "Peer Victimization Psychometric Report - Time 1 and 2")

# CONCLUSION:
# - 1-factor model had good fit (.99, .07, .03)

####    CTBPS    ####

## Investigating up to 5 factor model
# run kfa
ctbps.kfa <- kfa(data = dial.sslf, variables = ctbps.items,
                 k = 2, # number of folds
                 m = 5, # maximum number of factors to test
                 ordered = TRUE, # treat variables as ordered
                 missing = "listwise")


# generate report
kfa_report(ctbps.kfa,
           index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
           file.name = "Psychometric Reports/Colorado Trust Student Psychometric Report_T1T2",
           report.title = "Colorado Trust Student Psychometric Report - time 1 and 2")

# CONCLUSION:
# - Not helpful

## Investigating separate 1 factor models
ctbps.kfa.1d <- map(.x = list(`Getting Along` = gawo.items,
                              `Staff Intervene` = srtint.items, 
                              `Students Intervene` = srsint.items),
                    ~kfa(data = dial.sslf, variables = .x,
                         k = 2, # number of folds
                         m = 2, # maximum number of factors to test (defaults to 2)
                         ordered = TRUE, # treat variables as ordered
                         missing = "listwise"))

ctbps.kfa.1d$`Classroom Climate` <- kfa(data = dial.sslf, variables = classclim.items,
                                        k = 2, # number of folds
                                        m = 2, # maximum number of factors to test (defaults to 2)
                                        ordered = TRUE, # treat variables as ordered
                                        missing = "listwise",
                                        custom.cfas = list(Resid23 = paste0(write_efa(1, classclim.items),"ST_CLASSROOM_CLIMATE_2 ~~ ST_CLASSROOM_CLIMATE_3")))


map2(.x = ctbps.kfa.1d, .y = names(ctbps.kfa.1d),
     ~kfa_report(.x,
                 index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr"),
                 file.name = paste0("Psychometric Reports/CTBPS_Psychometric_Report_", .y, "_T1T2"),
                 report.title = paste0("CTBPS Psychometric Report - ", .y, " - Time 1 and 2")))

lapply(ctbps.kfa.1d, function(x) lavaan::modificationindices(x$cfas[[1]]$`1-factor`, sort. = TRUE))
lavaan::modificationindices(ctbps.kfa.1d$`Classroom Climate`$cfas[[1]]$Resid23, sort. = TRUE)

# CONCLUSION:
# - Getting Along: 1-factor fit is meh (.99, .13, .06), but item 2 has iffy loading and reliability is only .58
# - Staff Intervene: 1-factor is good (.99, .05, .03)
# - Student Intervene: 1-factor is good enough (.98, .06, .03)
# - Classroom Climate: 1-factor is not great (.90, .15, .09), but 2-factor model unstable


#### Student Self-Report Summary ####

# st.summary <- list(`Classroom Experiences` = list(Items = sps.items,
#                                                     Structure = model_structure(sps.kfa)[1, 1],
#                                                     Fit = agg_model_fit(k_model_fit(sps.kfa, index = c("chisq.scaled", "cfi.scaled", "rmsea.scaled", "srmr")))[1,],
#                                                     Reliability = agg_rels(sps.kfa)$reliabilities[[1]]))

############################################################

# -----   Saving datasets and results  --------

save(dial.long, dial.rn1, dial.tst, dial.tslf, dial.sslf,
     tst.freqs, tslf.freqs, sslf.freqs,
     idplus.vars, tst.items, t.items, st.items,
     # tst.summary,
     file = "Psychometric Reports/Psychometrics_Checks_T1T2.RData")
