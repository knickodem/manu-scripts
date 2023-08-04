#########################################################
#                                                       #
#  IES Goal 2: Disability Anti-Bullying (DIAL) Training #      
#            Psychometric Investigation                 #
#                                                       #
#########################################################


source("Scripts/IG2_PnF.R")

## Importing data
load("Psychometric Reports/Psychometrics_Checks_T1T2.RData")

##########################################
####          Creating Scores         ####


# --------   Teacher Reports of Students  --------

#### Creating scores #### 
## must have response to all items to receive score
# scores are mean item response
tst.scores <- dial.tst %>%
  mutate(TR_BEHAVIOR_RISK_SCORE = scale_score(., items = tst.items$Behavior_Risk, type = "mean", min.valid = length(tst.items$Behavior_Risk)),
         TR_SOCIAL_COGNITION_SCORE = scale_score(., items = tst.items$Social_Cognition, type = "mean", min.valid = length(tst.items$Social_Cognition)),
         TR_SOCIAL_ANXIETY_SCORE = scale_score(., items = tst.items$Social_Anxiety, type = "mean", min.valid = length(tst.items$Social_Anxiety)),
         TR_CONFLICT_SCORE = scale_score(., items = tst.items$Conflict, type = "mean", min.valid = length(tst.items$Conflict)),
         TR_BULLYING_SCORE = scale_score(., items = tst.items$Bullying, type = "mean", min.valid = length(tst.items$Bullying)),
         TR_ACADEMIC_COMP_SCORE = scale_score(., items = tst.items$Academic_Competency, type = "mean", min.valid = length(tst.items$Academic_Competency)))

## transform to wide format - one row per student
tst.scores.wide <- tst.scores %>%
  select(Wave:SCHOOL_ID, ends_with("SCORE"), -starts_with("FSA")) %>% # exclude FSA scores
  tidyr::gather(variable, score, ends_with("SCORE")) %>%
  tidyr::unite(col = var, variable, Wave, sep = "_W") %>%
  tidyr::spread(var, score)


#### Testing Dimensionality on full sample ####

## Running CFA on full sample in each wave
tst.names2 <- rep(names(tst.items)[-1], each = 2)
tst.wvs <- rep(c(1, 2), times = length(tst.names2)/2)
tst.cfas <- map2(.x = tst.names2,
                 .y = tst.wvs,
                 ~cfa(model = paste(.x, "=~", paste(tst.items[[.x]], collapse = " + ")),
                      data = dial.tst[dial.tst$Wave == .y, ],
                      ordered = TRUE, missing = "listwise", estimator = "WLSMV")) %>%
  set_names(paste(stringr::str_replace_all(tst.names2, "_", " "), tst.wvs, sep = " - W"))

## SAEBRS used bifactor model, so running separately
tst.br <- map(.x = c(1, 2),
            ~cfa(model = paste0("total =~ ", paste(tst.items$Behavior_Risk, collapse = " + "),
                                "\nsocial =~ ", paste(tst.items$Behavior_Risk[1:6], collapse = " + "),
                                "\nacademic =~ ", paste(tst.items$Behavior_Risk[7:12], collapse = " + "),
                                "\nemotional =~ ", paste(tst.items$Behavior_Risk[13:19], collapse = " + "),
                                "\ntotal ~~ 0*social + 0*academic + 0*emotional\nsocial ~~ 0*academic + 0*emotional\nacademic ~~ 0*emotional"),
                 data = dial.tst[dial.tst$Wave == .x, ],
                 ordered = TRUE, missing = "listwise", estimator = "WLSMV")) %>%
  set_names(paste0("Behavior Risk - W", 1:2))


## Dataframe of fit statistics
tst.fits <- fits_wrapper(tst.br) %>%
  bind_rows(fits_wrapper(tst.cfas))
## Dataframe of alpha and omega reliability
tst.relis <- map_dfr(tst.br, ~get_relis(.x), .id = "Model") %>%
  bind_rows(map_dfr(tst.cfas, ~get_relis(.x), .id = "Model"))

## Dataframe of standardized factor loadings for W1 and W2
tst.loadings <- list(map2_dfr(tst.br, c(1, 2), ~extract_lavaan_parameters(.x, std = "std.all", params = "=~") %>%
                                filter(lhs == "total") %>% mutate(Wave = paste0("W", .y)) %>% select(Wave, rhs, est.std)) %>%
                       tidyr::spread(Wave, est.std),
                     map2_dfr(tst.cfas[1:2], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                       tidyr::spread(Wave, est.std),
                     map2_dfr(tst.cfas[3:4], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                       tidyr::spread(Wave, est.std),
                     map2_dfr(tst.cfas[5:6], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                       tidyr::spread(Wave, est.std),
                     map2_dfr(tst.cfas[7:8], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                       tidyr::spread(Wave, est.std),
                     map2_dfr(tst.cfas[9:10], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
                       tidyr::spread(Wave, est.std)) %>%
  set_names(names(tst.items))

## commnents to add to output report
tst.comments <- list("The 19 Behavior Risk items are from SAEBRS (Kilgus et al. 2013, 2016, 2018; von der Embse et al., 2016). Prior psychometric evidence suggested the bifactor model was the most appropriate measurement model for the scale and our results concur. There was only sufficient evidence to support the total score (presented below) in our sample, but not subscores for the specific social, academic, and emotion factors. Some items were reversed coded prior to creating the scale score in accordance with the SAEBRS documentation and shown in the 'Loadings' section below. Higher scores on the scale indicate better behavior/lower risk. See cited papers for risk cutoffs.",
                     "The first 4 items from the CLDQ (Willcutt et al., 2011) comprise the Social Cognition scale. Higher scores indicate greater cognitive difficulties.",
                     "The last 3 items from the CLDQ (Willcutt et al., 2011) comprise the Social Awareness scale. With only 3 items the cfa model will have perfect model fit. Higher scores indicate lower awareness.",
                     "Items come from the conflict subscale of the STRS-Short Form (Pianta, 1992). Higher scores indicate greater conflict in the student-teacher relationship.",
                     "Bullying items come from the TASB (Brown et al., 2011). The originally study had physical and non-physical subscales, but those did not fit in our sample. Higher scores indicate more bullying.",
                     "Academic competence items come from the TASB (Brown et al., 2011). In our sample, the factor analysis suggested the 4 items were redundant (i.e., provided the same information). Consequently, only 1 item might be needed. Higher scores indicated higher competence.") %>%
  set_names(names(tst.items))



# --------   Teacher Self-Reports  --------

#### Creating scores #### 
## must have response to all items to receive score
# scores are mean item response
tslf.scores <- dial.tslf %>%
  mutate(T_TEACHER_EFFICACY_SCORE = scale_score(., items = t.items$Teacher_Efficacy, type = "mean", min.valid = length(t.items$Teacher_Efficacy)),
         T_STUDENT_ENGAGEMENT_SCORE = scale_score(., items = t.items$Student_Engagement, type = "mean", min.valid = length(t.items$Student_Engagement)),
         T_CLASSROOM_MANAGEMENT_SCORE = scale_score(., items = t.items$Classroom_Management, type = "mean", min.valid = length(t.items$Classroom_Management)),
         T_INSTRUCTIONAL_STRATEGIES_SCORE = scale_score(., items = t.items$Instructional_Strategies, type = "mean", min.valid = length(t.items$Instructional_Strategies)),
         T_NORMATIVE_ATTITUDE_SCORE = scale_score(., items = t.items$Normative_Attitude, type = "mean", min.valid = length(t.items$Normative_Attitude)),
         T_AVOIDANCE_ATTITUDE_SCORE = scale_score(., items = t.items$Avoidance_Attitude, type = "mean", min.valid = length(t.items$Avoidance_Attitude)),
         T_MALADAPTIVE_ATTITUDE_SCORE = scale_score(., items = t.items$Maladaptive_Attitude, type = "mean", min.valid = length(t.items$Maladaptive_Attitude)),
         T_JOB_DISSATISFACTION_SCORE = scale_score(., items = t.items$Job_Dissatisfaction, type = "mean", min.valid = length(t.items$Job_Dissatisfaction)),
         T_STUDENTS_INTERVENE_SCORE = scale_score(., items = t.items$Students_Intervene, type = "mean", min.valid = length(t.items$Students_Intervene)),
         T_STAFF_INTERVENE_SCORE = scale_score(., items = t.items$Staff_Intervene, type = "mean", min.valid = length(t.items$Staff_Intervene)),
         T_AGGRESSION_PROBLEMS_SCORE = scale_score(., items = t.items$Aggression_Problems, type = "mean", min.valid = length(t.items$Aggression_Problems)),
         T_SCHOOL_COMMITMENT_SCORE = scale_score(., items = t.items$School_Commitment, type = "mean", min.valid = length(t.items$School_Commitment)),
         T_POSITIVE_INTERACTIONS_SCORE = scale_score(., items = t.items$Positive_Interactions, type = "mean", min.valid = length(t.items$Positive_Interactions)),
         T_PREVENTION_PD_SCORE = scale_score(., items = t.items$Prevention_PD, type = "mean", min.valid = length(t.items$Prevention_PD)))

## transform to wide format - one row per teacher
tslf.scores.wide <- tslf.scores %>%
  select(Wave:SCHOOL_ID, ends_with("SCORE"), -starts_with("FSA")) %>% # exclude FSA scores
  tidyr::gather(variable, score, ends_with("SCORE")) %>%
  tidyr::unite(col = var, variable, Wave, sep = "_W") %>%
  tidyr::spread(var, score)


#### Test Dimensionality on Full Sample ####

## Running CFA on full sample in each wave
tslf.names2 <- rep(names(t.items), each = 2) # [c(1:4, 7:10, 14)]
tslf.wvs <- rep(c(1, 2), times = length(tslf.names2)/2)
dial.tslf_c <- dial.tslf %>%
  mutate(T_JOB_SATISFACTION_r = if_else(T_JOB_SATISFACTION_r == 3, 2, T_JOB_SATISFACTION_r)) # frequencies are [4 33 26 0]
tslf.cfas <- map2(.x = tslf.names2,
                 .y = tslf.wvs,
                 ~cfa(model = paste(.x, "=~", paste(t.items[[.x]], collapse = " + ")),
                      data = dial.tslf_c[dial.tslf_c$Wave == .y, ],
                      ordered = TRUE, missing = "listwise", estimator = "WLSMV")) %>%
  set_names(paste(stringr::str_replace_all(tslf.names2, "_", " "), tslf.wvs, sep = " - W"))

## Dataframe of fit statistics
tslf.fits <- fits_wrapper(tslf.cfas[-c(13:14)]) %>%
  bind_rows(data.frame(Model = paste0("Avoidance Attitude - W", 1:2))) # need to add separately b/c it does not have fit statistics (2-item scale)
## Dataframe of alpha and omega reliability
tslf.relis <- map_dfr(tslf.cfas, ~get_relis(.x), .id = "Model")

## Dataframe of standardized factor loadings for W1 and W2
tslf.loadings <- vector(mode = "list", length = length(tslf.cfas))
for(i in seq(1, length(tslf.cfas), by = 2)){
  
  tslf.loadings[[i]] <- map2_dfr(tslf.cfas[c(i, i+1)], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
    tidyr::spread(Wave, est.std)
  
}
tslf.loadings <- Filter(Negate(is.null), tslf.loadings) # removes blank list elements
names(tslf.loadings) <- names(t.items) # names the list for printing in report

## comments to add to output report
tslf.comments <- list("The 12 items for the Teacher Efficacy score come from the Ohio State Teacher Efficacy short-form scale (Tschannen-Moran & Woolfolk Hoy, 2001). Higher scores indicate greater teacher self-efficacy.",
                      "A subscore from the Teacher Efficacy scale, the Student Engagement scale consists of 4 items from the Ohio State Teacher Efficacy scale (Tschannen-Moran & Woolfolk Hoy, 2001). Higher scores indicate higher teacher self-efficacy for engaging students.",
                      "A subscore from the Teacher Efficacy scale, the Classroom Management scale consists of 4 items from the Ohio State Teacher Efficacy scale (Tschannen-Moran & Woolfolk Hoy, 2001). Higher scores indicate higher teacher self-efficacy for managing their classroom.",
                      "A subscore from the Teacher Efficacy scale, the Instructional Strategies scale consists of 4 items from the Ohio State Teacher Efficacy scale (Tschannen-Moran & Woolfolk Hoy, 2001). Higher scores indicate higher teacher self-efficacy for using effective instructional strategies.",
                      "The Special Needs Confidence Scale was originally a 22-item scale (LePage, Lewis, & Casella, 1995), which was shortened and adapted to 13-items by Jung, Cho, & Ambrosetti (2011). The IES Goal 2 study used 11 of these 13 items, some of which were further adapted. The 1-factor model has high RMSEA and SRMR. Alternative models and modification indices did not reveal a clear structure. No score was created for this scale.",
                      "The SSBQ (Kochenderfer-Ladd & Pelletier, 2008; Troop-Gordon & Ladd, 2015) had 13 items and a 4-factor structure. One factor was the 4-item Normative Attitude scale. The IES Goal 2 study used 10 items loading onto 3 factors, including a 3-item Normative Attitude scale. As a 3-item scale, model fit cannot be evaluated. Higher scores indicate stronger attitude toward bullying being a normal part of child interactions.",
                      "The SSBQ (Kochenderfer-Ladd & Pelletier, 2008; Troop-Gordon & Ladd, 2015) had 13 items and a 4-factor structure. One factor was the 3-item Avoidance Attitude scale. The IES Goal 2 study used 10 items loading onto 3 factors, including a 2-item Avoidance Attitude scale. As a 2-item scale, model fit cannot be evaluated. Higher scores indicated stronger attitude that students should ignore bullies.",
                      "The SSBQ (Kochenderfer-Ladd & Pelletier, 2008; Troop-Gordon & Ladd, 2015) had 13 items and a 4-factor structure. Two factors were the 3-item Advocate Assertion and 3-item Dismissive Belifs scales. The IES Goal 2 study used 10 items loading onto 3 factors, including a 5-item Maladaptive Attitude scale comprised of the 3 Advocate Assertion items and adapted versions of 2 of the 3 Dismissive Beliefs items. Higher scores indicate stronger maladaptive attitudes toward bullying.",
                      "The 3 items in the Job Dissatisfaction scale are either newly created or adapted versions of the job satisfaction subscale of the Michigan Organized Assessment Questionnaire (Bowling & Hammond, 2008; Camman et al., 1979). One item is reversed scored (contains '_r' suffix). Higher scores indicate greater dissatisfaction with job.",
                      "The CMPQ (Troop-Gordon and Ladd, 2015) had 26 items and a 6 factor structure, all of which were moderately to weakly correlated (r < |.36|). The IES Goal 2 study used all items from the Contact Parents construct and shorted the remaining 5 factors from 4, 5, 4, 4, and 5 items to 2, 3, 2, 2, and 1 item, respectively. The new 14-item scale had poor unidimensional fit and unclear multidimensional structure. The Contact Parents construct also did not hold together on its own. No score was created for this scale.",
                      "The 5 items for the Students Intervene scale come from the [Colorado Trust Bullying Prevention Staff Survey](https://www.coloradotrust.org/strategies/bullying-prevention/). The scale had high RMSEA in the IES Goal 2 study, so previous psychometric work (e.g., Espelage, Polanin & Low, 2014; Low et al., 2014) should be referenced to justify score interpretation. Higher scores indicate greater willingness of students to help out in bullying situations.",
                      "The 5 items for the Staff Intervene scale come from the [Colorado Trust Bullying Prevention Staff Survey](https://www.coloradotrust.org/strategies/bullying-prevention/). The scale had high RMSEA in the IES Goal 2 study, so previous psychometric work (e.g., Espelage, Polanin & Low, 2014; Low et al., 2014) should be referenced to justify score interpretation. Higher scores indicate greater willingness of teachers and staff to help out in bullying situations.",
                      "The 5 items for the Aggression Problem scale come from the [Colorado Trust Bullying Prevention Staff Survey](https://www.coloradotrust.org/strategies/bullying-prevention/). The scale had high RMSEA in the IES Goal 2 study, so previous psychometric work (e.g., Espelage, Polanin & Low, 2014; Low et al., 2014) should be referenced to justify score interpretation. Higher scores indicate higher perceptions of aggression problems.",
                      "Eight of the 10 items for the School Commitment scale come from the [Colorado Trust Bullying Prevention Staff Survey](https://www.coloradotrust.org/strategies/bullying-prevention/) with 2 items newly created. The scale had high RMSEA in the IES Goal 2 study, so previous psychometric work (e.g., Espelage, Polanin & Low, 2014; Low et al., 2014) should be referenced to justify score interpretation. Higher scores indicate greater school commitment to bullying prevention.",
                      "The 7 items for the Positive Interactions scale come from the [Colorado Trust Bullying Prevention Staff Survey](https://www.coloradotrust.org/strategies/bullying-prevention/). The scale had high RMSEA in the IES Goal 2 study, so previous psychometric work (e.g., Espelage, Polanin & Low, 2014; Low et al., 2014) should be referenced to justify score interpretation. Higher scores indicate more positive school environment and interactions between teachers and staff with colleagues, students, and parents.",
                      "The 7 items for the Bullying Prevalence scale come from the Authoritative School Climate Teacher Version (Cornell, 2015). Three items were used exactly (1, 2, and 6) while the others were adapted. Both 1- and 2-factor structures had poor RMSEA. Appealing to prvious psychometric work is tenuous given that most of the items were adapted; therefore, no score has been created for this scale at this time.",
                      "The 5 items in the Prevention PD scale came from the 10-item Bullying Prevention Survey (Kennedy, Russom, & Kevorkian, 2012). Higher scores indicate stronger support for bullying prevention professional development.",
                      "We attempted to build a Confidence scale based on 2 items from the Bullying Prevention Survey (Kennedy, Russom, & Kevorkian, 2012), 2 items adapted from those BPS items specifically for children with disabilities, and 2 newly created items (6 total items). A suitable factor structure, however, could not be found, so no score was created.") %>%
  set_names(names(t.items))


# --------   Student Self-Report  --------

#### Creating scores #### 
## must have response to all items to receive score
# scores are mean item response
sslf.scores <- dial.sslf %>%
  mutate(ST_CLASSROOM_EXPERIENCE_SCORE = scale_score(., items = st.items$Classroom_Experience, type = "mean", min.valid = length(st.items$Classroom_Experience)),
         ST_BULLYING_SCORE = scale_score(., items = st.items$Bullying, type = "mean", min.valid = length(st.items$Bullying)),
         ST_FIGHT_SCORE = scale_score(., items = st.items$Fight, type = "mean", min.valid = length(st.items$Fight)),
         ST_PEER_VICTIMIZATION_SCORE = scale_score(., items = st.items$Peer_Victimization, type = "mean", min.valid = length(st.items$Peer_Victimization)),
         ST_PROSOCIAL_SCORE = scale_score(., items = st.items$Prosocial, type = "mean", min.valid = length(st.items$Prosocial)),
         ST_STAFF_INTERVENE_SCORE = scale_score(., items = st.items$Staff_Intervene, type = "mean", min.valid = length(st.items$Staff_Intervene)),
         ST_STUDENT_INTERVENE_SCORE = scale_score(., items = st.items$Student_Intervene, type = "mean", min.valid = length(st.items$Student_Intervene)),
         ST_CLASS_CLIMATE_SCORE = scale_score(., items = st.items$Class_Climate, type = "mean", min.valid = length(st.items$Class_Climate)))

## transform to wide format - one row per student
sslf.scores.wide <- sslf.scores %>%
  select(Wave:SCHOOL_ID, MULTIPLE_INFORMANTS, ends_with("SCORE"), -starts_with("FSA")) %>% # exclude FSA scores
  tidyr::gather(variable, score, ends_with("SCORE")) %>%
  tidyr::unite(col = var, variable, Wave, sep = "_W") %>%
  tidyr::spread(var, score)


testthat::test_that("no duplicates", {
                    testthat::expect_equal(sslf.scores.wide$STUDENT_ID[duplicated(sslf.scores.wide$STUDENT_ID)] %>%
                                             unique() %>% length(), 0)
  })


#### Test Dimensionality on Full Sample ####

## Running CFA on full sample in each wave
sslf.names2 <- rep(names(st.items), each = 2)
sslf.wvs <- rep(c(1, 2), times = length(sslf.names2)/2)
sslf.cfas <- map2(.x = sslf.names2,
                  .y = sslf.wvs,
                  ~cfa(model = paste(.x, "=~", paste(st.items[[.x]], collapse = " + ")),
                       data = dial.sslf[dial.sslf$Wave == .y, ],
                       ordered = TRUE, missing = "listwise", estimator = "WLSMV")) %>%
  set_names(paste(stringr::str_replace_all(sslf.names2, "_", " "), sslf.wvs, sep = " - W"))

## Dataframe of fit statistics
sslf.fits <- fits_wrapper(sslf.cfas)
## Dataframe of alpha and omega reliability
sslf.relis <- map_dfr(sslf.cfas, ~get_relis(.x), .id = "Model")

## Dataframe of standardized factor loadings for W1 and W2
sslf.loadings <- vector(mode = "list", length = length(sslf.cfas))
for(i in seq(1, length(sslf.cfas), by = 2)){
  
  sslf.loadings[[i]] <- map2_dfr(sslf.cfas[c(i, i+1)], c(1,2), ~get_loadings(.x, std = "std.all") %>% mutate(Wave = paste0("W", .y))) %>%
    tidyr::spread(Wave, est.std)
  
}
sslf.loadings <- Filter(Negate(is.null), sslf.loadings) # removes blank list elements
names(sslf.loadings) <- names(st.items) # names the list for printing in report


## comments to add to output report
sslf.comments <- list("The 8 items for the Classroom Experience scale are drawn from 2 of the 4 subscales from the Student Perception Survey (Colorado Education Initiative, 2013). The 4 original subscales were Student Learning (15 items), Student-Centered Environment (10 items), Classroom Community (5 items) and Classroom Management (4 items). The Classrom Experience scale for the IES Goal 2 study used 4 items each from the Student-Centered Environment and Classroom Community subscales. One item from the Classroom Management scale was also administered, but did not fit with the rest of the rest of the scale. Higher Classroom Experience scores indicate more positive experiences.",
                      "Items come from the University of Illinois Bully scale (Espelage & Holt, 2001). Higher scores indicate more bullying perpetration.",
                      "Items come from the University of Illinois Fighting scale (Espelage & Holt, 2001). Higher scores indicate more fighting.",
                      "Items come from the University of Illinois Victimization scale (Espelage & Holt, 2001). Higher scores indicate more peer victimization.",
                      "The 4 items for the Prosocial scale are drawn from the Colorado Trust Bullying Prevention Student Survey ('Getting along with others' section). The first item was adapted. Higher scores indicate greater prosocial behavior.",
                      "The 5 items for the Staff Intervene scale are drawn from the Colorado Trust Bullying Prevention Student Survey ('Teachers and Staff at your school would help out if...' section) with one added item ('if hurt or upset' item). Higher scores indicate greater willingness of staff to intervene.",
                      "The 5 items for the Student Intervene scale are drawn from the Colorado Trust Bullying Prevention Student Survey ('Students at your school would help out if...' section) with one added item ('if hurt or upset' item). Higher scores indicate greater willingness of students to intervene.",
                      "The 6 items for the Classroom Climate scale are drawn from the original 16 items in the 'My School' section of the Colorado Trust Bullying Prevention Student Survey. Higher scores indicate more positive climate.") %>%
  set_names(names(st.items))

################################################################

# -----   Updating Dataset With Score  -----------------

## Joining scores and ordering items by scale rather than alphabetical
dial <- dial.rn1 %>%
  left_join(tst.scores.wide) %>%
  left_join(sslf.scores.wide) %>%
  left_join(tslf.scores.wide) %>%
  mutate(DIAL = as.numeric(CONDITION)) %>% # binary intervention indicator
  # below: ordering dataset by scale rather than alphabetical
  select(all_of(idplus.vars), DIAL,TR_DATE_W1, TR_DATE_W2, TR_DURATION_SEC_W1:TR_RACE_OTHER_SPECIFY_W2,
         # starts_with("TR_SOCIAL_BEHAVIOR"), starts_with("TR_ACADEMIC_BEHAVIOR"), starts_with("TR_EMOTIONAL_BEHAVIOR")
         TR_SOCIAL_BEHAVIOR_1_W1, TR_SOCIAL_BEHAVIOR_1_W1_r, TR_SOCIAL_BEHAVIOR_1_W2, TR_SOCIAL_BEHAVIOR_1_W2_r,
         TR_SOCIAL_BEHAVIOR_2_W1, TR_SOCIAL_BEHAVIOR_2_W2,
         TR_SOCIAL_BEHAVIOR_3_W1, TR_SOCIAL_BEHAVIOR_3_W1_r, TR_SOCIAL_BEHAVIOR_3_W2, TR_SOCIAL_BEHAVIOR_3_W2_r,
         TR_SOCIAL_BEHAVIOR_4_W1, TR_SOCIAL_BEHAVIOR_4_W1_r, TR_SOCIAL_BEHAVIOR_4_W2, TR_SOCIAL_BEHAVIOR_4_W2_r,
         TR_SOCIAL_BEHAVIOR_5_W1, TR_SOCIAL_BEHAVIOR_5_W2,
         TR_SOCIAL_BEHAVIOR_6_W1, TR_SOCIAL_BEHAVIOR_6_W1_r, TR_SOCIAL_BEHAVIOR_6_W2, TR_SOCIAL_BEHAVIOR_6_W2_r,
         TR_ACADEMIC_BEHAVIOR_1_W1:TR_ACADEMIC_BEHAVIOR_3_W2,
         TR_ACADEMIC_BEHAVIOR_4_W1, TR_ACADEMIC_BEHAVIOR_4_W1_r, TR_ACADEMIC_BEHAVIOR_4_W2, TR_ACADEMIC_BEHAVIOR_4_W2_r,
         TR_ACADEMIC_BEHAVIOR_5_W1, TR_ACADEMIC_BEHAVIOR_5_W1_r, TR_ACADEMIC_BEHAVIOR_5_W2, TR_ACADEMIC_BEHAVIOR_5_W2_r,
         TR_ACADEMIC_BEHAVIOR_6_W1, TR_ACADEMIC_BEHAVIOR_6_W2,
         TR_EMOTIONAL_BEHAVIOR_1_W1, TR_EMOTIONAL_BEHAVIOR_1_W1_r, TR_EMOTIONAL_BEHAVIOR_1_W2, TR_EMOTIONAL_BEHAVIOR_1_W2_r,
         TR_EMOTIONAL_BEHAVIOR_2_W1, TR_EMOTIONAL_BEHAVIOR_2_W1_r, TR_EMOTIONAL_BEHAVIOR_2_W2, TR_EMOTIONAL_BEHAVIOR_2_W2_r,
         TR_EMOTIONAL_BEHAVIOR_3_W1:TR_EMOTIONAL_BEHAVIOR_4_W2,
         TR_EMOTIONAL_BEHAVIOR_5_W1, TR_EMOTIONAL_BEHAVIOR_5_W1_r, TR_EMOTIONAL_BEHAVIOR_5_W2, TR_EMOTIONAL_BEHAVIOR_5_W2_r,
         TR_EMOTIONAL_BEHAVIOR_6_W1, TR_EMOTIONAL_BEHAVIOR_6_W1_r, TR_EMOTIONAL_BEHAVIOR_6_W2, TR_EMOTIONAL_BEHAVIOR_6_W2_r,
         TR_EMOTIONAL_BEHAVIOR_7_W1, TR_EMOTIONAL_BEHAVIOR_7_W1_r, TR_EMOTIONAL_BEHAVIOR_7_W2, TR_EMOTIONAL_BEHAVIOR_7_W2_r,
         starts_with("TR_SOCIAL_COGNITION"), starts_with("TR_SOCIAL_ANXIETY"),
         starts_with("TR_STUDENT_TEACHER_CONFLICT"), starts_with("TR_BULLY_PERP"), starts_with("TR_ACADEMIC_COMP"), starts_with("TR_"),
         T_DATE_W1, T_DATE_W2, T_DURATION_SEC_W1:T_EDUCATION_LEVEL_W2,
         starts_with("T_TEACHER_EFFICACY"), starts_with("T_SPED_CONFIDENCE"), starts_with("T_BULLY_ATTITUDES"),
         T_JOB_SATISFACTION_W1, T_JOB_SATISFACTION_W1_r, T_JOB_SATISFACTION_W2, T_JOB_SATISFACTION_W2_r,
         T_INTENT_LEAVE_1_W1:T_INTENT_LEAVE_2_W2, starts_with("T_CLASS_MANAG"),
         starts_with("T_STUDENTS_INTERVENE"), starts_with("T_STAFF_INTERVENE"), starts_with("T_AGGRESSION"),
         starts_with("T_SCHOOL_COMMITMENT"), starts_with("T_POSITIVE_INTERACT"), starts_with("T_BULLYING_PREVALENCE"),
         starts_with("T_PREVENTION_PD"), starts_with("T_CONFIDENCE_PARENTS"), starts_with("T_"),
         ST_DATE_W1, ST_DATE_W2, ST_TEACHER_NOT_IN_ROSTER_W1:ST_DISABILITY_SPECIFY_W2,
         starts_with("ST_CLASSROOM_EXPERIENCE"), starts_with("ST_BULLYING_PERP"), starts_with("ST_FIGHT"),
         starts_with("ST_PEER_VICT"), starts_with("ST_PROSOCIAL"), starts_with("ST_STAFF_INTERVENE"),
         starts_with("ST_STUDENT_INTERVENE"), starts_with("ST_CLASSROOM_CLIMATE"), starts_with("ST_"),
         -contains("SCORE_W"), contains("SCORE_W"), everything()) #moves scale scores to end of dataset along with any variable yet to be ordered

# names(dial)
# curious <- map_dfr(names(dial), ~data.frame(Item = .x, Class = paste(unlist(class(dial[[.x]])), collapse = ", ")))

##################################################################################


#################################
####    Demographic Coding   ####

## The team has decided to use priority for race with Haitian then Hispanic,
## and use Other for gender. Alternatively, people could just use W2


## Gathering unique teacher data since teacher rows were repeated in combined dataset
dial.t <- dial %>%
  select(TEACHER_ID, DIAL, CONDITION, SCHOOL_ID, T_WAVES_PRESENT, starts_with("T_")) %>%  # keeping only teacher ID and self-report items
  unique() %>% # dropping duplicates here
  filter(!is.na(T_DATE_W1) | !is.na(T_DATE_W2)) # dropping empty rows (some teachers did not complete self-report either wave)


# ----------- Teachers -------------

####   Race/Ethnicity  #####

t.other.specify <- c("White- Hispanic", "Hispanic", "Haitian", "hispanic", "Latina")

## teacher race data in long format
race.t.long <- dial.long %>%
  filter(!is.na(T_DATE)) %>%
  select(Wave, TEACHER_ID, starts_with('T_RACE')) %>%
  unique() %>%
  rename_with(~str_remove(., "T_RACE_") %>% str_to_title(), .cols = starts_with("T_RACE_")) %>%
  mutate(across(.cols = Asian:Other, as.numeric)) %>%
  mutate(Other = if_else(Other_specify %in% t.other.specify, 0, Other),
         White = if_else(Other_specify %in% c("White- Hispanic"), 1, White),
         Hispanic = if_else(Other_specify %in% c("White- Hispanic", "Hispanic", "hispanic", "Latina"), 1, Hispanic),
         Haitian = if_else(Other_specify %in% c("Haitian"), 1, Haitian))


## Coding teacher race across waves by multiple, priority, and mode approaches
race.t.codes <- catacode(race.t.long, id = TEACHER_ID, approach = "multiple", Asian:Other, time=Wave,new.name = "Multiple") %>%
  left_join(catacode(race.t.long, id = TEACHER_ID, approach = "priority", Asian:Other,priority = c('Haitian', 'Hispanic'), time = Wave, new.name = "Priority")) %>%
  left_join(catacode(race.t.long, id = TEACHER_ID, approach = "mode", Asian:Other, time=Wave, new.name = "Mode"))

## Plotting comparison
race.t.summary <- race.t.codes %>%
  gather(Approach, Identity, -TEACHER_ID) %>%
  group_by(Approach, Identity) %>%
  summarize(Count = n())

race.t.plot <- race.t.summary %>%
  ggplot(aes(x = Identity, y = Count, fill = Identity)) +
  geom_col() +
  geom_text(aes(label = Count, vjust = -.1)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Approach) +
  guides(fill = "none")


## How many teachers changed identity across waves?
race.t.all <- catacode(race.t.long, id = TEACHER_ID, approach = "all", Asian:Other, time = Wave, new.name = "Race_Ethnicity")
race.t.all.wide <- race.t.all %>%
  pivot_wider(names_from = Wave, values_from = Race_Ethnicity, names_prefix = "W") %>%
  mutate(Same = ifelse(W1==W2, 1, 0))
table(race.t.all.wide$Same, useNA = "always")
# 23 changed; 30 stayed the same; 12 had NA in one wave
# race.t.all.wide %>% filter(Same == 0) %>% View()
# all cases were mixed in W1 and picked one of the W1 identities for W2; mostly Hispanic-White to Hispanic

####   Gender   ####
## teacher gender data in long format
gen.t.long <- dial.long %>%
  filter(!is.na(T_DATE)) %>%
  select(Wave, TEACHER_ID, starts_with('T_GENDER')) %>%
  unique() %>%
  mutate(T_GENDER = as_factor(T_GENDER))

table(gen.t.long$T_GENDER, useNA = "always") # no teachers identified as Other

## coding gender - mode approach not relevant b/c this is only 2 waves and not CATA
gen.t.codes <- catacode(gen.t.long, id = TEACHER_ID, approach = "multiple", T_GENDER, time=Wave,new.name = "Multiple") %>%
  # left_join(catacode(gen.t.long, id = TEACHER_ID, approach = "priority", T_GENDER,priority = c('Other'), time = Wave, new.name = "Priority")) %>%
  left_join(catacode(gen.t.long, id = TEACHER_ID, approach = "mode", T_GENDER, time=Wave, new.name = "Mode"))

## Plotting comparison
gen.t.summary <- gen.t.codes %>%
  gather(Approach, Identity, -TEACHER_ID) %>%
  group_by(Approach, Identity) %>%
  summarize(Count = n())

gen.t.plot <- gen.t.summary %>%
  ggplot(aes(x = Identity, y = Count, fill = Identity)) +
  geom_col() +
  geom_text(aes(label = Count, vjust = -.1)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Approach) +
  guides(fill = "none")


## What kind of responses were coded as multiple and NA?
gen.t.multiple <- gen.t.codes[gen.t.codes$Multiple == "Multiple"|is.na(gen.t.codes$Multiple), ]$TEACHER_ID
# dial.t %>% filter(TEACHER_ID %in% gen.t.multiple) %>%
#   select(TEACHER_ID, starts_with("T_GENDER")) %>% 
#   mutate(across(starts_with("T_GENDER"), as_factor)) %>% View()


#########################################################

# ----------- Students -------------

####   Race/Ethnicity   ####

s.other.specify <- c("white", "White or American",
                     "hispanic american", "Hispanic", "hispanic", "Latina", "latina",
                     "hispanic or latin", "hispanic/latin", "Latin", "latin american", "Spanish",
                     "haitian", "haitan", "haitian american", "hatian-american")

race.s.long <- dial.long %>%
  filter(!is.na(ST_DATE)) %>%
  select(Wave, STUDENT_ID, starts_with('ST_RACE'), ST_HISPANIC, ST_HAITIAN) %>%
  unique() %>%
  rename_with(~str_remove(., "ST_RACE_") %>% str_to_title(), .cols = starts_with("ST_RACE_")) %>%
  rename_with(~str_remove(., "ST_") %>% str_to_title(), .cols = starts_with("ST_")) %>%
  mutate(across(.cols = -c(Wave, STUDENT_ID, Other_specify), as.numeric)) %>%
  mutate(Other = if_else(Other_specify %in% t.other.specify, 0, Other),
         White = if_else(Other_specify %in% c("white", "White or American"), 1, White),
         Hispanic = if_else(Other_specify %in% c("hispanic american", "Hispanic", "hispanic", "Latina", "latina",
                                                 "hispanic or latin", "hispanic/latin", "Latin", "latin american", "Spanish"), 1, Hispanic),
         Haitian = if_else(Other_specify %in% c("haitian", "haitan", "haitian american", "hatian-american"), 1, Haitian))

# unique(race.s.long$Other_specify)


## Coding student race across waves by multiple, priority, and mode approaches
race.s.codes <- catacode(race.s.long, id = STUDENT_ID, approach = "multiple",
                         Asian, Black, Native, Other, Pac_island, White, Dont_know, Hispanic, Haitian,
                         time = Wave, new.name = "Multiple") %>%
  left_join(catacode(race.s.long, id = STUDENT_ID, approach = "priority",
                     Asian, Black, Native, Other, Pac_island, White, Dont_know, Hispanic, Haitian,
                     priority = c('Hispanic','Haitian'), time = Wave, new.name = "Priority")) %>%
  left_join(catacode(race.s.long, id = STUDENT_ID, approach = "mode",
                     Asian, Black, Native, Other, Pac_island, White, Dont_know, Hispanic, Haitian,
                     time = Wave, new.name = "Mode")) %>%
  left_join(catacode(race.s.long, id = STUDENT_ID, approach = "priority",
                     Asian, Black, Native, Other, Pac_island, White, Dont_know, Hispanic, Haitian,
                     priority = c('Haitian', "Hispanic"), time = Wave, new.name = "Priority-Haitian"))
  

## Plotting comparison
race.s.summary <- race.s.codes %>%
  gather(Approach, Identity, -STUDENT_ID) %>%
  group_by(Approach, Identity) %>%
  summarize(Count = n())

race.s.plot <- race.s.summary %>%
  ggplot(aes(x = Identity, y = Count, fill = Identity)) +
  geom_col() +
  geom_text(aes(label = Count, vjust = -.1)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Approach) +
  guides(fill = "none")

## How many students changed identity across waves?
race.s.all <- catacode(race.s.long, id = STUDENT_ID, approach = "all", Asian:Native, Other:Haitian, time = Wave, new.name = "Race_Ethnicity")
race.s.all.wide <- race.s.all %>%
  pivot_wider(names_from = Wave, values_from = Race_Ethnicity, names_prefix = "W") %>%
  mutate(Same = ifelse(W1 == W2, 1, 0),
         Missing = case_when(is.na(W1) & is.na(W2) ~ 1,
                             is.na(W1) & W2 == "Dont_know" ~ 1,
                             W1 == "Dont_know" & W2 == "Dont_know" ~ 1,
                             W1 == "Dont_know" & is.na(W2) ~ 1,
                             TRUE ~ 0))
table(race.s.all.wide$Same, useNA = "always") %>% sum()
# 182 changed; 57 stayed the same; 45 had NA in one wave
# race.s.all.wide %>% filter(Same == 0) %>% View()
# many cases were mixed in W1 and picked one of the W1 identities for W2; mostly involving Hispanic

# Which students are missing race?
race.s.miss <- race.s.all.wide[race.s.all.wide$Missing==1, ]$STUDENT_ID # 7 students


####   Gender   ####

## student gender data in long format and re-coding jokester responses
gen.s.long <- dial.long %>%
  filter(!is.na(ST_DATE)) %>%
  select(Wave, STUDENT_ID, starts_with('ST_GENDER')) %>%
  unique() %>%
  mutate(ST_GENDER = as_factor(ST_GENDER) %>% as.character(),
         ST_GENDER = ifelse(ST_GENDER_OTHER_SPECIFY %in% c("Useless", "Attack Helicopter", "Publix bag/mayonaise", "Mummy Pig"), NA, ST_GENDER))

# unique(gen.s.long$ST_GENDER_OTHER_SPECIFY)

## coding gender - mode approach not relevant b/c this is only 2 waves and not CATA
gen.s.codes <- catacode(gen.s.long, id = STUDENT_ID, approach = "multiple", ST_GENDER, time=Wave,new.name = "Multiple") %>%
  left_join(catacode(gen.s.long, id = STUDENT_ID, approach = "priority", ST_GENDER,priority = c('Other (please describe)'), time = Wave, new.name = "Priority")) %>%
  left_join(catacode(gen.s.long, id = STUDENT_ID, approach = "mode", ST_GENDER, time=Wave, new.name = "Mode"))

## Plotting comparison
gen.s.summary <- gen.s.codes %>%
  gather(Approach, Identity, -STUDENT_ID) %>%
  group_by(Approach, Identity) %>%
  summarize(Count = n())

gen.s.plot <- gen.s.summary %>%
  ggplot(aes(x = Identity, y = Count, fill = Identity)) +
  geom_col() +
  geom_text(aes(label = Count, vjust = -.1)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Approach) +
  guides(fill = "none")

## What kind of responses were coded as multiple and NA?
gen.s.multiple <- gen.s.codes[gen.s.codes$Multiple %in% c("Multiple", "Other (please describe)")|is.na(gen.s.codes$Multiple), ]$STUDENT_ID
# dial %>% filter(STUDENT_ID %in% gen.s.multiple) %>%
#   select(STUDENT_ID, starts_with("ST_GENDER")) %>% 
#   mutate(across(starts_with("ST_GENDER"), as_factor)) %>% View()


######################################################

# ----------- Teacher Report of Students -------------

####   Race/Ethnicity   ####

tr.other.specify <- c("Black and White", "Hispanic", "hispanic", "HISPANIC", "Haitian")

race.tr.long <- dial.long %>%
  filter(!is.na(TR_DATE)) %>%
  select(Wave, STUDENT_ID, TEACHER_ID, starts_with('TR_RACE')) %>%
  unique() %>%
  rename_with(~str_remove(., "TR_RACE_") %>% str_to_title(), .cols = starts_with("TR_RACE_")) %>%
  mutate(across(.cols = -c(Wave, STUDENT_ID, TEACHER_ID, Other_specify), as.numeric)) %>%
  mutate(Other = if_else(Other_specify %in% tr.other.specify, 0, Other),
         White = if_else(Other_specify %in% c("Black and white"), 1, White),
         Black = if_else(Other_specify %in% c("Black and white"), 1, Black),
         Hispanic = if_else(Other_specify %in% c("Hispanic", "hispanic", "HISPANIC"), 1, Hispanic),
         Haitian = if_else(Other_specify %in% c("Haitian"), 1, Haitian))

## Coding student race across waves by multiple, priority, and mode approaches
race.tr.codes <- catacode(race.tr.long, id = STUDENT_ID, approach = "multiple",
                         Asian:Other, time = Wave, new.name = "Multiple") %>%
  left_join(catacode(race.tr.long, id = STUDENT_ID, approach = "priority",
                     Asian:Other, priority = c('Haitian', 'Hispanic'), time = Wave, new.name = "Priority")) %>%
  left_join(catacode(race.tr.long, id = STUDENT_ID, approach = "mode",
                     Asian:Other, time = Wave, new.name = "Mode"))

## Plotting comparison
race.tr.summary <- race.tr.codes %>%
  gather(Approach, Identity, -STUDENT_ID) %>%
  group_by(Approach, Identity) %>%
  summarize(Count = n())

race.tr.plot <- race.tr.summary %>%
  ggplot(aes(x = Identity, y = Count, fill = Identity)) +
  geom_col() +
  geom_text(aes(label = Count, vjust = -.1)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Approach) +
  guides(fill = "none")

race.tr.all <- catacode(race.tr.long, id = STUDENT_ID, approach = "all", Asian:Other, time = Wave, new.name = "Race_Ethnicity") 
race.tr.all.wide <- race.tr.all %>%
  pivot_wider(names_from = Wave, values_from = Race_Ethnicity, names_prefix = "W") %>%
  mutate(Same = ifelse(W1==W2, 1, 0))
table(race.tr.all.wide$Same, useNA = "always") %>% sum()
# 227 changed; 159 stayed the same; 86 had NA in one wave
# race.t.all.wide %>% filter(Same == 0) %>% View()



## Compare student self-response to teacher response
race.str <- race.s.codes %>%
  rename_with(.cols = -STUDENT_ID, ~paste0("ST_", .x)) %>%
  left_join(race.tr.codes)

####   Gender   ####

## student gender data in long format
gen.tr.long <- dial.long %>%
  filter(!is.na(TR_DATE)) %>%
  select(Wave, STUDENT_ID, starts_with('TR_GENDER')) %>%
  unique() %>%
  mutate(TR_GENDER = as_factor(TR_GENDER) %>% as.character())

# table(gen.tr.long$TR_GENDER) # nobody coded as Other

gen.tr.codes <- catacode(gen.tr.long, id = STUDENT_ID, approach = "multiple", TR_GENDER, time=Wave,new.name = "Multiple") %>%
  # left_join(catacode(gen.tr.long, id = STUDENT_ID, approach = "priority", TR_GENDER,priority = c('Other (please describe)'), time = Wave, new.name = "Priority")) %>%
  left_join(catacode(gen.tr.long, id = STUDENT_ID, approach = "mode", TR_GENDER, time=Wave, new.name = "Mode"))

## Plotting comparison
gen.tr.summary <- gen.tr.codes %>%
  gather(Approach, Identity, -STUDENT_ID) %>%
  group_by(Approach, Identity) %>%
  summarize(Count = n())

gen.tr.plot <- gen.tr.summary %>%
  ggplot(aes(x = Identity, y = Count, fill = Identity)) +
  geom_col() +
  geom_text(aes(label = Count, vjust = -.1)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Approach) +
  guides(fill = "none")

## What kind of responses were coded as multiple and NA?
gen.tr.multiple <- gen.tr.codes[gen.tr.codes$Multiple %in% c("Multiple")|is.na(gen.tr.codes$Multiple), ]$STUDENT_ID
# dial %>% filter(STUDENT_ID %in% gen.tr.multiple) %>%
#   select(STUDENT_ID, TEACHER_ID, starts_with("TR_GENDER")) %>% 
#   mutate(across(starts_with("TR_GENDER"), as_factor)) %>% View()


# -----  Creating final demographic codes --------
## do we need to add variable labels?
no.label <- unlist(lapply(names(dial), function(x) is.null(attr(dial[[x]], which = "label"))))
table(no.label)
names(dial)[no.label]


## joining final race and gender codes,
## then prepping dataset for final output
dial.demo <- dial %>%
  left_join(race.t.codes %>%
              select(TEACHER_ID, T_Race = Priority), by = "TEACHER_ID") %>%
  left_join(gen.t.codes %>%
              select(TEACHER_ID, T_Gender = Multiple), by = "TEACHER_ID") %>%
  left_join(race.s.codes %>%
              select(STUDENT_ID, ST_Race = `Priority-Haitian`), by = "STUDENT_ID") %>%
  left_join(gen.s.codes %>%
              select(STUDENT_ID, ST_Gender = Multiple), by = "STUDENT_ID") %>%
  left_join(race.tr.codes %>%
              select(STUDENT_ID, TR_Race = Priority), by = "STUDENT_ID") %>%
  left_join(gen.tr.codes %>%
              select(STUDENT_ID, TR_Gender = Multiple), by = "STUDENT_ID") %>%
  # left_join(exceptionality, by = "STUDENT_ID") %>%
  mutate(across(.cols = c(T_Gender, ST_Gender, TR_Gender), ~ifelse(. == "Multiple", "Other", .))) %>%
  mutate(across(T_Race:TR_Gender, factor)) %>%
  labelled::set_variable_labels(DIAL = "Intervention indicator",
                                T_Race = "Teacher race coded across waves prioritizing Hatian and Hispanic responses",
                                ST_Race = "Student race coded across waves prioritizing Hatian and Hispanic responses",
                                TR_Race = "Teacher report of student race coded across waves prioritizing Hatian and Hispanic responses",
                                T_Gender = "Teacher gender coded across waves",
                                ST_Gender = "Student gender coded across waves",
                                TR_Gender = "Teacher report of student gender coded across waves") %>%
  select(STUDY_ID:CONDITION, DIAL, all_of(idplus.vars), T_Race:TR_Gender,
         starts_with("TR_"), starts_with("T_"), starts_with("ST_"), everything())

## Coding final student race, gender, and ELL status across the various data sources
dial.s <- dial.demo %>%
  filter(!is.na(STUDENT_ID)) %>% # 5 teachers had self-reports without a student
  # select(-starts_with("T_")) %>% # need for mediation
  mutate(S_Race = case_when(ST_Race == "Dont_know" ~ as_factor(TR_Race),     # using Teacher code for students' "Dont know" responses
                            !is.na(ST_Race) ~ as_factor(ST_Race),  # use Student code when available
                            !is.na(TR_Race) ~ as_factor(TR_Race)), # otherwise use Teacher code 
         S_Gender = case_when(!is.na(ST_Gender) ~ as_factor(ST_Gender),     # use Student code when available
                              !is.na(TR_Gender) ~ fct_recode(as_factor(TR_Gender), Boy = "Male", Girl = "Female")), # otherwise use Teacher code
         ELL = case_when(!is.na(TR_ESOL_W2) ~ as_factor(TR_ESOL_W2),
                         !is.na(ST_ENGLISH_LEARNER_W2) ~ as_factor(ST_ENGLISH_LEARNER_W2)) %>%
           fct_drop())

dial.demo <- dial.demo %>%
  left_join(dial.s %>%
              select(STUDENT_ID, S_Race, S_Gender, ELL), by = "STUDENT_ID") %>%
  labelled::set_variable_labels(S_Race = "Student race coded across sources prioritizing ST_Race then TR_Race",
                                S_Gender = "Student gender coded across sources prioritizing ST_Gender then TR_Gender",
                                ELL = "ELL coded across souces priortizing TR_ESOL_W2 then ST_ENGLISH_LEARNER_W2") %>%
  select(STUDY_ID:T_WAVES_PRESENT, S_Race, S_Gender, ELL, everything())


##################################################

#######################################################################


# -----    Exporting Final Dataset    -----------

## Exporting updated dataset to SPSS
# NOTE: UPDATE FILE DATE BEFORE EXPORTING ##
# haven::write_sav(data = dial.demo, path = "00 Data/COMBINED T1_T2/DIAL_ALL_DATA_AND_SCORES_x-x-23.sav")

## Creating Codebook
# dial.demo2 <- read_sav( "00 Data/COMBINED T1_T2/DIAL_ALL_DATA_AND_SCORES_x-x-23.sav") # need to read back in so factors get treated as labelled, rather than doing it manually above
# Create_Codebook(dial.demo2, export_type = "excel", export_name = "00 Data/COMBINED T1_T2/DIAL_ALL_DATA_AND_SCORES_x-x-23_Codebook")

## Gathering unique teacher data since teacher rows were repeated in combined dataset
dial.t <- dial.demo %>%
  select(TEACHER_ID, DIAL, CONDITION, SCHOOL_ID,
         T_WAVES_PRESENT, starts_with("T_")) %>%  # keeping only teacher ID and self-report items
  unique() %>% # dropping duplicates here
  filter(!is.na(T_DATE_W1) | !is.na(T_DATE_W2)) # dropping empty rows (some teachers did not complete self-report either wave)
  
# NOTE: UPDATE FILE DATE BEFORE EXPORTING ##
# haven::write_sav(data = dial.t, path = "00 Data/COMBINED T1_T2/DIAL_TEACHER_DATA_SCORES_X-X-23.sav")

# curious <- haven::read_sav("00 Data/COMBINED T1_T2/DIAL_ALL_DATA_AND_SCORES_3-6-23.sav")



# ------   Saving Demographic and Psychometric Information for Reporting --------------
# Reporting document is 2b-Psychometric_Score_Report.rmd
save(tst.cfas, tst.br, tst.fits, tst.relis, tst.loadings, tst.comments,
     tslf.cfas, tslf.fits, tslf.relis, tslf.loadings, tslf.comments,
     sslf.cfas, sslf.fits, sslf.relis, sslf.loadings, sslf.comments,
     race.t.long, race.t.codes, race.t.summary, race.t.plot, race.t.all.wide,
     gen.t.long, gen.t.codes, gen.t.summary, gen.t.plot,
     race.s.long, race.s.codes, race.s.summary, race.s.plot, race.s.all.wide,
     gen.s.long, gen.s.codes, gen.s.summary, gen.s.plot,
     race.tr.long, race.tr.codes, race.tr.summary, race.tr.plot, race.tr.all.wide,
     gen.tr.long, gen.tr.codes, gen.tr.summary, gen.tr.plot,
     file = "Psychometric Reports/Score_Report_Results.RData")
# load("Psychometric Reports/Score_Report_Results.RData")

################################################

