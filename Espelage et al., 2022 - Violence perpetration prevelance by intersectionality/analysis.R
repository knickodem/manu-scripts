########################################
#                                      #
#       Paper 3 - SV Perp Identities   #
#   Data Preparation and Exploration   #
#                                      #
########################################

## Loading packages and functions
source("PnF.R")

sos_w1_w4 <- read_sav("sources-of-strength_w1-w4_plus_demographics_by_wave_05_10_2021.sav")
dsa <- sos_w1_w4[which(!is.na(sos_w1_w4$CONDITION_W1)), ] %>%
  select(SUBJECT_ID, School = SCHOOLS_W1, Gender, Transgender, Race, SexOr, ends_with("_W1"))

table(rowSums(is.na(sos_w1_w4[paste0("CONDITION_W", 1:4)])) == 4)
# appears that everyone in this dataset participated in at least 1 wave
# then again, some may have all blank responses


## Scale Items (at wave 1)
svvars <- paste0(paste("SEX_VIOL_PERP", 1:13, sep = "_"), "_W1")
hncpvars <- paste0(paste("HOM_PERP", 1:5, sep = "_"), "_W1")
tdvvars <- paste0(paste("TDV_PERP", 1:3, sep = "_"), "_W1")
cybersexvars <- paste0(paste("CYBER_SEX_PERP", 1:3, sep = "_"), "_W1")
outcomevars <- c("Sexual_Violence", "Sexual_Harassment", "Unwanted_Sexual_Contact", "Forced_Sexual_Contact",
                 "Homophobic_Name_Calling", "Cyber_Sexual", "Teen_Dating_Violence")




# ---- Checking scale quality ---------------------

scales.k <- map2(.x = list(svvars, hncpvars, cybersexvars),
                 .y = c("svperp", "hncperp", "cybersex"),
                 ~cfa_wrapper(dsk, .x, .y, ordered = .x, missing = "pairwise")) %>%
  set_names(c("SV", "HNC", "Cybersex"))
scales.k$TDV <- cfa_wrapper(dsk[dsk$EVER_DATED_NOT_IN_SCALE_W1 == 1, ], tdvvars, "Teen_Dating_Violenceperp", ordered = tdvvars, missing = "pairwise")

scales.a <-  map2(.x = list(svvars, svvars[1:4], svvars[5:9], svvars[10:13], hncpvars, cybersexvars),
                  .y = c("svperp", "shperp", "unwanted", "forced", "hncperp", "cybersex"),
                  ~cfa_wrapper(dsa, .x, .y, ordered = .x, missing = "pairwise")) %>%
  set_names(c(outcomevars[-7]))
scales.a$Teen_Dating_Violence <- cfa_wrapper(dsa[dsa$EVER_DATED_NOT_IN_SCALE_W1 == 1, ], tdvvars, "tdvperp", ordered = tdvvars, missing = "pairwise")

lapply(scales.k, "[[", "reliabilities")
lapply(scales.a, "[[", "reliabilities")
lapply(scales.k, "[[", "fits")
lapply(scales.a, "[[", "fits")


################################################################


# ----- Create binary variables -------------------

## binary version of each survey item
dsa <- dsa %>%
  mutate(across(all_of(c(svvars, hncpvars, tdvvars, cybersexvars)),
                ~if_else(.x == 0, 0, 1),
                .names = '{.col}_b'))

# table(dsa$CYBER_SEX_PERP_3_W1_b, dsa$CYBER_SEX_PERP_3_W1, useNA = "always") # check if properly re-coded

## 1) sum score calculated for each scale. Students missing on all indicators get NA value
## 2) perpetration indicator created from sum score
dsa <- dsa %>%
  mutate(Sexual_Violence_Score = scale_score(., items = paste0(svvars, "_b"), type = "sum"),
         Sexual_Harassment_Score = scale_score(., items = paste0(svvars[1:4], "_b"), type = "sum"),
         Unwanted_Sexual_Contact_Score = scale_score(., items = paste0(svvars[5:9], "_b"), type = "sum"),
         Forced_Sexual_Contact_Score = scale_score(., items = paste0(svvars[10:13], "_b"), type = "sum"),
         Homophobic_Name_Calling_Score = scale_score(., items = paste0(hncpvars, "_b"), type = "sum"),
         Cyber_Sexual_Score = scale_score(., items = paste0(cybersexvars, "_b"), type = "sum"),
         Teen_Dating_Violence_Score = scale_score(., items = paste0(tdvvars, "_b"), type = "sum")) %>%
  mutate(across(Sexual_Violence_Score:Teen_Dating_Violence_Score,                 # Creating binary perpetration indicator
                ~if_else(.x == 0, 0, 1),
                .names = "{.col}_Perp")) %>%
  rename_with(.cols = ends_with("_Score_Perp"), ~str_remove(., "_Score_Perp"))

# table(dsa$Sexual_Violence_Score, dsa$Sexual_Violence, useNA = "always") # check if properly re-coded


################################################################

# ----- Finalizing analysis dataset ------------------------

dsa2 <- dsa %>%
  select(SUBJECT_ID:CONDITION_W1, Race_multiracial_w1:SexualOr_W1,
         ends_with("_b"), Sexual_Violence:Teen_Dating_Violence,
         dated = EVER_DATED_NOT_IN_SCALE_W1) %>%
  mutate(across(where(is.labelled), as_factor)) %>%
  mutate(
    Gender_rec_W1 = case_when(
      Gender_W1 == "Female" ~ "Female",
      Gender_W1 == "Male" ~ "Male",
      Gender_W1 == "Multiple" ~ "Other",
      Gender_W1 == "Other" ~ "Other"),
    race_aapi = case_when(
      Race_multiracial_w1 == "African American" ~ "African American",
      Race_multiracial_w1 == "Asian" ~ "AAPI",
      Race_multiracial_w1 == "Hispanic" ~ "Hispanic",
      Race_multiracial_w1 == "Multiracial" ~ "Multiracial",
      Race_multiracial_w1 == "Native American" ~ "Native American",
      Race_multiracial_w1 == "Pacific Islander" ~ "AAPI",
      Race_multiracial_w1 == "White" ~ "White"),
    four_way = ifelse(!is.na(Gender_rec_W1) & !is.na(race_aapi) & !is.na(SexualOr_W1) & !is.na(Trans_W1), paste(Gender_rec_W1, race_aapi, SexualOr_W1, Trans_W1, sep = " - "), NA),
    
    three_way =  ifelse(!is.na(Gender_rec_W1) & !is.na(race_aapi) & !is.na(SexualOr_W1), paste(Gender_rec_W1, race_aapi, SexualOr_W1, sep = " - "), NA),
    
    gender_X_race = ifelse(!is.na(Gender_rec_W1) & !is.na(race_aapi), paste(Gender_rec_W1, race_aapi, sep = " - "), NA),
    
    gender_X_sexOR = ifelse(!is.na(Gender_rec_W1) & !is.na(SexualOr_W1), paste(Gender_rec_W1, SexualOr_W1, sep = " - "), NA),
    
    gender_X_Trans = ifelse(!is.na(Gender_rec_W1) & !is.na(Trans_W1), paste(Gender_rec_W1, Trans_W1, sep = " - "), NA),
    
    race_X_sexOR = ifelse(!is.na(race_aapi) & !is.na(SexualOr_W1), paste(race_aapi, SexualOr_W1, sep = " - "), NA),
    
    race_X_Trans = ifelse(!is.na(race_aapi) & !is.na(Trans_W1), paste(race_aapi, Trans_W1, sep = " - "), NA),
    
    sexOR_X_Trans = ifelse(!is.na(SexualOr_W1) & !is.na(Trans_W1), paste(SexualOr_W1, Trans_W1, sep = " - "), NA))

dsa_daters <- filter(dsa2, dated == "Yes")


#######################################################

# ---- Create summary tables -------------------------

## overall
tb1a.ov <- get_tab(dsa2[rowSums(is.na(dsa2[outcomevars])) !=7,], NULL, outcomevars[-7])
tb2a.ov <- get_tab(dsa_daters[rowSums(is.na(dsa_daters[outcomevars])) !=7,], NULL, outcomevars[7])
overall1a <- tbl_stack(list(tb1a.ov, tb2a.ov)) %>%
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## gender
tb1a.gen <- get_tab(dsa2, "Gender_rec_W1", outcomevars[-7])
tb2a.gen <- get_tab(dsa_daters, "Gender_rec_W1", outcomevars[7])
by_gender1a <- tbl_stack(list(tb1a.gen, tb2a.gen)) %>%
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## race
tb1a.race <- get_tab(dsa2, "race_aapi", outcomevars[-7])      # Not TDV
tb2a.race <- get_tab(dsa_daters, "race_aapi", outcomevars[7]) # TDV with dated
by_race1a <- tbl_stack(list(tb1a.race, tb2a.race)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## sexual orientation
tb1a.so <- get_tab(dsa2, "SexualOr_W1", outcomevars[-7])      # Not TDV
tb2a.so <- get_tab(dsa_daters, "SexualOr_W1", outcomevars[7]) # TDV with dated
by_so1a <- tbl_stack(list(tb1a.so, tb2a.so)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## transgender
tb1a.trans <- get_tab(dsa2, "Trans_W1", outcomevars[-7])      # Not TDV
tb2a.trans <- get_tab(dsa_daters, "Trans_W1", outcomevars[7]) # TDV with dated
by_trans1a <- tbl_stack(list(tb1a.trans, tb2a.trans)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## gender x race
tb1a.gxr <- get_tab(dsa2, "gender_X_race", outcomevars[-7])      # Not TDV
tb2a.gxr <- get_tab(dsa_daters, "gender_X_race", outcomevars[7]) # TDV with dated
by_gxr1a <- tbl_stack(list(tb1a.gxr, tb2a.gxr)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## gender x sexOr
tb1a.gxs <- get_tab(dsa2, "gender_X_sexOR", outcomevars[-7])      # Not TDV
tb2a.gxs <- get_tab(dsa_daters, "gender_X_sexOR", outcomevars[7]) # TDV with dated
by_gxs1a <- tbl_stack(list(tb1a.gxs, tb2a.gxs)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## gender x transgender
tb1a.gxt <- get_tab(dsa2, "gender_X_Trans", outcomevars[-7])      # Not TDV
tb2a.gxt <- get_tab(dsa_daters, "gender_X_Trans", outcomevars[7]) # TDV with dated
by_gxt1a <- tbl_stack(list(tb1a.gxt, tb2a.gxt)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## race x sexor
tb1a.rxs <- get_tab(dsa2, "race_X_sexOR", outcomevars[-7])      # Not TDV
tb2a.rxs <- get_tab(dsa_daters, "race_X_sexOR", outcomevars[7]) # TDV with dated
by_rxs1a <- tbl_stack(list(tb1a.rxs, tb2a.rxs)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## race x transgender
tb1a.rxt <- get_tab(dsa2, "race_X_Trans", outcomevars[-7])      # Not TDV
tb2a.rxt <- get_tab(dsa_daters, "race_X_Trans", outcomevars[7]) # TDV with dated
by_rxt1a <- tbl_stack(list(tb1a.rxt, tb2a.rxt)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")

## sexor x transgender
tb1a.sxt <- get_tab(dsa2, "sexOR_X_Trans", outcomevars[-7])      # Not TDV
tb2a.sxt <- get_tab(dsa_daters, "sexOR_X_Trans", outcomevars[7]) # TDV with dated
by_sxt1a <- tbl_stack(list(tb1a.sxt, tb2a.sxt)) %>%          # join 2 tables
  modify_header(label = "Variable") %>%
  as_flex_table() %>%
  bold(part = "header")


names(outcomevars) <- outcomevars

## gender
con.gender <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$Gender_rec_W1, y = dsa2[[.x]], alpha = .01),
                      .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$Gender_rec_W1, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## race
con.race <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$race_aapi, y = dsa2[[.x]], alpha = .01),
                    .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$race_aapi, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## sexual orientation
con.so <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$SexualOr_W1, y = dsa2[[.x]], alpha = .01),
                  .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$SexualOr_W1, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## transgender
con.trans <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$Trans_W1, y = dsa2[[.x]], alpha = .01),
                     .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$Trans_W1, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## gender x race
con.gxr <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$gender_X_race, y = dsa2[[.x]], alpha = .01),
                   .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$gender_X_race, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))


## gender x sexual orientation
con.gxs <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$gender_X_sexOR, y = dsa2[[.x]], alpha = .01),
                   .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$gender_X_sexOR, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## gender x transgender
con.gxt <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$gender_X_Trans, y = dsa2[[.x]], alpha = .01),
                   .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$gender_X_Trans, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## race x sexor
con.rxs <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$race_X_sexOR, y = dsa2[[.x]], alpha = .01),
                   .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$race_X_sexOR, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## race x transgender
con.rxt <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$race_X_Trans, y = dsa2[[.x]], alpha = .01),
                   .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$race_X_Trans, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## sexual orientation x transgender
con.sxt <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$sexOR_X_Trans, y = dsa2[[.x]], alpha = .01),
                   .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$sexOR_X_Trans, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## three-way
con.3way <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$three_way, y = dsa2[[.x]], alpha = .01),
                    .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$three_way, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))

## four-way
con.4way <- map_dfr(.x = outcomevars[-7], ~contrasts.m(x = dsa2$four_way, y = dsa2[[.x]], alpha = .01),
                    .id = "Outcome") %>%
  bind_rows(contrasts.m(dsa_daters$four_way, y = dsa_daters[[outcomevars[[7]]]], alpha = .01) %>% mutate(Outcome = "Teen_Dating_Violence"))


save(ds2, dsa_daters, outcomevars,
     by_gender1a, by_race1a, by_so1a, by_trans1a,
     by_gxr1a, by_gxs1a, by_gxt1a, by_rxs1a, by_rxt1a, by_sxt1a,
     tb2a.gen, tb2a.race, tb2a.so, tb2a.trans,
     tb2a.gxr, tb2a.gxs, tb2a.gxt, tb2a.rxs, tb2a.rxt, tb2a.sxt,
     con.gender, con.race, con.so, con.trans,
     con.gxr, con.gxs, con.gxt, con.rxs, con.rxt,
     con.sxt, con.3way, con.4way,
     file = "Tables_and_Contrasts.RData")
