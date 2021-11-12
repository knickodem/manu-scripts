##############################################################################
#                                                                            #
#    Cumulative Lifetime Adversities in first generation Latinx population   #
#                          Late July 2019                                    #
#                                                                            #
##############################################################################

## Loading Packages and Functions
source("PackFunc.R")

## Factor Levels
LatinxLevs <- c("Mexico","Puerto Rico","Central America","Cuba","Dominican","South American")

## Loading Original Data
TIPOrig <- read_sav("Data for Trauma Invariance Project_Kyle.sav") %>%
  mutate(Group = case_when(IMEA2 == "0" ~ "Mexico",
                             IMEA2 == "1" ~ "Puerto Rico",
                             IMEA2 == "2" ~ "Central America",
                             IMEA2 == "3" ~ "Cuba",
                             IMEA2 == "4" ~ "Dominican",
                             IMEA2 == "5" ~ "South American") %>%
           factor(.,levels = LatinxLevs))

## Exporting codebook
# Create_Codebook(TIPOrig, export_type = "csv",export_name = "TIP_Codebook", keep_R_Object = FALSE)

#### Identifying which items belong to which construct/factor ####
LVItemsDF <- read_csv("TIP_Codebook_Revised.csv") %>%
  select(LV,Item) %>%
  filter(!is.na(LV))

LVItemsList <- map(unique(LVItemsDF$LV), ~LVItemsDF[LVItemsDF$LV==.x,]$Item) %>%
  set_names(unique(LVItemsDF$LV))


##################################################
#### Preliminary Descriptives and Frequencies ####

#### Item Frequencies ####
## by Group
IFbG <- TIPOrig %>% select(Group, one_of(LVItemsDF$Item)) %>%
  gather(Item,Response,-Group) %>%
  group_by(Group,Item,Response) %>%
  summarize(n = n()) %>%
  spread(Response, n) %>% ungroup() %>%
  mutate_if(is.numeric,~replace_na(.,0))

## by language preference
IFbLP <- TIPOrig %>% select(LANG_PREF, one_of(LVItemsDF$Item)) %>%
  gather(Item,Response,-LANG_PREF) %>%
  group_by(LANG_PREF,Item,Response) %>%
  summarize(n = n()) %>%
  spread(Response, n) %>% ungroup() %>%
  mutate_if(is.numeric,~replace_na(.,0))

#### Raw sum score calculation ####
## Calculating raw sum scores for analytic sample (those with complete responses on all scales)
AnalyticSample <- TIPOrig %>% mutate(ACE = rowSums(.[LVItemsList$ACE]),
                         Per = rowSums(.[LVItemsList$Perceived]),
                         Chr = rowSums(.[LVItemsList$Chronic]),
                         Dis = rowSums(.[LVItemsList$Discrimination]),
                         Acc = rowSums(.[LVItemsList$Acculturation]),
                         CLA = rowSums(.[LVItemsDF$Item])) %>%
  filter_at(vars(LVItemsDF$Item), all_vars(!is.na(.)))

## Sample with missing data on any item
MissingSample <- TIPOrig %>% mutate(ACE = rowSums(.[LVItemsList$ACE]),
                                    Per = rowSums(.[LVItemsList$Perceived]),
                                    Chr = rowSums(.[LVItemsList$Chronic]),
                                    Dis = rowSums(.[LVItemsList$Discrimination]),
                                    Acc = rowSums(.[LVItemsList$Acculturation]),
                                    CLA = rowSums(.[LVItemsDF$Item])) %>%
  filter_at(vars(LVItemsDF$Item), any_vars(is.na(.)))
  
#### Raw sum score descriptives ####
## Analytic sample
AnalyticSampRawDescrips <- AnalyticSample %>%
  gather(Scale, Score, ACE:CLA) %>%
  Get_Descriptives(Score, Scale, digits = 2) %>% mutate(Group = "Full Sample") %>%
  bind_rows(AnalyticSample %>% gather(Scale, Score, ACE:CLA) %>%
              Get_Descriptives(Score, Group, Scale, digits = 2))
# For publication
RawDescripsTable <- AnalyticSampRawDescrips %>%
  mutate(MSD = paste0(Mean, " (", SD, ")")) %>%
  select(Group, n, Scale, MSD) %>%
  spread(Scale, MSD) %>%
  select(Group, n, ACE, Per, Chr, Dis, Acc, CLA) %>%
  mutate(Group = factor(Group, levels = c("Full Sample",LatinxLevs))) %>%
  arrange(Group)

## ANOVAs and eta2G
Anovas <- purrr::map(c("ACE","Per","Chr","Dis","Acc","CLA"),~lm(as.formula(paste0(.x,"~ Group")),data = AnalyticSample)) %>%
  set_names(c("ACE","Perceived","Chronic","Discrimination","Acculturation","CLA"))
etas <- purrr::map2_dfr(Anovas,names(Anovas),~sjstats::eta_sq(.x, partial = FALSE) %>% mutate(Scale = .y)) %>%
  mutate_if(is.numeric,~round(.,2)) %>% spread(Scale,partial.etasq)
Fs <- purrr::map2_dfr(Anovas,names(Anovas),~anova(.x) %>% tidy() %>% mutate(Scale = .y,) %>% select(Scale,`F` = statistic, p = p.value)) %>%
  drop_na()

Combo <- Fs %>% select(-p) %>% spread(Scale,`F`) %>% bind_rows(etas) %>%
  select(ACE,Perceived,Chronic,Discrimination,Acculturation,CLA)

## Missing sample
MissingSampRawDescrips <- MissingSample %>%
  gather(Scale, Score, ACE:CLA) %>%
  Get_Descriptives(Score, Scale, digits = 2) %>% mutate(Group = "Full Sample") %>%
  bind_rows(MissingSample %>% gather(Scale, Score, ACE:CLA) %>%
              Get_Descriptives(Score, Group, Scale, digits = 2))

## Comparison
MissingAnalysis <- AnalyticSampRawDescrips %>%
  inner_join(MissingSampRawDescrips, by = c("Scale","Group"), suffix =c("_A","_Miss")) %>%
  mutate(Diff = Get_CohensD(Mean_A,Mean_Miss,SD_A, SD_Miss,n_A,n_Miss, sample = "ind")) # from Useful code.R


#### Raw score correlation for construct validity ####
RawCorrs <- AnalyticSample %>%
  select(CES_D:LIFE_ENGAGE, ACE:CLA) %>%
  rquery.cormat(type = "upper", digits = 3, graph = FALSE)


#### Demographic Descriptives ####
## Age
table(AnalyticSample$AGE >= 45, useNA = "always") %>% prop.table()
psych::describe(AnalyticSample$AGE)
## Years in US
psych::describe(AnalyticSample$YRSUS)
table(AnalyticSample$YRSUS >= 10, useNA = "always") %>% prop.table()
hist(AnalyticSample$YRSUS)

## Latinx subgroup
table(AnalyticSample$Group, useNA = "always")# %>% sum() 
table(AnalyticSample$INCOME_C3, useNA = "always") # 2328 / (2328 + 792)
table(AnalyticSample$MARITAL_STATUS, useNA = "always") # 1755 / (761 + 1755 + 775)
table(AnalyticSample$GENDERNUM, useNA = "always") %>% prop.table()
table(AnalyticSample$LANG_PREF, useNA = "always") %>% prop.table()
table(AnalyticSample$Group,AnalyticSample$LANG_PREF, useNA = "always") %>% prop.table(margin = 1) %>% round(3)

#### Save Prelim Data Analysis ####
save(LVItemsDF, LVItemsList, LatinxLevs,
     TIPOrig, IFbG, IFbLP,
     AnalyticSample, RawDescripsTable,
     MissingSample, MissingAnalysis, RawCorrs,
     file = "PrelimAnalysis.RData")

################################################################


#######################################
#### Setting up Measurement Models ####

#### Defining CFA measurement models ####

## Five separate factors
SeparateMods <- map2(LVItemsList, names(LVItemsList), ~paste(.y, "=~",paste(.x, collapse = " + ")))

## Correlated traits models with 5, 4, and 3 factors
CorrelatedTraits5Mod <- paste(SeparateMods, collapse = "\n ")
CorrelatedTraits4Mod <- paste(map2(LVItemsList[1:4],names(LVItemsList)[1:4],
                                   ~paste(.y, "=~",paste(.x, collapse = " + "))), collapse = "\n")
CorrelatedTraits3Mod <- paste(map2(LVItemsList[2:4], names(LVItemsList)[2:4],
                                   ~paste(.y, "=~",paste(.x, collapse = " + "))), collapse = "\n") # equivalent to dissertation model

## Single factor Cumulative Lifetime Adversities Model using items from 5, 4, or 3 subfactors
OneFactor5Mod <- paste("CLA =~",paste(LVItemsDF$Item, collapse = " + "))
OneFactor4Mod <- paste("CLA =~",paste(LVItemsDF[LVItemsDF$LV!="Acculturation",]$Item, collapse = " + "))
OneFactor3Mod <- paste("CLA =~",paste(LVItemsDF[LVItemsDF$LV %in% c("Chronic","ACE","Perceived"),]$Item, collapse = " + "))


## Bifactor Model with 5, 4, or 3 specific factors
Bifactor5Mod <- paste(CorrelatedTraits5Mod, OneFactor5Mod,sep = "\n ")
Bifactor4Mod <- paste(CorrelatedTraits4Mod, OneFactor4Mod,sep = "\n ")
Bifactor3Mod <- paste(CorrelatedTraits3Mod, OneFactor3Mod,sep = "\n ")


####################################################################


############################################################
#### Running CFA models with lavaan and mimicking Mplus ####
# Note: all items treated as categorical, so WLSMV estimator is used

#### Separate factors ####
SeparateFitLav <- map(SeparateMods, ~cfa(model = .x, data = select(TIPOrig,one_of(LVItemsDF$Item)) %>% drop_na(), ordered = LVItemsDF$Item))
SeparateFitMplus <- map(SeparateMods, ~cfa(model = .x, data = TIPOrig, ordered = LVItemsDF$Item,
                           mimic = "Mplus", missing = "pairwise"))
#### Correlated Traits ####
## Five factor
CorrelatedTraits5FitLav <- cfa(model = CorrelatedTraits5Mod, data = TIPOrig, ordered = LVItemsDF$Item)
CorrelatedTraits5FitMplus <-cfa(model = CorrelatedTraits5Mod, data = TIPOrig, ordered = LVItemsDF$Item,
                               mimic = "Mplus", missing = "pairwise")
## Four factor
CorrelatedTraits4FitLav <- cfa(model = CorrelatedTraits4Mod, data = TIPOrig, ordered = LVItemsDF$Item)#, test = "Satorra.Bentler")
CorrelatedTraits4FitMplus <-cfa(model = CorrelatedTraits4Mod, data = TIPOrig, ordered = LVItemsDF$Item,
                                mimic = "Mplus", missing = "pairwise")

## Three factor
CorrelatedTraits3FitLav <- cfa(model = CorrelatedTraits3Mod, data = TIPOrig, ordered = LVItemsDF$Item)
CorrelatedTraits3FitMplus <-cfa(model = CorrelatedTraits3Mod, data = TIPOrig, ordered = LVItemsDF$Item,
                                mimic = "Mplus", missing = "pairwise")

#### Single factor CLA ####
## Five factor
OneFactor5FitLav <- cfa(model = OneFactor5Mod, data = TIPOrig, ordered = LVItemsDF$Item)
OneFactor5FitMplus <-cfa(model = OneFactor5Mod, data = TIPOrig, ordered = LVItemsDF$Item,
                               mimic = "Mplus", missing = "pairwise")

## Four factor
OneFactor4FitLav <- cfa(model = OneFactor4Mod, data = TIPOrig, ordered = LVItemsDF$Item)
OneFactor4FitMplus <-cfa(model = OneFactor4Mod, data = TIPOrig, ordered = LVItemsDF$Item,
                         mimic = "Mplus", missing = "pairwise")

## Three factor
OneFactor3FitLav <- cfa(model = OneFactor3Mod, data = TIPOrig, ordered = LVItemsDF$Item)
OneFactor3FitMplus <-cfa(model = OneFactor3Mod, data = TIPOrig, ordered = LVItemsDF$Item,
                         mimic = "Mplus", missing = "pairwise")


#### Bifactor Model ####
## Five specific factors
Bifactor5FitLav <- cfa(model = Bifactor5Mod, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE)
Bifactor5FitMplus <-cfa(model = Bifactor5Mod, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE,
                                  mimic = "Mplus", missing = "pairwise")
# Bifactor5FitML <- cfa(model = Bifactor5Mod, data = TIPOrig, orthogonal=TRUE, std.lv=TRUE, estimator = "MLR",missing = "ML")
# summary(Bifactor5FitML,fit.measures = TRUE,standardized = TRUE)

## Four specific factors
Bifactor4FitLav <- cfa(model = Bifactor4Mod, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE)#, test = "Satorra.Bentler")
Bifactor4FitMplus <-cfa(model = Bifactor4Mod, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE,
                        mimic = "Mplus", missing = "pairwise")

## Three specific factors
Bifactor3FitLav <- cfa(model = Bifactor3Mod, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE)
Bifactor3FitMplus <-cfa(model = Bifactor3Mod, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE,
                        mimic = "Mplus", missing = "pairwise")


##################################################

##############################################################
#### Extracting and Saving Results for Measurement Models ####

# ## Quick summary printout for individual models
# summary(Bifactor5FitLav,standardized = TRUE, fit.measures = TRUE)
# summary(Bifactor5FitMplus,standardized = TRUE, fit.measures = TRUE)
# modindices(CorrelatedTraitsFitLav,sort. = TRUE)

#### Fit statistics and Total Reliabilities ####
CFAStats <- map2_dfr(SeparateFitLav, names(SeparateFitLav), ~Get_FitRels(.x, Mn = .y, Sn = "lavaan")) %>%
  bind_rows(map2_dfr(SeparateFitMplus, names(SeparateFitMplus), ~Get_FitRels(.x, Mn = .y, Sn = "Mplus"))) %>%
  bind_rows(Get_FitRels(CorrelatedTraits5FitLav, Mn = "Correlated Traits_5", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(CorrelatedTraits5FitMplus, Mn = "Correlated Traits_5", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(CorrelatedTraits4FitLav, Mn = "Correlated Traits_4", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(CorrelatedTraits4FitMplus, Mn = "Correlated Traits_4", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(CorrelatedTraits3FitLav, Mn = "Correlated Traits_3", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(CorrelatedTraits3FitMplus, Mn = "Correlated Traits_3", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(OneFactor5FitLav, Mn = "One Factor_5", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(OneFactor5FitMplus, Mn = "One Factor_5", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(OneFactor4FitLav, Mn = "One Factor_4", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(OneFactor4FitMplus, Mn = "One Factor_4", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(OneFactor3FitLav, Mn = "One Factor_3", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(OneFactor3FitMplus, Mn = "One Factor_3", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(Bifactor5FitLav, Mn = "Bifactor_5", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(Bifactor5FitMplus, Mn = "Bifactor_5", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(Bifactor4FitLav, Mn = "Bifactor_4", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(Bifactor4FitMplus, Mn = "Bifactor_4", Sn = "Mplus")) %>%
  bind_rows(Get_FitRels(Bifactor3FitLav, Mn = "Bifactor_3", Sn = "lavaan")) %>%
  bind_rows(Get_FitRels(Bifactor3FitMplus, Mn = "Bifactor_3", Sn = "Mplus"))

#### Reliabilities for all factors in the model ####
# For single factor models, the Total displayed in CFAStats is sufficient
AllReliabilities <- ScaleRelis(Bifactor5FitLav,"Bifactor_5", "lavaan") %>%
  bind_rows(ScaleRelis(Bifactor5FitMplus,"Bifactor_5", "Mplus")) %>%
  bind_rows(ScaleRelis(Bifactor4FitLav,"Bifactor_4", "lavaan")) %>%
  bind_rows(ScaleRelis(Bifactor4FitMplus,"Bifactor_4", "Mplus")) %>%
  bind_rows(ScaleRelis(Bifactor3FitLav,"Bifactor_3", "lavaan")) %>%
  bind_rows(ScaleRelis(Bifactor3FitMplus,"Bifactor_3", "Mplus")) %>%
  bind_rows(ScaleRelis(CorrelatedTraits5FitLav,"Correlated Traits_5", "lavaan")) %>%
  bind_rows(ScaleRelis(CorrelatedTraits5FitMplus,"Correlated Traits_5", "Mplus")) %>%
  bind_rows(ScaleRelis(CorrelatedTraits4FitLav,"Correlated Traits_4", "lavaan")) %>%
  bind_rows(ScaleRelis(CorrelatedTraits4FitMplus,"Correlated Traits_4", "Mplus")) %>%
  bind_rows(ScaleRelis(CorrelatedTraits3FitLav,"Correlated Traits_3", "lavaan")) %>%
  bind_rows(ScaleRelis(CorrelatedTraits3FitMplus,"Correlated Traits_3", "Mplus"))

#### Factor Loadings ####
AllLoadings <- ExtractParams(Bifactor5FitLav, "=~", "Bifactor_5_lav", standardized = TRUE) %>%
  left_join(ExtractParams(Bifactor5FitMplus, "=~", "Bifactor_5_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(Bifactor4FitLav, "=~", "Bifactor_4_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(Bifactor4FitMplus, "=~", "Bifactor_4_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(Bifactor3FitLav, "=~", "Bifactor_3_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(Bifactor3FitMplus, "=~", "Bifactor_3_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(OneFactor5FitLav, "=~", "One_Factor_5_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(OneFactor5FitMplus, "=~", "One_Factor_5_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(OneFactor4FitLav, "=~", "One_Factor_4_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(OneFactor4FitMplus, "=~", "One_Factor_4_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(OneFactor3FitLav, "=~", "One_Factor_3_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(OneFactor3FitMplus, "=~", "One_Factor_3_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(CorrelatedTraits5FitLav, "=~", "Correlated Traits_5_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(CorrelatedTraits5FitMplus, "=~", "Correlated Traits_5_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(CorrelatedTraits4FitLav, "=~", "Correlated Traits_4_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(CorrelatedTraits4FitMplus, "=~", "Correlated Traits_4_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(CorrelatedTraits3FitLav, "=~", "Correlated Traits_3_lav", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(ExtractParams(CorrelatedTraits3FitMplus, "=~", "Correlated Traits_3_Mplus", standardized = TRUE), by = c("Outcome", "Predictor" )) %>%
  left_join(map_dfr(SeparateFitLav,~ExtractParams(.x, "=~", "Separate_lav", standardized = TRUE)), by = c("Outcome", "Predictor")) %>%
  left_join(map_dfr(SeparateFitMplus,~ExtractParams(.x, "=~", "Separate_Mplus", standardized = TRUE)), by = c("Outcome", "Predictor")) %>%
  rename(Factor = Outcome, Item = Predictor)


#### Model Comparisons ####
ModComps <- Delta_Fit(CorrelatedTraits5FitLav,Bifactor5FitLav, modnames = c("Correlated Traits_5", "Bifactor_5")) %>%
  bind_rows(Delta_Fit(OneFactor5FitLav,Bifactor5FitLav, modnames = c("One Factor_5", "Bifactor_5"))) %>%
  bind_rows(Delta_Fit(CorrelatedTraits4FitLav,Bifactor4FitLav, modnames = c("Correlated Traits_4", "Bifactor_4"))) %>%
  bind_rows(Delta_Fit(OneFactor4FitLav,Bifactor4FitLav, modnames = c("One Factor_4", "Bifactor_4"))) %>%
  bind_rows(Delta_Fit(CorrelatedTraits3FitLav,Bifactor3FitLav, modnames = c("Correlated Traits_3", "Bifactor_3"))) %>%
  bind_rows(Delta_Fit(OneFactor3FitLav,Bifactor3FitLav, modnames = c("One Factor_3", "Bifactor_3")))

#### Bifactor indices for general factor/full model and for group factors ####
BIorder <- c("omega","omegaH","omegaHS","FD","H","ECV","PUC")
BfInd5 <- Bifactor_Indices(Bifactor5FitLav, "CLA") %>% mutate(Factor = "CLA") %>%
  bind_rows(map_dfr(names(LVItemsList),
                    ~Bifactor_Indices(Bifactor5FitLav,"CLA",.x) %>% mutate(Factor = .x))) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  select(Factor,one_of(BIorder))


# BfInd43 <- Bifactor_Indices(Bifactor4FitLav, "CLA") %>% mutate(Factor = "CLA.4") %>%
#   bind_rows(Bifactor_Indices(Bifactor3FitLav, "CLA") %>% mutate(Factor = "CLA.3")) %>%
#   bind_rows(map_df(names(LVItemsList)[1:4],~Bifactor_Indices(Bifactor4FitLav,"CLA",.x) %>%
#                      mutate(Factor = paste0(.x,".4"))) %>%
#               bind_rows(map_df(names(LVItemsList)[2:4],~Bifactor_Indices(Bifactor3FitLav,"CLA",.x) %>%
#                                  mutate(Factor = paste0(.x,".3"))))) %>%
#   mutate_if(is.numeric,~round(.,2)) %>%
#   select(Factor,one_of(BIorder))

# ## Calculating alpha, if needed
# alphas <- select(TIPOrig,one_of(LVItemsDF[LVItemsDF$LV!="Acculturation",]$Item)) %>% Get_Alpha("Bifactor_4") %>%
#   bind_rows(select(TIPOrig,one_of(LVItemsDF[LVItemsDF$LV %in% c("Chronic","ACE","Perceived"),]$Item)) %>% Get_Alpha("Bifactor_3"))


#### Exporting lavaan CFA results tables ####
# ## Fit and Model Comparisons
# CFAStats %>%
#   filter(Software == "lavaan" & Model %in% c("One Factor_5","One Factor_4","One Factor_3",
#                                              "Correlated Traits_5","Correlated Traits_4","Correlated Traits_3",
#                                             "Bifactor_5","Bifactor_4","Bifactor_3")) %>%
#   left_join(ModComps, by = c("Model"="Model1")) %>%
#   separate(Model,c("Structure","Scales_Used"),"_") %>%
#   select(Scales_Used,Structure,n,X2,CFI,RMSEA,SRMR,starts_with("Delta_")) %>% 
#   mutate(Structure = factor(Structure, levels = c("One Factor", "Correlated Traits", "Bifactor"))) %>%
#   arrange(desc(Scales_Used), Structure) %>%
#   write_csv(path = "CFAstats_lavaan.csv", na = "")

# ## 5 scale models only
# CFAStats5 <- Get_FitRels(OneFactor5FitLav, Mn = "Unidimensional", Sn = "lavaan", digs = 3) %>%
#   bind_rows(Get_FitRels(CorrelatedTraits5FitLav, Mn = "Multidimensional", Sn = "lavaan", digs = 3)) %>%
#   bind_rows(Get_FitRels(Bifactor5FitLav, Mn = "Bifactor", Sn = "lavaan", digs = 3))
# ModComps5 <- Delta_Fit(OneFactor5FitLav,Bifactor5FitLav, modnames = c("Unidimensional", "Bifactor"), digits = 3) %>%
#   bind_rows(Delta_Fit(CorrelatedTraits5FitLav,Bifactor5FitLav, modnames = c("Multidimensional", "Bifactor"), digits = 3))
# CFAStats5 %>%
#   left_join(ModComps5, by = c("Model"="Model1")) %>% 
#   select(Model,X2,pvalue,CFI,RMSEA,SRMR,starts_with("Delta_")) %>%
#   write_csv(path = "CFAstats5.csv", na = "")

## Separate Models
CFAStatsSep <- map2_dfr(SeparateFitLav, names(SeparateFitLav), ~Get_FitRels(.x, Mn = .y, Sn = "lavaan",digs = 3))

Get_FitRels(CorrelatedTraits5FitLav, Mn = "Correlated Traits_5", Sn = "lavaan", digs=3)

## Loadings and Bifactor stats
# Five scale model
AllLoadings %>% select(Factor,Item,Bifactor_5_lav) %>%
  spread(Factor,Bifactor_5_lav) %>%
  select(Item,CLA,ACE,Chronic,Perceived,Discrimination,Acculturation) %>%
  mutate(Item = factor(Item, levels = c(LVItemsList$ACE,LVItemsList$Chronic,LVItemsList$Perceived,
                                        LVItemsList$Discrimination,LVItemsList$Acculturation))) %>%
  arrange(Item) %>%
  mutate_at(vars(ACE:Acculturation),list(higher = ~CLA - .)) %>% View()
  bind_rows(BfInd5 %>% gather(Item,Value,-Factor) %>%
              spread(Factor,Value) %>% 
              mutate(Item = factor(Item,levels = BIorder)) %>%
              arrange(Item)) %>% 
  write_csv(path = "Loadings_BifactorStats5_lav.csv", na = "")
  
  
# # Four and three scale models
# select(AllLoadings[!str_detect(AllLoadings$Item,"ACEA"),],Factor,Item,Bifactor_4_lav) %>%
#   spread(Factor,Bifactor_4_lav) %>% select(Item,CLA,ACE,Chronic,Perceived,Discrimination) %>%
#   left_join(select(AllLoadings[!str_detect(AllLoadings$Item,"ACEA"),],Factor,Item,Bifactor_3_lav) %>%
#               spread(Factor,Bifactor_3_lav) %>% select(Item,CLA,ACE,Chronic,Perceived,Discrimination),
#             by = "Item", suffix = c(".4", ".3")) %>% 
#   mutate(Item = factor(Item, levels = c(LVItemsList$ACE,LVItemsList$Chronic,LVItemsList$Perceived,LVItemsList$Discrimination))) %>%
#   arrange(Item) %>%
#   bind_rows(BfInd %>% gather(Item,Value,-Factor) %>%
#               spread(Factor,Value) %>% 
#               mutate(Item = factor(Item,levels = BIorder)) %>%
#               arrange(Item)) %>% 
#   select(-Discrimination.3) %>%
#   write_csv(path = "Loadings_BifactorStats_lav.csv", na = "")


#############################################

#### Saving Measurement Models and Extracted Results ####
save(LVItemsList,SeparateFitLav,SeparateFitMplus,
     CorrelatedTraits5FitLav,CorrelatedTraits5FitMplus,CorrelatedTraits4FitLav,CorrelatedTraits4FitMplus,CorrelatedTraits3FitLav,CorrelatedTraits3FitMplus,
     OneFactor5FitLav,OneFactor5FitMplus,OneFactor4FitLav,OneFactor4FitMplus,OneFactor3FitLav,OneFactor3FitMplus,
     Bifactor5FitLav,Bifactor5FitMplus,Bifactor4FitLav,Bifactor4FitMplus,Bifactor3FitLav,Bifactor3FitMplus,
     CFAStats,AllReliabilities,AllLoadings,ModComps,BfInd5,
     file = "Measurement_Models.RData")

## Used in final analysis  
save(LVItemsDF, LVItemsList,
     SeparateMods,CorrelatedTraits5Mod,OneFactor5Mod,Bifactor5Mod,  # Model syntax
     SeparateFitLav,CorrelatedTraits5FitLav,OneFactor5FitLav,Bifactor5FitLav,   # Fitted Models
       BfInd5,#CFAStats,AllReliabilities,AllLoadings,ModComps,                   # Extracted Results
       file = "Final_Measurement_Models.RData")

load("Final_Measurement_Models.RData")
# rm(SeparateFitMplus,CorrelatedTraits5FitLav,CorrelatedTraits5FitMplus,CorrelatedTraits4FitLav,CorrelatedTraits4FitMplus,CorrelatedTraits3FitLav,CorrelatedTraits3FitMplus,
#    OneFactor5FitLav,OneFactor5FitMplus,OneFactor4FitLav,OneFactor4FitMplus,OneFactor3FitLav,OneFactor3FitMplus,
#    Bifactor5FitMplus,Bifactor4FitLav,Bifactor4FitMplus,Bifactor3FitLav,Bifactor3FitMplus)




###################################################
####  Measurement Invariance by Country/Region ####

#### Collapsing response categories in order to run the multigroup analysis ####
## Identifying items and categories
Collapse54 <- c("DCEA1","DCEA3","DCEA6","DCEA7","DCEA10","DCEA15","DCEA16","DCEA17",
                "ACEA11S")
Collapse43 <- c("DCEA15","DCEA16")
Collapse01 <- c("ACEA6S","ACEA8S","ACEA11S","ACEA13S")

## Recoding data
TIPmi <- TIPOrig %>%
  mutate_at(vars(one_of(Collapse54)),~car::recode(.,"5=4")) %>%
  mutate_at(vars(one_of(Collapse43)),~car::recode(.,"4=3")) %>%
  mutate_at(vars(one_of(Collapse01)),~car::recode(.,"0=1"))


#### lavaan ####
## Configural Model
Bifactor5FitLavConfig <-cfa(model = Bifactor5Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group")

Bifactor4FitLavConfig <-cfa(model = Bifactor4Mod, data = TIPmi, ordered = LVItemsDF$Item,
                              orthogonal=TRUE, std.lv=TRUE, 
                              group = "Group")

Bifactor3FitLavConfig <-cfa(model = Bifactor3Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group")

## Metric Model
Bifactor5FitLavMetric <-cfa(model = Bifactor5Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group", group.equal = c("loadings"))

Bifactor4FitLavMetric <-cfa(model = Bifactor4Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group", group.equal = c("loadings"))

Bifactor3FitLavMetric <-cfa(model = Bifactor3Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group", group.equal = c("loadings"))

## Scalar Model
Bifactor5FitLavScalar <-cfa(model = Bifactor5Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group", group.equal = c("loadings","thresholds"))

Bifactor4FitLavScalar <-cfa(model = Bifactor4Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group", group.equal = c("loadings","thresholds"))

Bifactor3FitLavScalar <-cfa(model = Bifactor3Mod, data = TIPmi, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "Group", group.equal = c("loadings","thresholds"))

## Modification Indices for 5 scale Scalar model
# Bifactor5ModInd <- modindices(Bifactor5FitLavScalar, sort. = TRUE, op="|", maximum.number = 10) # vector too big to save

# Examining possible threshold discrepancies manually
LatinxGroups <- c("Mexico","Puerto Rico","Central America","Cuba","Dominican","South American")
ExaminingThresholds <- ExtractParams(Bifactor5FitLavMetric,"|","Threshold",include.groups = TRUE,digits = 4) %>%
  group_by(Outcome,Predictor) %>%
  summarize(SD = sd(Threshold),
            Range = max(Threshold) - min(Threshold))

# ## Partial Scalar Model
# # Does not finish running after 2+ hours
# Bifactor5FitLavPartial <-cfa(model = Bifactor5Mod, data = TIPmi, ordered = LVItemsDF$Item,
#                             orthogonal=TRUE, std.lv=TRUE, 
#                             group = "Group", group.equal = c("loadings","thresholds"),
#                             group.partial = c("DCEA1|t3"))


#### Extracting Fit Indices and Model Comparison Results ####
## Model Fit
MIfitstats5 <- Get_FitRels(Bifactor5FitLavConfig, Mn = "Configural", Sn = "lavaan", relis = FALSE, digs = 3) %>%
  bind_rows(Get_FitRels(Bifactor5FitLavMetric, Mn = "Metric", Sn = "lavaan", relis = FALSE, digs = 3)) %>%
  bind_rows(Get_FitRels(Bifactor5FitLavScalar, Mn = "Scalar", Sn = "lavaan", relis = FALSE, digs = 3))
  
  # Get_FitRels(Bifactor4FitLavConfig, Mn = "Bifactor_4_Configural", Sn = "lavaan",relis = FALSE) %>%
  # bind_rows(Get_FitRels(Bifactor3FitLavConfig, Mn = "Bifactor_3_Configural", Sn = "lavaan",relis = FALSE)) %>%
  # bind_rows(Get_FitRels(Bifactor4FitLavMetric, Mn = "Bifactor_4_Metric", Sn = "lavaan",relis = FALSE)) %>%
  # bind_rows(Get_FitRels(Bifactor3FitLavMetric, Mn = "Bifactor_3_Metric", Sn = "lavaan",relis = FALSE)) %>%
  # bind_rows(Get_FitRels(Bifactor4FitLavScalar, Mn = "Bifactor_4_Scalar", Sn = "lavaan",relis = FALSE)) %>%
  # bind_rows(Get_FitRels(Bifactor3FitLavScalar, Mn = "Bifactor_3_Scalar", Sn = "lavaan",relis = FALSE))

## Model Comparisons
MIModComp5 <- Delta_Fit(Bifactor5FitLavConfig, Bifactor5FitLavMetric, modnames = c("Configural","Metric"), digits = 3) %>%
  bind_rows(Delta_Fit(Bifactor5FitLavMetric, Bifactor5FitLavScalar, modnames = c("Metric","Scalar"), digits = 3))
  
  # Delta_Fit(Bifactor4FitLavConfig, Bifactor4FitLavMetric, modnames = c("Bifactor_4_Configural","Bifactor_4_Metric")) %>%
  # bind_rows(Delta_Fit(Bifactor4FitLavMetric ,Bifactor4FitLavScalar, modnames = c("Bifactor_4_Metric","Bifactor_4_Scalar"))) %>%
  # # bind_rows(Delta_Fit(Bifactor3FitLavConfig, Bifactor3FitLavMetric, modnames = c("Bifactor_3_Configural","Bifactor_3_Metric"))) %>%
  # bind_rows(Delta_Fit(Bifactor3FitLavMetric, Bifactor3FitLavScalar, modnames = c("Bifactor_3_Metric","Bifactor_3_Scalar")))


MIfitstats5 %>%
  left_join(MIModComp5, by = c("Model" = "Model2")) %>%
  select(-Software) %>%
  write_csv(path = "MIModComps5_lav.csv", na = "")


#### Saving measurement invariance models and results ####
save(Bifactor5FitLavConfig,Bifactor4FitLavConfig,Bifactor3FitLavConfig,
     Bifactor5FitLavMetric,Bifactor4FitLavMetric,Bifactor3FitLavMetric,
     Bifactor5FitLavScalar,Bifactor4FitLavScalar,Bifactor3FitLavScalar,
     # Bifactor5FitLavPartial,
     MIfitstats,MIModComp,MIfitstats5,MIModComp5,
     file = "Invariance_Models.RData")

load("Invariance_Models.RData")
rm(Bifactor4FitLavConfig,Bifactor3FitLavConfig,
   Bifactor4FitLavMetric,Bifactor3FitLavMetric,
   Bifactor4FitLavScalar,Bifactor3FitLavScalar)


#######################################################
#### Measurement invariance by language preference ####

Collapse54LP <- c("ACEA3S")
Collapse01LP <- c("ACEA3S")#,"ACEA8S","ACEA11S","ACEA13S")

## Recoding data
TIPmiLP <- TIPOrig %>%
  mutate_at(vars(one_of(Collapse54LP)),~car::recode(.,"5=4")) %>%
  # mutate_at(vars(one_of(Collapse43)),~car::recode(.,"4=3")) %>%
  mutate_at(vars(one_of(Collapse01LP)),~car::recode(.,"0=1"))


## Configural Model
Bifactor5FitConfigLP <-cfa(model = Bifactor5Mod, data = TIPmiLP, ordered = LVItemsDF$Item,
                            orthogonal=TRUE, std.lv=TRUE, 
                            group = "LANG_PREF")
## Metric Model
Bifactor5FitMetricLP <-cfa(model = Bifactor5Mod, data = TIPmiLP, ordered = LVItemsDF$Item,
                           orthogonal=TRUE, std.lv=TRUE, 
                           group = "LANG_PREF", group.equal = c("loadings"))
## Scalar Model
Bifactor5FitScalarLP <-cfa(model = Bifactor5Mod, data = TIPmiLP, ordered = LVItemsDF$Item,
                           orthogonal=TRUE, std.lv=TRUE, 
                           group = "LANG_PREF", group.equal = c("loadings","thresholds"))


## Model Fit
MIfitstats5LP <- Get_FitRels(Bifactor5FitConfigLP, Mn = "Configural", Sn = "lavaan", relis = FALSE, digs = 3) %>%
  bind_rows(Get_FitRels(Bifactor5FitMetricLP, Mn = "Metric", Sn = "lavaan", relis = FALSE, digs = 3)) %>%
  bind_rows(Get_FitRels(Bifactor5FitScalarLP, Mn = "Scalar", Sn = "lavaan", relis = FALSE, digs = 3))

## Model Comparisons
MIModComp5LP <- Delta_Fit(Bifactor5FitConfigLP, Bifactor5FitMetricLP, modnames = c("Configural","Metric"), digits = 3) %>%
  bind_rows(Delta_Fit(Bifactor5FitMetricLP, Bifactor5FitScalarLP, modnames = c("Metric","Scalar"), digits = 3))

MIfitstats5LP %>%
  left_join(MIModComp5LP, by = c("Model" = "Model2")) %>%
  select(-Software) %>% 
  write_csv(path = "MIModComps5_LangPreference.csv", na = "")


#############################################################


#################################################
#### Association with Mental Health Measures ####

#### Obtaining Factor Scores ####
## Factor scores
FactorScoresAppendEBM <- lavPredict(Bifactor5FitLav, type = "lv", method = "EBM")# , se = "standard", append.data = TRUE, assemble = TRUE)
FactorScoresML <- lavPredict(Bifactor5FitLav, type = "lv", method = "ML",append.data = TRUE)

## Appending to data
TIPwScores <- TIPOrig %>% drop_na(LVItemsDF$Item) %>%
  bind_cols(data.frame(FactorScoresAppendEBM)) %>%
  bind_cols(AnalyticSample %>% select(ace = ACE,Per,Chr,Dis,Acc,cla = CLA))

FinalCorrs <- TIPwScores %>%
  select(CES_D:LIFE_ENGAGE,ace:cla,CLA) %>%
  rquery.cormat(type = "lower", digits = 3, graph = FALSE)




#### Model Based Correlations ####
## Five Scale Bifactor model with correlations - Mental Health with General factor only
Bifactor5ModCorr1 <- paste(Bifactor5Mod,"Depression =~ CES_D","Anxiety =~ STAS",
                          "Self_Esteem =~ SELF_ESTM_SCALE","Engagement =~ LIFE_ENGAGE",
                          "CLA ~~ Depression","CLA ~~ Anxiety","CLA ~~ Self_Esteem","CLA ~~ Engagement", # All correlations orthogonal except these
                          "Depression ~~ Anxiety","Depression ~~ Self_Esteem","Depression ~~ Engagement",
                          "Anxiety ~~ Self_Esteem","Anxiety ~~ Engagement","Self_Esteem ~~ Engagement",
                          sep = "\n ")
# ## Five Scale Bifactor model with correlations - Mental Health with General Factor and Specific factors
# Bifactor5ModCorr2 <- paste(Bifactor5Mod,"Depression =~ CES_D","Anxiety =~ STAS",
#                            "Self_Esteem =~ SELF_ESTM_SCALE","Engagement =~ LIFE_ENGAGE",
#                            "CLA ~~ 0*Discrimination","CLA ~~ 0*Perceived","CLA ~~ 0*Chronic","CLA ~~ 0*ACE","CLA ~~ 0*Acculturation",   # All correlations estimated except these
#                            "Discrimination ~~ 0*Perceived","Discrimination ~~ 0*Chronic","Discrimination ~~ 0*ACE","Discrimination ~~ 0*Acculturation",
#                            "Perceived ~~ 0*Chronic","Perceived ~~ 0*ACE","Perceived ~~ 0*Acculturation",
#                            "Chronic ~~ 0*ACE","Chronic ~~ 0*Acculturation","ACE ~~ 0*Acculturation",sep = "\n ")
# 
# ## Using regression rather than latent variable correlations
# Bifactor5ModReg <- paste(Bifactor5Mod,"CES_D ~ CLA","STAS ~ CLA","SELF_ESTM_SCALE ~ CLA","LIFE_ENGAGE ~ CLA",sep = "\n")

## Running Correlation/Regression Model                           
Bifactor5FitCorr1 <- cfa(model = Bifactor5ModCorr1, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE)
# Bifactor5FitCorr2 <- cfa(model = Bifactor5ModCorr2, data = TIPOrig, ordered = LVItemsDF$Item, std.lv=TRUE)
# summary(Bifactor5FitCorr2,fit.measures=TRUE,standardized=TRUE)
# 
# Bifactor5FitReg <- cfa(model = Bifactor5ModReg, data = TIPOrig, ordered = LVItemsDF$Item, orthogonal=TRUE, std.lv=TRUE)

## Extracting Correlations
CLAcorrModFit <- Get_FitRels(Bifactor5FitCorr1, Mn = "Correlations", Sn = "lavaan", relis = FALSE)
CLAcorrs <- ExtractParams(Bifactor5FitCorr1,"~~","Correlations",standardized = TRUE)
CLAcorrs_FullInfo <- standardizedSolution(Bifactor5FitCorr1,type="std.lv") %>% filter(op=="~~")
# parameterEstimates(Bifactor5FitCorr1, standardized = TRUE) %>% filter(op == "~~") %>%
#   filter(lhs %in% CorOrder & lhs!=rhs)

# CLAcorrModFit2 <- Get_FitRels(Bifactor5FitCorr2, Mn = "Correlations", Sn = "lavaan", relis = FALSE)
# CLAcorrs2 <- ExtractParams(Bifactor5FitCorr2,"~~","Correlations",standardized = TRUE,digits=3) %>%
#   mutate(Outcome = factor(Outcome, levels = c("CLA","Depression","Anxiety","Self_Esteem","Engagement",
#                                               "ACE","Perceived","Chronic","Discrimination","Acculturation")))
# 
# ## Extracting Regressions
# CLAregModFit <- Get_FitRels(Bifactor5FitReg, Mn = "Regressions", Sn = "lavaan", relis = FALSE, digs = 3)
# CLAregs <- parameterEstimates(Bifactor5FitReg, standardized = TRUE, rsquare=TRUE) # produces same correlations reported in CLAcorrs, but sans direction and pvalues

## Exporting Correlation table
CorOrder <- c("CLA","Depression","Anxiety","Self_Esteem","Engagement")
# CLAcorrs %>%
#   filter(Outcome %in% CorOrder & Outcome!=Predictor)  %>% 
#   spread(Outcome, Correlations) %>%
#   bind_rows(data.frame(Predictor = "CLA")) %>%
#   mutate(Engagement = NA,
#          Predictor = factor(Predictor, levels = CorOrder)) %>%
#   select(Predictor,one_of(CorOrder)) %>%
#   arrange(Predictor) %>%
#   write_csv(path = "CLA_Correlations.csv", na = "")
#   


save(FactorScoresAppendEBM,FactorScoresML,TIPwScores,
     Bifactor5ModCorr1,Bifactor5ModCorr2,
     Bifactor5FitCorr1,Bifactor5FitCorr2,
     CLAcorrModFit,CLAcorrs,file = "Correlations.RData")
load("Correlations.RData")
