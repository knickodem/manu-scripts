################################
#                              #
#                              #
#    Analyze - Card Sorting    #
#                              #
#                              #
################################


# ------------------------------------------------------------------------
# Final Exam Data - Intervention v Control
# ------------------------------------------------------------------------

## Adding demographic variables for use in regression
# Need to double check Sex and redo analysis; brick, brown, hauer, thao, trett, rahne
StudentInfo <- read_excel("Spring 19 Updates/Final analysis sheets/StudentInfo.xlsx")

####################
#### Item Level ####

## Common Response
CR <- FinalExam %>%
  filter(!is.na(IC)) %>%
  group_by(IC, Item, Response) %>%
  summarize(n = n(),
            Correct = mean(Score, na.rm=TRUE))

#### % Correct by Item ####
## Intervention v Control
FEItemData <- FinalExam %>%
  filter(!is.na(IC)) %>%    #!is.na(Sex)                     # removing 2015 data, which will not be used for analysis
  # mutate(IC2 = as.character(IC),
  #        IC2 = ifelse(Semester == "Fall 2018","I2",IC2)) %>%
  Get_Descriptives(Score,IC,Item) %>% #Sex
  mutate_at(vars(Mean,SE,SD), ~.*100)
FEItemPlot <- FEItemData %>%
  ICcomparePlots("Mean","Percent Correct",0,100,10,xvar="Item",guideTF = TRUE) +    #,gvar="IC2"
  geom_errorbar(aes(ymin=Mean-1.96*SE,ymax=Mean+1.96*SE),color="black",width=.2)     # creates error bars
  # facet_wrap(~Sex,ncol=2)

## Male v Female
FEItemMF <- FinalExam %>%
  left_join(StudentInfo %>% select(ID,Semester,Sex2=Sex),by = c("ID","Semester")) %>%
  mutate(Gender = case_when(Sex2 == "F" ~ "Female",
                             Sex2 == "M" ~ "Male",
                             is.na(Sex2) ~ Sex)) %>%
  filter(!is.na(Gender) &!is.na(IC)) %>%                          # removing 2015 data, which will not be used for analysis
  Get_Descriptives(Score,Gender,Item) %>%
  mutate_at(vars(Mean,SE,SD), ~.*100)
# FEItemMF %>% select(Gender,Item,Mean) %>% spread(Gender,Mean) %>% mutate(Diff = Male - Female)

FEItemMFPlot <- FEItemMF %>%
  ICcomparePlots("Mean","Percent Correct",0,100,10,xvar="Item",gvar="Gender",guideTF = TRUE) +
  geom_errorbar(aes(ymin=Mean-1.96*SE,ymax=Mean+1.96*SE),color="black",width=.2)      # creates error bars

ggsave(FEItemMFPlot,file = "Final Exam Item Gender Differences.png",height = 6, width = 12)

##############################

############################
####     Total Score    ####

#### Calculating Exam Score ####
FinalExamScores <- FinalExam %>%
  filter(!is.na(IC)) %>%                          # removing 2015 data, which will not be used for analysis
  group_by(IC,Semester,ID,Sex) %>%
  summarize(Num_Responses = sum(!is.na(Score)),   # Some students did not respond to all 8 items
            Exam_Score = sum(Score, na.rm=TRUE),
            Exam_Percent = (Exam_Score / 8)*100) %>%     # 8 items on the exam
  ungroup() %>%
  left_join(StudentInfo %>% select(-IC), by = c("ID","Semester")) %>%  # Missing demographics: aweis005, montg369, rippx011, frees035; Missing exam: radke081
  mutate(Gender = case_when(Sex.y == "F" ~ "Female",
                            Sex.y == "M" ~ "Male",
                            is.na(Sex.y) ~ Sex.x)) %>%
  mutate(Female = ifelse(Gender == "Female", 1, 0),           # Creating regression variables
         Intervention = ifelse(IC == "Intervention", 1, 0),
         Student_of_Color = ifelse(Ethnicity == "White", 0, 1),
         FemInt = Female * Intervention)

# psych::describeBy(FinalExamScores$Exam_Score,FinalExamScores$IC)


#### Plotting ####
## Plotting total score density by Semester
FESemesterDensityPlot <- FinalExamScores %>%
  mutate(Semester = factor(Semester, levels = c("Spring 2016","Fall 2016","Fall 2017","Fall 2018"))) %>%
  Get_DenPlot("Exam_Score","Semester","Exam Score", xmin = 0, xmax = 8, xseq = 1)

## Plotting total score density by IC
FEICDensityPlot <- FinalExamScores %>%
  Get_DenPlot("Exam_Score","IC","Exam Score", "Condition", xmin = 0, xmax = 8, xseq = 1, fillcolors = c("#d8b365","#5ab4ac"))

ggsave(FEICDensityPlot, file = "Exam Score Density by Condition.png",height = 6, width = 6)


#### Regression ####

## Linear
FE.lm <- FinalExamScores %>%
  lm(Exam_Score ~ 1 + Intervention + Female + Cumulative_GPA + ACT_Math, data=.)
car::vif(FE.lm)
FEModelCheck <- ModelCheckPlots(FE.lm)
summary(FE.lm)

# ## Poisson
# FE.poi <- FinalExamScores %>%
#   glm(Exam_Score ~ 1 + Intervention + Female + FemInt, data=., family = "poisson")
# car::vif(FE.poi)
# FEModelCheckpoi <- ModelCheckPlots(FE.poi)
# summary(FE.poi)
# 
# Get_Fixed(FE.poi,"Final Exam", digits = 2, logistic="poisson")

## Export Results
FEprednames <- c("Intercept","Intervention","Female","Cumulative GPA","ACT Math")
FEcoefs <- Get_Fixed(FE.lm, "Final Exam", digits = 2,pnames = FEprednames) %>% ## Fixed effects
  mutate(Coef = paste0(Estimate, " [",Lower,", ",Upper,"]"),
         Coef = ifelse(Sig == "Yes", paste0(Coef, "*"), paste0(Coef, " "))) %>%
  select(Model, Predictor, Coef) %>%
  spread(Model, Coef) %>%
  mutate(Predictor = factor(Predictor,levels = FEprednames)) %>%
  arrange(Predictor) %>%
  bind_rows(All_R2(FE.lm, "Final Exam"))                 ## Model Fit

## Effect sizes (eta2)
# sjstats::eta_sq(FE.lm,partial=TRUE)
# # Power analysis from Gpower shows model had 80% power to detect effect of d=.5, f2 = .065, eta2=.06, which are medium effects


###########################


# ------------------------------------------------------------------------
# Pile-Level
# ------------------------------------------------------------------------

#####################################################################
#### Test of Proportions between Intervention and Control groups ####
# ## Didn't end up using this in the report
# PilePropTestData <- ThePiles %>% filter(!is.na(IC) & Name_Type=="function") %>%
#   group_by(IC,Time) %>%
#   summarize(Total_Piles2 = sum(Total_Piles),
#             Function = sum(Piles)) %>%
#   mutate(Not_Function = Total_Piles2-Function) %>%
#   ungroup()
# 
# ## At Pre
# PreICPropTest <- PilePropTestData %>% filter(Time=="Pre") %>%
#   select(Function,Not_Function) %>% as.matrix() %>%
#   prop.test(correct=FALSE)
# 
# ## At Post
# PostICPropTest <- PilePropTestData %>% filter(Time=="Post") %>%
#   select(Function,Not_Function) %>% as.matrix() %>%
#   prop.test(correct=FALSE)

###########################################################################

###############################################
#### Intervention/Control comparison plots ####

## Descriptive data
# Group-level averages
PileDescrips <- ThePiles %>% filter(!is.na(IC) & Name_Type=="function") %>%
  group_by(IC,Time) %>%
  summarize(Num_Groups = n(),
            TP_mean = mean(Total_Piles),
            TP_se = sd(Total_Piles)/sqrt(Num_Groups),
            Prcnt = mean(Percent),
            Per_se = sd(Percent)/sqrt(Num_Groups)) %>% ungroup()
# # Not aggregated by Group         
# PileDescrips <- ThePiles %>% filter(!is.na(IC) & Name_Type=="function") %>%
#   group_by(IC,Time) %>%
#   summarize(Num_Groups = n(),
#             Tot_Piles = sum(Total_Piles),
#             TP_sd = sd(Total_Piles),
#             Function_Piles = sum(Piles)) %>% ungroup() %>%
#   mutate(Prcnt = Function_Piles/Tot_Piles,
#          Per_se = sqrt((Percent*(1-Percent))/Tot_Piles),
#          TP_mean = Tot_Piles/Num_Groups,
#          TP_se = TP_sd/sqrt(Num_Groups)) %>%
#   mutate_at(vars(Prcnt,Per_se),~.*100)

## % of piles with function-based names Plot - Condition by Time
# Distribution
FunctionPilesPlot <- PileDescrips %>%
  ICcomparePlots(yvar = "Prcnt", yname = "Percent", 0, 105, 20, guideTF = TRUE) +
  geom_errorbar(aes(ymin=Prcnt-1.96*Per_se,ymax=Prcnt+1.96*Per_se),color="black",width=.2)                 # creates error bars
# Distribution
FunctionPilesDistPlot <- ThePiles %>%
  filter(Name_Type=="function" & !is.na(IC)) %>%
  mutate(Condition_x_Time = paste(IC,Time,sep = " - ")) %>%
  Get_DenPlot(xvar="Percent",fillvar = "Condition_x_Time","Percent","Condition x Time",0,100,10,"Paired")   
  

## Total Number of Piles - Condition by Time
# Mean Values
TotalPilesPlot <- PileDescrips %>%
  ICcomparePlots(yvar = "TP_mean", yname = "Piles", 0, 15, 3, guideTF = FALSE) +
  geom_errorbar(aes(ymin=TP_mean-1.96*TP_se,ymax=TP_mean+1.96*TP_se),color="black",width=.2)                  # creates error bars
# Distribution
TotalPilesDistPlot <- ThePiles %>%
  filter(Name_Type=="function" & !is.na(IC)) %>%
  mutate(Condition_x_Time = paste(IC,Time,sep = " - ")) %>%
  Get_DenPlot(xvar="Total_Piles",fillvar = "Condition_x_Time","Total Piles","Condition x Time",0,25,5,"Paired")  

#############################################################

############################
#### Regression Models  ####

## Thoughts
# Although the ICC is ~15% for total piles, it is hard to justify using a multilevel model with an overall sample size  of 54.
# Also, is a multilevel model really needed with only 2 time points? I think not.
# All the models point to the same conclusion too, which implies using the more simple model.

#### Single-level Models with Post as criterion ####
## For Total Piles
# Model
TotPiles.pre <- ThePiles %>% Get_WideRegData(Total_Piles, Name_Type == "function") %>%
  lm(Post ~ 1 + Intervention + Pre + PreInt, data=.)
# summary(TotPiles.pre)
# Model Check
car::vif(TotPiles.pre)
TotPilesModelPlots <- ModelCheckPlots(TotPiles.pre) # Not bad actually


## For Percent Function-based names
# Model
PerFunc.pre <- ThePiles %>% Get_WideRegData(Percent,Name_Type == "function") %>%
  lm(Post ~ 1 + Intervention + Pre + PreInt, data=.)
# summary(PerFunc.pre)
# Model Check
car::vif(PerFunc.pre)
PerFuncModelPlots <- ModelCheckPlots(PerFunc.pre) # Heteroscedastic residuals

## Combined Results
Pprednames <- FEprednames %>% str_replace("Female","Pre")
SLR2 <- All_R2(TotPiles.pre, "Total Piles") %>% 
  left_join(All_R2(PerFunc.pre, "Percent Function"))
SLcoefs <- Get_Fixed(TotPiles.pre, "Total Piles", digits = 2,pnames = Pprednames) %>%
  bind_rows(Get_Fixed(PerFunc.pre, "Percent Function", digits = 2,pnames = Pprednames)) %>%
  mutate(Coef = paste0(Estimate, " [",Lower,", ",Upper,"]"),
         Coef = ifelse(Sig == "Yes", paste0(Coef, "*"), paste0(Coef, " "))) %>%
  select(Model, Predictor, Coef) %>%
  spread(Model, Coef) %>%
  bind_rows(SLR2) %>%
  select(Predictor, `Total Piles`,`Percent Function`)

## Effect sizes (eta2)
# # Total Piles
# sjstats::eta_sq(TotPiles.pre,partial=TRUE)
# # Percent Function
# sjstats::eta_sq(PerFunc.pre,partial=TRUE)
# Power analysis from Gpower shows model had 80% power to detect effect of d=1.13, f2 = .32, eta2=.24, which are big effects


#### Multilevel Models #####
# 
# ## Formatting data for Multilevel models
# PileMultiRegdata <- ThePiles %>% ungroup() %>%
#   filter(!is.na(IC) & Name_Type=="function") %>%
#   mutate(Intervention = ifelse(IC=="Intervention",1,0),
#          Post = ifelse(Time=="Post",1,0),
#          PostInt = Intervention*Post,
#          SemGrp = paste(Semester,Group,sep="_"))
# 
# 
# ## For Total Piles
# # Models
# TotPiles.unc <- lmer(Total_Piles ~ 1 + (1|SemGrp), data = PileMultiRegdata)
# TotPiles.lmer <- lmer(Total_Piles ~ 1 + Post + Intervention + PostInt + (1|SemGrp), data = PileMultiRegdata)
# TotPiles.lm <- lm(Total_Piles ~ 1 + Post + Intervention + PostInt, data = PileMultiRegdata) # Single-level
# 
# # Model Results
# TotPilesR2 <- All_R2(TotPiles.unc,"Unconditional",TotPiles.unc,"SemGrp") %>%
#   full_join(All_R2(TotPiles.lmer,"Model",TotPiles.unc,"SemGrp"),by="Predictor")
# TotPilesHLMResults <- Get_HLM_Coefs(TotPiles.lmer,digits=2,modelname="Model") %>%
#   full_join(Get_HLM_Coefs(TotPiles.unc,digits=2,modelname="Unconditional"),by="Predictor") %>%
#   bind_rows(TotPilesR2) %>%
#   select(Predictor,Unconditional,Model) %>%
#   mutate_at(vars(-Predictor),~replace_na(.,""))
# 
# 
# ## For Percent Function-based names
# # Models
# PerFunc.unc <- lmer(Percent ~ 1 + (1|SemGrp), data = PileMultiRegdata)
# PerFunc.lmer <- lmer(Percent ~ 1 + Post + Intervention + PostInt + (1|SemGrp), data = PileMultiRegdata)
# PerFunc.lm <- lm(Percent ~ 1 + Post + Intervention + PostInt, data = PileMultiRegdata) # Single-level
# 
# # Model Results
# PerFuncR2 <- All_R2(PerFunc.unc,"Unconditional",PerFunc.unc,"SemGrp") %>%
#   full_join(All_R2(PerFunc.lmer,"Model",PerFunc.unc,"SemGrp"),by="Predictor")
# PerFuncHLMResults <- Get_HLM_Coefs(PerFunc.lmer,digits=2,modelname="Model") %>%
#   full_join(Get_HLM_Coefs(PerFunc.unc,digits=2,modelname="Unconditional"),by="Predictor") %>%
#   bind_rows(PerFuncR2) %>%
#   select(Predictor,Unconditional,Model) %>%
#   mutate_at(vars(-Predictor),~replace_na(.,""))
# 
# ## Combining TotPiles and PerFunc Results
# TPPFResults <- left_join(TotPilesHLMResults,PerFuncHLMResults,by="Predictor")


##########################################



# ------------------------------------------------------------------------
# Free Sort Card Data
# ------------------------------------------------------------------------

#########################
#### Score Frequency and Proportion of Responses (i.e. treating score as ordinal, not continuous) ####
# I end up using GroupFreq to calculate ICSummary which is then used for the plot; I don't use any of the wide format


#### Calculating Group Scores ####
## Score (i.e. color) frequency for each group and calculating Total Score (Score value * frequency)
# All cards
GroupFreqTot <- TheColorCards %>%
  group_by(Semester,IC,Group,Time,Score) %>%
  summarize(Score_Freq = n()) %>%
  mutate(Card_Category="Total",
         Total_Score = sum(as.numeric(Score)*Score_Freq),
         Num_Cards = sum(Score_Freq),
         Score_Prop = Score_Freq/Num_Cards)
# By Card Category, then binding All cards totals
GroupFreq <- TheColorCards %>%
  group_by(Semester,IC,Group,Time,Card_Category,Score) %>%
  summarize(Score_Freq = n()) %>%
  mutate(Total_Score = sum(as.numeric(Score)*Score_Freq),
         Num_Cards = sum(Score_Freq),
         Score_Prop = Score_Freq/Num_Cards) %>%
  bind_rows(GroupFreqTot) %>%
  select(Semester:Card_Category,Num_Cards,Score,Score_Freq,Score_Prop,Total_Score)

## Frequencies and proportions summarized by intervention/control, pre/post, and card category
ICSummary <- GroupFreq %>%
  group_by(IC,Time,Card_Category,Score) %>%
  summarize(Frequency = sum(Score_Freq)) %>%
  mutate(Tot_Freq = sum(Frequency),
         Proportion = Frequency/Tot_Freq)

## Plotting proportions
SummaryPropPlot <- ICSummary %>% filter(!is.na(IC)) %>%
  mutate(Percent = round(Proportion*100,0),
         pos = cumsum(Percent) - 0.5*Percent) %>% ungroup() %>%
  mutate(Score = fct_rev(as_factor(as.character(Score)))) %>%
  # Color = recode(Score,"0='Purple';1='Orange';2='Pink';3='Cyan';4='Green';5='Yellow'"))
  ggplot(aes(x=Time,y=Percent,fill=Score,group=Score)) +
  geom_bar(stat="identity",position="stack") +
  geom_text(aes(label=ifelse(Percent >= 7,Percent,""),y=pos),color = "black",size=5) +
  scale_fill_manual(values=rev(c("#9141A6","#FF9900","#FF00FF","#00FFFF","#93C47D","#FFE599"))) + #palette="Dark2"
  xlab("") + coord_flip() +
  theme_bw(base_size = 20) + 
  facet_grid(IC~Card_Category)

#### Wide Format Displays ####
# ## Frequencies in wide format for display purposes
# # For each Group
# GroupFreqWide <- GroupFreq %>% select(-Num_Cards,-Score_Prop) %>%
#   spread(Score,Score_Freq) %>%
#   mutate_at(vars(`0`:`5`),~car::recode(.,"NA=0"))
# # Summarized by intervention/control, pre/post, and card category
# SummaryFreqWide <- ICSummary %>%
#   select(-Proportion,-Tot_Freq) %>%
#   spread(Score,Frequency) %>%
#   mutate_at(vars(`0`:`5`),~car::recode(.,"NA=0"))
#
# ## Proportions in wide format for display purposes
# # For each Group 
# GroupPropWide <- GroupFreq %>% select(-Num_Cards,-Score_Freq) %>%
#   spread(Score,Score_Prop) %>%
#   mutate_at(vars(`0`:`5`),~round(car::recode(.,"NA=0")*100,1))
# # Summarized by intervention/control, pre/post, and card category
# SummaryPropWide <- ICSummary %>%
#   select(-Frequency,-Tot_Freq) %>%
#   spread(Score,Proportion) %>%
#   mutate_at(vars(`0`:`5`),~round(car::recode(.,"NA=0")*100,1))




########################################


#################################
#### Total Score Comparisons ####

## Total Score descriptives by Intervention/control and pre/post
TotalScoreDescriptives <- GroupFreq %>% ungroup() %>%
  select(IC,Group,Time,Card_Category,Total_Score) %>% unique() %>%
  Get_Descriptives(Total_Score,IC,Time,Card_Category)

## Score change by card category
TotalScoreChange <- TotalScoreDescriptives %>%
  filter(!is.na(IC)) %>%
  select(IC,Time,Card_Category,Mean) %>%
  spread(Time,Mean) %>%
  mutate(Delta = Post - Pre,
         PerInc = Delta / Pre)

## Plotting total score descriptives
TotalScorePlot <- TotalScoreDescriptives %>% filter(!is.na(IC)) %>%
  ICcomparePlots("Mean","Mean Score",0,300,50, guideTF = TRUE) +
  geom_errorbar(aes(ymin=Mean-1.96*SE,ymax=Mean+1.96*SE),color="black",width=.2) +                  # creates error bars
  guides(linetype = guide_legend(reverse=TRUE)) +
  facet_wrap(~Card_Category)

# Distribution
TotalScoreDistPlot <- GroupFreq %>%
  filter(!is.na(IC)) %>%
  mutate(Condition_x_Time = paste(IC,Time,sep = " - ")) %>%
  Get_DenPlot(xvar="Total_Score",fillvar = "Condition_x_Time","Total Score","Condition x Time",0,300,50,"Paired")  


#### Regression Models for Total score ####

# ## Multilevel
# # Formatting data
# TotScoreMultiRegdata <- GroupFreq %>% ungroup() %>%
#   select(Semester,IC,Group,Time,Card_Category,Total_Score) %>% unique() %>%
#   mutate(Intervention = ifelse(IC=="Intervention",1,0),
#          Post = ifelse(Time=="Post",1,0),
#          PostInt = Intervention*Post,
#          SemGrp = paste(Semester,Group,sep="_")) %>%
#   filter(Card_Category=="Total" & !is.na(IC))
# 
# # Models
# TotScore.unc <- lmer(Total_Score ~ 1 + (1|SemGrp), data = TotScoreMultiRegdata)
# TotScore.lmer <- lmer(Total_Score ~ 1 + Post + Intervention + PostInt + (1|SemGrp), data = TotScoreMultiRegdata)
# TotScore.lm <- lm(Total_Score ~ 1 + Post + Intervention + PostInt, data = TotScoreMultiRegdata) # Single-level check
# 
# # Model Results
# TotScoreR2 <- All_R2(TotScore.unc,"Unconditional",TotScore.unc,"SemGrp") %>%
#   full_join(All_R2(TotScore.lmer,"Model",TotScore.unc,"SemGrp"),by="Predictor")
# TotScoreHLMResults <- Get_HLM_Coefs(TotScore.lmer,digits=2,modelname="Model") %>%
#   full_join(Get_HLM_Coefs(TotScore.unc,digits=2,modelname="Unconditional"),by="Predictor") %>%
#   bind_rows(TotScoreR2) %>%
#   select(Predictor,Unconditional,Model) %>%
#   mutate_at(vars(-Predictor),~replace_na(.,""))


## Single-level with Post as criterion
# Data
TotScoreWide <- GroupFreq %>% ungroup() %>%
  Get_WideRegData(Total_Score, filter_exp = Card_Category=="Total")

# Model
TotScore.pre <- TotScoreWide %>%
  # mutate(Pre_cent = scale(Pre, scale = FALSE),
  #        PreInt_cent = scale(PreInt, scale = FALSE)) %>% 
  lm(Post ~ 1 + Intervention + Pre + PreInt, data=.)
# Centering doesn't change much and the model checks show heterogenity and non-normality of residuals

# Model Checking
car::vif(TotScore.pre)
TotScoreModelCheck <- ModelCheckPlots(TotScore.pre)
summary(TotScore.pre)

## Effect sizes (eta2)
sjstats::eta_sq(TotScore.pre,partial=TRUE)
# # Power analysis from Gpower shows model had 80% power to detect effect of d=.5, f2 = .065, eta2=.06, which are medium effects


## Export Results
TScoefs <- Get_Fixed(TotScore.pre, "Total Score", digits = 2, pnames = Pprednames) %>% ## Fixed effects
  mutate(Coef = paste0(Estimate, " [",Lower,", ",Upper,"]"),
         Coef = ifelse(Sig == "Yes", paste0(Coef, "*"), paste0(Coef, " "))) %>%
  select(Model, Predictor, Coef) %>%
  spread(Model, Coef) %>%
  bind_rows(All_R2(TotScore.pre, "Total Score"))                 ## Model Fit

## t-tests
# TotScorePreTtest <- t.test(Pre ~ IC, data = TotScoreWide)
# TotScorePostTtest <- t.test(Post ~ IC, data = TotScoreWide)
# TotScoreChangeTtest <- TotScoreWide %>% mutate(Change = Post - Pre) %>%
#   t.test(Change ~ IC, data = .)




#############################################

###############################################
#### Comparing Pre-Post difference by Card ####

#### Average Score difference ####
## Calculating difference
CardScoreDiff <- TheColorCards %>% spread(Time,Score) %>%
  mutate(Score_Diff = Post - Pre)

## Summarized by intervention/control and Card
# Descriptives
SummaryScoreDiff <- CardScoreDiff %>% Get_Descriptives(Score_Diff,IC,Card_Category,Card_ID)
# Plot
ScoreDiffPlot <- SummaryScoreDiff %>% filter(!is.na(IC)) %>%
  ggplot(aes(x=fct_reorder(Card_ID,Mean),y=Mean,fill=IC,group=IC)) +
  geom_bar(stat="identity",position=position_dodge(width=.7),width=.7) +
  # geom_errorbar(aes(ymin=Mean-SE*1.96,ymax=Mean+SE*1.96),position=position_dodge(width=.7),width=.4,color="black") +
  # geom_text(aes(label=format(round(V,2),nsmall=2),y=.05),position=position_dodge(width=.9),color="black") +
  # scale_y_continuous(limits = c(0,1),breaks=seq(0,1,.2),name="Association (Cramer's V)") +
  geom_hline(yintercept=0,color="black") +
  xlab("Card") + ylab("Mean Pre/Post Difference") +
  scale_fill_manual(name="",values=rev(c("#d8b365","#5ab4ac"))) +
  theme_bw(base_size = 20) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.position="top") +
  facet_wrap(~Card_Category,ncol=1,scales="free_x")


#### Median Score Frequency ####
set.seed(5320)
CardFreqPlot <- TheColorCards %>% drop_na(IC) %>%
  group_by(IC,Card_Category,Card_ID,Time) %>%
  summarize(Median_Score = median(Score, na.rm = TRUE)) %>%
  unite(ConditionxCategory,IC,Card_Category,sep = " - ") %>%
  spread(Time, Median_Score) %>%
  mutate(PreJitter = jitter(Pre),
         Diff = Pre - PreJitter) %>%
  ggplot(aes(x = PreJitter, y = Post, label = Card_ID, color = ConditionxCategory)) +
  geom_text(angle = -45) +
  geom_abline(slope=1,intercept=0,color="grey") +
  scale_x_continuous(limits = c(0,5.15), name="Pre") +
  scale_y_continuous(limits = c(0,5.15)) +
  scale_color_brewer(palette = "Paired",name = "Condition x Category") +
  # scale_colour_manual(values=c("#d8b365","#5ab4ac")) +
  theme_bw(base_size = 20)
  # theme(legend.justification=c(1,0), legend.position=c(.95,.05))

#### Cramer's V Association ####
# ## Association between Pre and Post scores
# # n per card is too small to be useful by IC
# # Calculating Cramer's V by Card (excluding Alverno)
# CardCV <- CardScoreDiff %>% drop_na(Pre,Post,IC) %>% 
#   group_by(Card_Category,Card_ID) %>%
#   summarize(n=n(),
#             V =        CramerV(Pre,Post,conf.level=.95,method="ncchisq")[["Cramer V"]],
#             Lower_CI = CramerV(Pre,Post,conf.level=.95,method="ncchisq")[["lwr.ci"]],
#             Upper_CI = CramerV(Pre,Post,conf.level=.95,method="ncchisq")[["upr.ci"]])
# 
# # Plotting V
# CardCVPlot <- CardCV %>%
#   ggplot(aes(x=fct_reorder(Card_ID,V),y=V,fill=Card_Category,group=Card_Category)) +
#   geom_bar(stat="identity",position=position_dodge(width=.7),width=.7) +
#   # geom_errorbar(aes(ymin=Lower_CI,ymax=Upper_CI),position=position_dodge(width=.9),width=.5,color="black") +
#   # geom_text(aes(label=format(round(V,2),nsmall=2),y=.05),position=position_dodge(width=.9),color="black") +
#   scale_y_continuous(limits = c(0,1),breaks=seq(0,1,.2),name="Association (Cramer's V)") +
#   theme_bw(base_size = 20) +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#########################################################


# ------------------------------------------------------------------------
# Directed Sort
# ------------------------------------------------------------------------

#############################
#### Total Score Summary ####
## Total and Percent Correct for each group
TotScore18Summary <- TheCorIncCards %>%
  group_by(Time,Section,Group) %>%
  summarize(n = n(),
            Score_2066 = sum(Score[Card_ID=="2066"]),     # Adjusting number correct because card 2066 and 1193 had two correct answers and were counted twice
            Score_1193 = sum(Score[Card_ID=="1193"]),
            Count_Correct = 62 - sum(!is.na(Wrong_Pile)),
            Count_Correct = ifelse(Score_1193==0,Count_Correct+1,Count_Correct),
            Count_Correct = ifelse(Score_2066==0,Count_Correct+1,Count_Correct),
            Percent_Correct = (Count_Correct/62)*100)

TotScore18CC <- TheCorIncCards %>%
  group_by(Time,Card_Category) %>%
  summarize(n = n(),
            Score_2066 = sum(Score[Card_ID=="2066"]),     # Adjusting number correct because card 2066 and 1193 had two correct answers and were counted twice
            Score_1193 = sum(Score[Card_ID=="1193"]),
            Count_InCorrect = sum(!is.na(Wrong_Pile)),
            Count_Correct = n - Count_InCorrect,
            Percent_Correct = (Count_Correct/n)*100)
(480-18+1) / 480 # CAR post 463, 96.5
(480-112+3) / 480 # CAR pre 371, 77.3
psych::describeBy(TotScore18Summary$Count_Correct, TotScore18Summary$Time)

prepostd <- (60.25-44.7)/sqrt((2.79^2+10.05^2)/2)

## Distribution Plot of Pre and Post Count Correct (equivalent to Total Score)
PrePost18DistPlot <- TotScore18Summary %>%
  Get_DenPlot("Count_Correct","Time","Correct Cards","Time",0,65,10,c("#d8b365","#5ab4ac"))

# Data in wide format for inference testing
TotScore18SummaryWide <- TotScore18Summary %>%
  gather(Temp,Value,Count_Correct,Percent_Correct) %>%
  unite(Time_Correct,Time,Temp) %>%
  select(Section, Group, Time_Correct, Value) %>%
  spread(Time_Correct,Value)
names(TotScore18SummaryWide) <- str_remove(names(TotScore18SummaryWide),"_Correct")

## Pre/Post comparison on number of correct cards
# Note: Count and Percent produce the same results
# Paired T-test with descriptives on pre/post difference
TotScore18ttest <- t.test(TotScore18SummaryWide$Post_Count, TotScore18SummaryWide$Pre_Count, paired = TRUE) %>%
  tidy() %>% mutate_if(is.numeric,~round(.,3)) %>%
  bind_cols(psych::describe(TotScore18SummaryWide$Post_Count-TotScore18SummaryWide$Pre_Count))
# Checking assumption of normally distributed score difference
car::qqPlot(TotScore18SummaryWide$Post_Count-TotScore18SummaryWide$Pre_Count) # pretty good
plot(density(TotScore18SummaryWide$Post_Count-TotScore18SummaryWide$Pre_Count)) # surprisingly normal
# Wilcoxon paired signed rank test
TotScore18wctest <- wilcox.test(TotScore18SummaryWide$Post_Count, TotScore18SummaryWide$Pre_Count, paired = TRUE) %>%
  tidy() %>% mutate_if(is.numeric,~round(.,3))

#### Comparing the calculated score to the one presented in the Excel file ####

# ## Reading in % Correct from excel files
# OriginalPercents <- map_dfr(1:11, ~Read_CorInc_Data(file = "Total analysis sheets/Pre sort Fall 2018.xlsx",sheet = .x,
#                                                     AnswerKey = CorrectCards18, semester = "Fall 2018",prepost = "Pre",
#                                                     PercentCheck = TRUE))
# ## Joining to TestSummary
# TestPercentCheck <- TestSummary %>%
#   left_join(OriginalPercents, by = c("Time","Section", "Students")) %>%
#   mutate(Diff = round((Percent_Correct*100) - as.numeric(Initial_Percent),4))
## Diff is 0 for all groups

#########################
#### Card Difficulty ####

Card18Difficulty <- TheCorIncCards %>%
  group_by(Time,Pile,Card_ID,Card,Card_Category) %>%
  summarize(n = n(),
            Difficulty = mean(Score)) %>%
  spread(Time,Difficulty)

# ## t-test
# Card18ttest <- t.test(Card18Difficulty$Post,Card18Difficulty$Pre,paired = TRUE)

set.seed(5320) # used so we get the same jittering upon replication
## ScatterPlot by Card Category
Card18scatterCategory <- Card18Difficulty %>% drop_na() %>%
  mutate(PreJitter = jitter(Pre)) %>%
  ggplot(aes(x = PreJitter, y = Post, label = Card_ID, color = Card_Category)) +
  geom_text(angle = -45) +
  geom_abline(slope=1,intercept=0,color="grey") +
  scale_x_continuous(limits = c(0,1.05), name="Pre") +
  scale_y_continuous(limits = c(0,1.05)) +
  scale_colour_manual(values=c("#d8b365","#5ab4ac"), name ="Category") +
  theme_bw(base_size = 20) +
  theme(legend.justification=c(1,0), legend.position=c(.95,.05))

## ScatterPlot by Pile type
Card18scatterPile <- Card18Difficulty %>% drop_na() %>%
  mutate(PreJitter = jitter(Pre)) %>%
  ggplot(aes(x = PreJitter, y = Post, label = Card_ID, color = Pile)) +
  geom_text(angle = -45) +
  geom_abline(slope=1,intercept=0,color="grey") +
  scale_x_continuous(limits = c(0,1.05), name="Pre") +
  scale_y_continuous(limits = c(0,1.05)) +
  scale_colour_brewer(palette = "Set2") +
  theme_bw(base_size = 20) +
  theme(legend.justification=c(1,0), legend.position=c(.95,.05))

##################################



##########################################

#--------------------------------------------------------------
# Card Sorting Task and Exam Data Association
#--------------------------------------------------------------


# #### Association between Card Sorting and Final Exam ####
# ## Joining performance on the Card Sorting Task
# # Card sorting prep with Wide data
# CardsPrep18 <- TotScore18SummaryWide %>%
#   mutate(Students = str_remove_all(Students," ")) %>%
#   separate(Students, into = paste("Student",1:3,sep="_"), sep=",") %>%
#   gather(Temp,ID,starts_with("Student_")) %>%
#   drop_na(ID)
# # The join
# CardsExam18 <- CardsPrep18 %>%
#   inner_join(FinalExamScores, by = "ID") %>%
#   mutate(Female = ifelse(Sex == "female", 1, 0)) %>%
#   select(Semester, Section, Group, ID, Sex, Female, everything(), -Temp)
# # Checking
# CardsNoExam <- CardsPrep18 %>% anti_join(FinalExamScores, by = "ID") # Every student with Card data had an exam
# ExamNoCards <- FinalExamScores %>% anti_join(CardsPrep18, by = "ID") # All of the 2018 exams were matched
# 
# ## Correlations
# CardExam18Cors <- CardsExam18 %>% mutate(Post_di = ifelse(Post_Count==62,1,0)) %>%
#   select(Pre_Count,Post_Count,Post_di,Exam_Score,Female) %>%
#   cor(use="pairwise.complete.obs")
# plot(CardsExam18$Post_Count,CardsExam18$Exam_Score)
# 
# ## Regression Model - Using counts rather than percents
# CardExam18.lm <- lm(Exam_Score ~ 1 + Pre_Count + Post_Count + Female, data = CardsExam18)
# car::vif(CardExam18.lm)
# summary(CardExam18.lm)
# CardExam18lmPlots <- ModelCheckPlots(CardExam18.lm)


####################################################################
#### Exporting datasets used in final analysis and scatterplots ####

# ## Creating Excel files
# # Final Exam
# FinExExcel <- loadWorkbook("Final analysis sheets/Final Exam Item and Total Scores.xlsx",create=TRUE)
# MakeSheet(FinExExcel,FinalExam %>% select(-Totals,-Multi_Response,-Diff,-Person,-Group_Change),"Item Responses")
# MakeSheet(FinExExcel,FinalExamScores %>% select(-Female,-Intervention,-FemInt),"Total Scores")
# saveWorkbook(FinExExcel)
# 
# # Free Sort
# FreeSortExcel <- loadWorkbook("Final analysis sheets/Free Sort Card Task.xlsx",create=TRUE)
# MakeSheet(FreeSortExcel, TheColorCards, "Card Data")
# MakeSheet(FreeSortExcel, GroupFreq, "Total Score Data")
# MakeSheet(FreeSortExcel, ThePiles %>% select(Semester:Name_Type,Piles,Percent), "Pile Data")
# saveWorkbook(FreeSortExcel)
# 
# # Directed Sort
# DirectSortExcel <- loadWorkbook("Final analysis sheets/Directed Sort Card Task.xlsx",create=TRUE)
# MakeSheet(DirectSortExcel, TheCorIncCards, "Card and Pile Data")
# MakeSheet(DirectSortExcel, TotScore18Summary, "Total Score Data")
# saveWorkbook(DirectSortExcel)
# 
# 
# ## Scatterplots
# ggsave(CardFreqPlot,file="Free Sort Cards by IC and Category.png", height=6, width=10)
# ggsave(Card18scatterCategory,file="Direct Sort Cards by Card Category.png", height=6, width=8)
# ggsave(Card18scatterPile, file="Direct Sort Cards by Pile.png", height=6, width=8)
