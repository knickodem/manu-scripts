#######################################
#                                     #
#   Concept Inventory - Pre & Post    # 
#                                     #
#######################################


#######################################################
#### Loading Initial Data, Packages, and Functions ####

#### Gathering Rubric Data and additional packages ####
source("Gather Data.R")
library(purrr)
library(flextable)

#### Concept Inventory Answer Key and Responses ####
## File Name shortcut
CIFile <- "../Concept Inventory Student Answers and Answer Key/Concept Inventory S16 F16 F17 answer key 08192018.xlsx"

## Answer Key and Item Type (Space and Electronics)
CIAnswerKey <- read_excel(CIFile, sheet = "Answer Key") %>%
  inner_join(read_excel("Rubric_CI_Item_Types.xlsx", sheet = "Inventory"), by = "Item") %>%
  mutate(Correct_Answer = str_replace(Correct_Answer,"–","-"))
# AnswerKeyQME <- CIAnswerKey %>% select(Item,Correct_Answer) %>%
#   spread(Item,Correct_Answer) %>%
#   select(one_of(CIAnswerKey$Item))
CIAllResponses <- read_excel(CIFile, sheet = "All Response Options") %>%
  mutate(Response = case_when(#Item == "Q3" ~ str_to_sentence(Response),
                               Item == "Q4" ~ str_remove(Response, "aka transition state") %>%
                                 str_remove(" \\(\\)") %>% str_replace("Site","site"),
                               # Item ==  "Q8" ~ str_to_sentence(Response) %>% str_replace("Dna","DNA"),
                               # Item ==  "Q10" ~ str_replace(Response,"and III only", "and III") %>%
                               #   str_replace(", II and", ", II, and"),
                               Item == "Q11" ~ str_replace(Response, "–", "-"),
                               TRUE ~ Response))


## Student Responses
# Before reading in the file I needed to:
# Add Semester and Time columns or edit values when they did exist, create column names to match answer key, and
# correct a few x500s that were not actually x500s.

# 2016 and 2017
CIResponses1617 <- read_excel(CIFile,sheet = "Pre+Post F16") %>%
  bind_rows(read_excel(CIFile,sheet = "Pre+Post S16"),
            read_excel(CIFile,sheet = "Pre+Post F17"))

# Fall 2018 Post
CIResponses18post <- read_excel("../Concept Inventory Student Answers and Answer Key/Biochemistry Concept Inventory_Post Fall2018.xlsx") %>%
  mutate(Time = "Post")
# Fall 2018 Pre
CIResponses18pre <- read_spss("../Concept Inventory Student Answers and Answer Key/Biochemistry Concept Inventory_September 27_2018_10_33.sav") %>%
  as_factor(only_labelled=TRUE) %>%
  select(starts_with("Q")) %>%
  mutate(Time = "Pre")

# # Checking if pre and post items are in the same order and if x500s matched
# PreItems <- purrr::map(CIResponses18pre,~attr(.,"label")) %>% tibble::enframe()
# PostItems <- CIResponses18post[1,] %>% t() %>% as.data.frame() %>% tibble::rownames_to_column("name") %>% right_join(PreItems)
# x50018anti <- anti_join(CIResponses18pre,CIResponses18post, by="Q1a") # sheet054 (pre), crake011 (post) not matched

#######################################################

##########################################################################
#### Binding, Transforming, & Scoring Concept Inventory Response Data ####

#### Binding Responses from all semesters ####
## Also removing irrelevant rows and columns
CIResponses <- CIResponses18pre %>%
  bind_rows(CIResponses18post)  %>%
  rename(x500 = "Q1a", Semester = "Q2a") %>%
  bind_rows(CIResponses1617) %>%
  filter(!str_detect(x500, "X500|test|ESICI ") & !is.na(x500) & !(x500 == "youn1957" & is.na(Q1))) %>% # blank rows
  mutate(x500 = tolower(x500) %>% str_remove(.,"@umn.edu"),
         Semester = str_to_sentence(Semester),
         IC = ifelse(Semester %in% c("Spring 2016","Fall 2016"), "Control", "Intervention")) %>%
  select(ID = x500, IC, Semester, Time, Q1:Q15)

# checkID <- CIResponses %>% count(IC,x500) # 11 x500s only have 1 observation; the other 126 have 2

#### Checking for discrepancies in response options and student info ####
# AllOptions <- CIResponses %>%
#   gather(Item,Response,starts_with("Q")) %>%
#   count(Item,Response)

## Correcting discrepancies
CIResponses <- CIResponses %>%
  mutate(Q3 = str_to_sentence(Q3),
         Q4 = str_remove(Q4, "aka transition state") %>% str_remove(" \\(\\)"),
         Q4 = str_replace(Q4, "Site","site"),
         Q8 = str_to_sentence(Q8) %>% str_replace("Dna","DNA"),
         Q10 = str_replace(Q10, "and III only", "and III"),
         Q10 = str_replace(Q10, ", II and", ", II, and"),
         Q11 = str_replace(Q11, "–", "-"))

# Exclusion criteria: "opting out of the study, failing to take the ESICI assessment pre- and post-instruction, withdrawal from the course, or re-taking the course."
CIComplete <- CIResponses %>%
  inner_join(StudentInfo %>% select(ID, Semester),by = c("ID","Semester")) %>%
  filter(ID != "crake011") # Does not have a pre assessment


#### Scoring ####
## Put responses in long format, join item type and answer key, and score responses
CIScored <- CIComplete %>%
  gather(Item, Response, Q1:Q15) %>%
  left_join(CIAnswerKey, by = "Item") %>%
  mutate(Item_Score = ifelse(Response == Correct_Answer, 1, 0))

# checkScore <- CIScored %>% select(Item,Response,Correct_Answer,Score) %>% unique()

#################################################################


#############################
#### Item-level analysis ####

## Note: I intially looked at differences by Semester to see if Fall 2017 and 2018 could be combined.
## Those two semesters had less variability than Spring and Fall 2016. In addition to the small sample sizes already
## making the stability of trends questionable, I conclude that comparing by IC will provide more stable comparisons.

#### Item Response Freq and % by Intervention/Control ####
CIItemSummary <- CIScored %>%
  filter(!is.na(Response)) %>%
  group_by(IC,Time,Item,Response)%>%
  summarize(Count = n()) %>%
  mutate(Total = sum(Count),
         Percent = round(Count / Total *100))
## Output table
CIItemSummaryTable <- CIItemSummary %>%
  unite(Temp, IC, Time, sep = "_") %>%
  mutate(Outcome = paste0(Count," (",Percent,"%)")) %>%
  select(Item, Response, Temp, Outcome) %>%
  spread(Temp, Outcome) %>%
  right_join(CIAllResponses, by = c("Item","Response")) %>% ungroup %>%
  mutate_at(vars(ends_with("Pre"),ends_with("Post")),~replace_na(.,"0 (0%)")) %>%
  mutate(Item = factor(Item, levels = CIAnswerKey$Item)) %>%
  arrange(Item,Response_Letter) %>%
  select(Item,Response,Control_Pre,Control_Post,Intervention_Pre,Intervention_Post)
  

#### Item Correct/Incorrect ####
CIItemAccuracy <- CIScored %>%
  group_by(IC,Time,Item) %>%
  filter(!is.na(Response)) %>%
  summarize(Total = n(),
            Percent = mean(Item_Score,na.rm = TRUE)*100,
            SE = (mean(Item_Score,na.rm = TRUE)*(1-mean(Item_Score,na.rm = TRUE))/sqrt(Total))*100,
            .groups = "drop") %>%
  mutate(Item = factor(Item, levels = CIAnswerKey$Item),
         # Semester = factor(Semester, levels = c("Spring 2016", "Fall 2016", "Fall 2017", "Fall 2018")),
         Time = factor(Time, levels = c("Pre","Post")))

## Generating Plot
CIItemAccPlot <- ggplot(CIItemAccuracy, aes(x=Time,y=Percent,linetype=IC,group=IC)) +
  geom_line(size=2) + geom_point(size=3) +                                                        # prints points on lines
  geom_errorbar(aes(ymin=Percent-1.96*SE,ymax=Percent+1.96*SE),color="black",width=.2) +                  # creates error bars
  scale_linetype_manual(values=c("solid","dotted"),name="",guide = guide_legend(reverse=TRUE)) +   # sets color scheme for bars
  scale_y_continuous(name="Percent Correct",limits=c(0,100),breaks=seq(0,100,20)) +
  # xlab(xlabel) + ylab(ylabel) +                                                         # sets x and y axis labels
  # coord_flip() +                                                                        # rotates plot 90 degrees clockwise
  theme_bw(base_size=22) +                                                              # sets base text size
  theme(strip.background = element_blank(), panel.border = element_rect(color = "black"), # additional theme settings
        # panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(),
        # axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
        legend.justification=c(1,0), legend.position=c(.95,.05)) +
  facet_wrap(~Item)

ggsave(CIItemAccPlot,file="CI Results/CI_Item_Accuracy_Plot.png",height=12,width=16)

# ## Organizing table for display - Redundant now that I've made CIItemSummaryTable above
# CIItemAccTable <- CIItemAccuracy %>%
#   select(IC,Time,Item,Percent) %>%
#   spread(Item,Percent) %>%
#   mutate_if(is.numeric,~round(.,0))

################################

################################################
#### Psychometric Evidence for Scale Scores ####

# Aims: Provide additional evidence for scale scores and contrast to Bretz and Linenberger (2012)
# The original article has 5 subcategories, but Cassidy has only given me info on 2 (Space and Electronics)
# However, it is not clear from the article or Cassidy's info what is the intended factor structure
# The original article only discusses a single summary score, so only a unidimensional model is investigated here
# Notably, the original article does not provide a strict interpretation of the sum score.

## Data in wide format
CIScoredWide <- CIScored %>%
  select(ID:Item, Item_Score) %>%
  spread(Item, Item_Score)

# CIScoredWide %>% count(Semester,ID)%>% View()

#### Classical Test Theory ####
## Item Analysis
# Pre
CI_CTT_Pre <- CIScoredWide %>% filter(Time == "Pre") %>%
  select(CIAnswerKey$Item) %>% psych::alpha(.,cumulative=TRUE)
# Post
CI_CTT_Post <- CIScoredWide %>% filter(Time == "Post") %>%
  select(CIAnswerKey$Item) %>% psych::alpha(.,cumulative=TRUE)
# Exporting Results
CI_CTT <- tibble::rownames_to_column(CI_CTT_Pre$item.stats,"Item") %>%
  inner_join(tibble::rownames_to_column(CI_CTT_Post$item.stats,"Item"),
             by = "Item", suffix = c("_Pre","_Post")) %>%
  select(Item,starts_with("mean"),starts_with("raw.r")) %>%
  mutate_if(is.numeric,~round(.,3)) %>%
  rename_at(vars(starts_with("mean")),~str_replace(.,"mean","Difficulty")) %>%
  rename_at(vars(starts_with("raw.r")),~str_replace(.,"raw.r","Discrimination"))

BandL <- data.frame(Item = paste0("Q",c(1:15)),
                    Diff = c(.873,.476,.419,.276,.751,.788,.432,.368,.653,.449,.235,.695,.524,.741,.741),
                    Disc = c(.178,.361,.204,.366,.508,.445,.319,.408,.424,.461,.251,.634,.586,.440,.550))


## Scale Reliabiity
CIomegaPre <- CIScoredWide %>% filter(Time == "Pre") %>%
  select(CIAnswerKey$Item) %>%
  psych::omega(., nfactors = 1, fm = "minres", rotate="oblimin", poly = TRUE)
CIomegaPost <- CIScoredWide %>% filter(Time == "Post") %>%
  select(CIAnswerKey$Item) %>%
  psych::omega(., nfactors = 1, fm = "minres", rotate="oblimin", poly = TRUE)
# Extracting relevant information
CIReliability <- data.frame(alpha_Pre = CIomegaPre$alpha,
                            alpha_Post = CIomegaPost$alpha,
                            omega_Pre = CIomegaPre$omega.tot,
                            omega_Post = CIomegaPost$omega.tot) %>%
  mutate_all(~round(.,2))


#### Exploratory Factor Analysis ####
## Scree Plots
# Pre
CIScreePre <- CIScoredWide %>% filter(Time == "Pre") %>% select(CIAnswerKey$Item) %>%
  psych::fa.parallel(fm = "minres", fa = "fa", cor = "poly", nfactors = 1, main = "Pre") 
# Post
CIScreePost <- CIScoredWide %>% filter(Time == "Post") %>% select(CIAnswerKey$Item) %>%
  psych::fa.parallel(fm = "minres", fa = "fa", cor = "poly", nfactors = 1, main = "Post") 

# Combine with ggplot
CIScreePlot <- data.frame(Actual_Pre = CIScreePre$fa.values,
                          Simulated_Pre = CIScreePre$fa.sim,
                          Actual_Post = CIScreePost$fa.values,
                          Simulated_Post = CIScreePost$fa.sim,
                          Factors = factor(c(1:length(CIScreePre$fa.values)))) %>%
  gather(Temp,Eigenvalues,-Factors) %>%
  separate(Temp,c("Data_Type","Time"),sep = "_") %>%
  mutate(Time = factor(Time, levels = c("Pre","Post"))) %>%
  ggplot(aes(x = Factors, y = Eigenvalues, linetype = Data_Type, group = Data_Type)) +
  geom_line(size = 1) + geom_point(size = 3) +
  scale_linetype_manual(values=c("solid","dotted"), name="Data Type") +   # sets color scheme for bars
  scale_y_continuous(name = "Eigenvalues of principal factors", limits = c(-1, 2.75), breaks = seq(-1, 2.5, .5)) +
  theme_bw(base_size = 22) +
  theme(strip.background = element_blank(),panel.border = element_rect(color = "black"),legend.position = "top") +
  facet_wrap(~Time)

ggsave(CIScreePlot, file = "CI Results/CI_Scree_Plot.png", height = 8, width = 12)

## KMO
# Pre
CIpolyPre <- CIScoredWide %>% filter(Time == "Pre") %>%
  select(CIAnswerKey$Item) %>% psych::polychoric()
CIKMOPre <- psych::KMO(CIpolyPre$rho)
# Post
CIpolyPost <- CIScoredWide %>% filter(Time == "Post") %>%
  select(CIAnswerKey$Item) %>% psych::polychoric()
CIKMOPost <- psych::KMO(CIpolyPost$rho)

## FA
# One Factor
CIefaPre <- CIScoredWide %>% filter(Time == "Pre") %>%
  select(CIAnswerKey$Item) %>% psych::fa(fm = "minres", rotate = "oblimin", cor = "poly", nfactors = 1)
CIefaPost <- CIScoredWide %>% filter(Time == "Post") %>%
  select(CIAnswerKey$Item) %>% psych::fa(fm = "minres", rotate = "oblimin", cor = "poly", nfactors = 1)
#  Two factor
CIefaPre2 <- CIScoredWide %>% filter(Time == "Pre") %>%
  select(CIAnswerKey$Item) %>% psych::fa(fm = "minres", rotate = "oblimin", cor = "poly", nfactors = 2)
CIefaPost2 <- CIScoredWide %>% filter(Time == "Post") %>%
  select(CIAnswerKey$Item) %>% psych::fa(fm = "minres", rotate = "oblimin", cor = "poly", nfactors = 2)

## Model Comparison
CIefaModComp <- anova(CIefaPre,CIefaPre2) %>% as.data.frame() %>%
  tibble::rownames_to_column("Temp") %>%
  bind_rows(as.data.frame(anova(CIefaPost,CIefaPost2)) %>% tibble::rownames_to_column("Temp")) %>%
  mutate(Time = ifelse(str_detect(Temp,"Pre"),"Pre","Post"),
         n_factors = ifelse(str_detect(Temp,"2"),"2","1")) %>%
  select(Time,n_factors,everything(),-contains("emp")) %>%
  mutate_if(is.numeric,~round(.,3))

## Exporting Results
# Loadings and Variance Explained
CIefaLoadings <- data.frame(Item = c(paste0("Q",c(1:15)),"Var Accounted"),
                           One_Pre_1 = c(CIefaPre$loadings,CIefaPre$Vaccounted[2,]),
                           Two_Pre_1 = c(CIefaPre2$loadings[,1],CIefaPre2$Vaccounted[2,1]),
                           Two_Pre_2 = c(CIefaPre2$loadings[,2],CIefaPre2$Vaccounted[2,2]),
                           One_Post_1 = c(CIefaPost$loadings,CIefaPost$Vaccounted[2,]),
                           Two_Post_1 = c(CIefaPost2$loadings[,1],CIefaPost2$Vaccounted[2,1]),
                           Two_Post_2 = c(CIefaPost2$loadings[,2],CIefaPost2$Vaccounted[2,2])) %>%
  mutate_if(is.numeric,~round(.,2))



##############################################
#### Analyzing CI Performance with Exams ####


# Research Aim: "Here we aim to present meta-analysis results investigating the impact of these physical models
# through assessment of the ESICI results, final exam results, and student self-assessed learning objectives
# from control and interventions sections."

# Methods overview: "From the individual scores of the ESICI, class averages on the overall misconceptions,
# specific misconception types, and the differences in misconceptions pre- and post-instruction were taken to assess student growth
# as well as teaching effectiveness"

#### Student-level CI Performance ####
## Calculating CI sum score and joining student info
CIStudentScores <- CIScored %>%
  group_by(Semester,ID,Time) %>%
  summarize(Count = sum(!is.na(Item_Score)),
            CI_Score = sum(Item_Score,na.rm = TRUE)) %>%
  inner_join(StudentInfo, by = c("Semester","ID")) %>% ungroup()

## Score Descriptives
OverallCI <- CIStudentScores %>%
  group_by(Time) %>%
  summarize(n = sum(!is.na(CI_Score)),
            Median = median(CI_Score, na.rm=TRUE),
            Mean = mean(CI_Score, na.rm=TRUE),
            SD = sd(CI_Score, na.rm=TRUE)) %>%
  mutate(IC = "Overall")

ICPPCI <- CIStudentScores %>%
  group_by(IC, Time) %>%
  summarize(n = sum(!is.na(CI_Score)),
            Median = median(CI_Score, na.rm=TRUE),
            Mean = mean(CI_Score, na.rm=TRUE),
            SD = sd(CI_Score, na.rm=TRUE),
            .groups = "drop") %>%
  # bind_rows(OverallCI) %>%
  unite(Group, IC, Time) %>%
  gather(Stat, Value, -Group) %>%
  spread(Group, Value) %>%
  mutate(Stat = factor(Stat, levels = c("n", "Median", "Mean", "SD"))) %>%
  select(Stat, Control_Pre, Intervention_Pre, Control_Post, Intervention_Post) %>%
  arrange(Stat)

CIflex <- flextable(ICPPCI) %>%
  colformat_num(j = names(ICPPCI)[-1], digits = 2) %>%
  autofit()

save_as_docx(CIflex, path = "CI Score Descriptives.docx")

## T-test by IC
t.test(CI_Score ~ IC, data = CIStudentScores[CIStudentScores$Time == "Pre", ])
t.test(CI_Score ~ IC, data = CIStudentScores[CIStudentScores$Time == "Post", ])
prepost <- CIStudentScores %>%
  select(ID, Time, CI_Score, IC) %>%
  spread(Time, CI_Score) %>%
  mutate(Gain = Post - Pre)
pp.mod <- lm(Gain ~ IC + Pre, data = prepost)
summary(pp.mod)

#### Student Demographics ####
Demos <- CIStudentScores %>% 
  mutate(Female = ifelse(Sex == "F", 1, 0),
         SOC = ifelse(Ethnicity != "White", 1, 0)) %>%
  select(ID,IC,Female,SOC,Total_Credits,Term_Credits,ACT_Math,Cumulative_GPA) %>%
  gather(Demographic,Value,Female:Cumulative_GPA) %>%
  unique() %>%
  mutate(Demographic = as_factor(Demographic))
# Output
CIDemoTable <- Demos %>%
  group_by(Demographic) %>%
  summarize(Summary = paste0(round(mean(Value, na.rm = TRUE),2), "(",round(sd(Value, na.rm = TRUE),2),")")) %>%
  mutate(IC = "Total") %>%
  bind_rows(Demos %>%
              group_by(IC,Demographic) %>%
              summarize(Summary = paste0(round(mean(Value, na.rm = TRUE), 2), " (",round(sd(Value, na.rm = TRUE), 2),")"))) %>%
  spread(IC, Summary) %>%
  mutate(Demographic = factor(Demographic, levels = levels(Demos$Demographic))) %>%
  arrange(Demographic)

## Proportion and T-test by IC
prop.test(table(CIStudentScores[CIStudentScores$Time == "Pre", ]$IC, CIStudentScores[CIStudentScores$Time == "Pre", ]$Sex))
prop.test(table(CIStudentScores[CIStudentScores$Time == "Pre", ]$IC, CIStudentScores[CIStudentScores$Time == "Pre", ]$Ethnicity == "White"))
t.test(Total_Credits ~ IC, data = CIStudentScores[CIStudentScores$Time == "Pre", ])
t.test(Term_Credits ~ IC, data = CIStudentScores[CIStudentScores$Time == "Pre", ])
t.test(ACT_Math ~ IC, data = CIStudentScores[CIStudentScores$Time == "Pre", ])
t.test(Cumulative_GPA ~ IC, data = CIStudentScores[CIStudentScores$Time == "Pre", ])

#### Correlation between CI and exam ####
## GGally version
CIcorrPlot <- CIStudentScores %>%
  spread(Time, CI_Score) %>%
  select(IC,Pre,Post,Exam_2,Final_Exam) %>%
  GGally::ggpairs(columns = 2:5, ggplot2::aes(colour = IC))

ggsave(CIcorrPlot, file = "CI Results/CI_ScoreCorr_Plot.png", height = 8, width = 12)

## Correlations between Outcomes
CIOutcomeCorrs <- CIStudentScores %>%
  spread(Time, CI_Score) %>%
  select(Pre,Post,Exam_2,Final_Exam) %>% #ACT_Math:Final_Exam, -Final_Letter
  rquery.cormat(.,type = "flatten",usena = "pairwise.complete.obs",graph=FALSE)


## Extracting correlation labels to add to scatterplot below
scatlabs <- CIOutcomeCorrs %>%
  filter(row %in% c("Pre","Post") & column %in% c("Exam_2","Final_Exam")) %>%
  mutate(CI_Score = 2, Percent_Correct = 92,
         Exam = str_replace(column, "_", " "),
         Time = factor(row, levels = c("Pre","Post")),
         r = paste("r =",cor)) %>%
  select(CI_Score:r)

## Scatterplots grouped by IC
CIScatterPlot <- CIStudentScores %>%
  select(ID,IC,Time,CI_Score,Exam_2,Final_Exam) %>%
  gather(Exam,Percent_Correct,Exam_2,Final_Exam) %>%
  mutate(Exam = str_replace(Exam, "_", " "),
         Time = factor(Time, levels = c("Pre","Post"))) %>%
  ggplot(aes(x = CI_Score, y = Percent_Correct)) +
  geom_point(aes(group = IC, color = IC, shape = IC), size = 4) + #shape = 1
  geom_smooth(method="lm",color = "grey",se=FALSE,size=2,linetype=1) + # aes(color = IC),
  scale_x_continuous(name = "ESICI Score", limits = c(0,15), breaks = seq(0,15,2)) +
  scale_y_continuous(name = "Percent Correct", limits = c(40,100), breaks = seq(45,95,5)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(data = scatlabs,aes(label = r), size = 7) +
  theme_bw(base_size = 22) +
  theme(legend.position="top", legend.title = element_blank()) +
  facet_grid(Exam~Time) #, scales = "free_y"

ggsave(CIScatterPlot, file = "CI Results/CI_Scatter_Plot.png", height = 10, width = 14)


####################################################################################

#### Saving All Output ####
save(CIScored, CIStudentScores, CIDemoTable, CIItemAccPlot, CIItemSummaryTable,
     CI_CTT, BandL, CIReliability, CIKMOPre, CIKMOPost, CIScreePlot,
     CIefaLoadings, CIefaModComp, CIScatterPlot, file = "CI Results/CIOutput.RData")

