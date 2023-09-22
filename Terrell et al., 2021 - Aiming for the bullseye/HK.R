##############################
#                            #
#          HK - Pre          # 
#                            #
##############################



#######################################################
#### Loading Initial Data, Packages, and Functions ####

#### Gathering Data ####
source("Gather Data.R")
# Note: Rubrics is the final transformed and joined dataset
rm(RubricsOrig,RubricsComments,RubricsReflect,RubricConverted)

#### Additional Packages Not in Gather ####
library(purrr)

##################################################################

#####################################################
#### Transforming Activity Data and Student Info ####

#### Post Self-Rate ####
HKPostSelfRate <- RubricsSelfRate %>%
  filter(Activity=="HK" & Time == "Post") %>%
  mutate(Item = str_remove(Item,"SelfRate_"))

#### Activity Data ####
HKPre <- Rubrics %>%
  filter(Activity=="HK" & Time == "Pre") %>%
  mutate(Item = str_remove(Item,"_"))

#### Demographic, Exam, and Group information ####
StudentInfo <- StudentInfo %>%
  mutate(Semester = as_factor(Semester) %>% fct_relevel(.,"Spring 2016", "Fall 2016", "Fall 2017","Fall 2018"),
         Card_Sort_Group = ifelse(ID %in% c("irber003","clar1632"), 1, Card_Sort_Group)) # Need to check the group assignments for the HK activity

## Exam and Course Performance outcomes in long format
OutcomesLong <- StudentInfo %>%
  select(Name:IC,Group = Card_Sort_Group,Sex,Ethnicity,Final_Course_Grade = Final_Course_Percent,Exam_1:Final_Exam) %>%
  gather(Outcome, Percent, Final_Course_Grade:Final_Exam) %>%
  mutate(Outcome = str_replace_all(Outcome,"_"," "),
         Group = ifelse(is.na(Section), paste0("Group ",Group),ifelse(Section=="002",paste0("Grp",Group),paste0("Group ",Group))))

##################################################################

##############################################
#### Analyzing HK Performance with Exams ####

# ## Joining activity responses with student info
# # It appears scores are for individuals, not groups
# HKPrewGrp <- HKPre %>%
#   inner_join(StudentInfo, by = c("ID","Semester")) # All students matched :)


#### Student-level HK Performance ####
## Calculating HK sum score and joining student info
HKPreStudentScores <- HKPre %>% group_by(Semester,ID) %>%
  summarize(HK = sum(Response, na.rm=TRUE)) %>% ungroup() %>%
  inner_join(StudentInfo, by = c("ID","Semester"))

## HK Performance by Intervention/Control
HKPreScoreDescrips <- HKPreStudentScores %>%
  Get_Descriptives(HK,IC)
HKPrettest <- t.test(HK~IC,data = HKPreStudentScores)

## Correlations between Outcomes
HKPreOutcomeCorrs <- HKPreStudentScores %>%
  select(HK,Exam_2,Final_Exam) %>% #ACT_Math:Final_Exam, -Final_Letter
  rquery.cormat(.,type = "flatten",usena = "pairwise.complete.obs",graph=FALSE)


## Extracting correlation labels to add to scatterplot below
scatlabs <- data.frame(HK = 4,
                       Value = 88,
                       Outcome = HKPreOutcomeCorrs[HKPreOutcomeCorrs$row=="HK",]$column,
                       r = paste("r =",HKPreOutcomeCorrs[HKPreOutcomeCorrs$row=="HK",]$cor))

## Scatterplots grouped by IC
HKPreExamScatterPlot <- HKPreStudentScores %>%
  gather(Outcome,Value,Exam_2,Final_Exam) %>%
  ggplot(aes(x = HK, y = Value)) +
  geom_point(aes(group = IC, color = IC, shape = IC), size = 4) + #shape = 1
  geom_smooth(method="lm",color = "grey",se=FALSE,size=2,linetype=1) + # aes(color = IC),
  scale_x_continuous(name = "HK Score", limits = c(0,23), breaks = seq(0,22,2)) +
  scale_y_continuous(name = "Percent Correct", limits = c(45,100), breaks = seq(50,95,5)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(data = scatlabs,aes(label = r), size = 8) +
  theme_bw(base_size = 20) +
  theme(legend.justification=c(1,0), legend.position=c(.22,.05),legend.title = element_blank()) +
  facet_wrap(~Outcome) #, scales = "free_y"


####################################################################################

#############################
#### Item-level analysis ####

#### Item Score Freq and % by Intervention/Control ####
HKPreItemSummary <- HKPre %>%
  mutate(IC = case_when(Semester %in% c("Fall 2017","Fall 2018") ~ "Intervention",   # Definining Intervention and Control Semesters
                 Semester %in% c("Spring 2016","Fall 2016") ~ "Control")) %>%
  filter(!is.na(Response)) %>%
  group_by(IC,Item,Response)%>%
  summarize(Responses = n()) %>%
  mutate(Total = sum(Responses),
         Percent = round(Responses / Total *100),
         Score = factor(Response, levels = c(0,1,2,3)),
         pos = cumsum(Percent) - 0.5*Percent) %>% # Location where Percent will be printed on the bar
  ungroup() %>%
  mutate(Name = car::recode(Item,"'1a1'='Substrates';'1b1'='Products';'1c1'='Chiral Carbon';'1d1'='Enter/Leave Active Site';
                            '1e1'='Pushing Mechanism 1';'1e2'='Pushing Mechanism 2';'1e3'='Pushing Mechanism 3';'1e4'='Pushing Mechanism 4'"),
         Name = as_factor(Name),
  Score = fct_rev(Score)) # Ordering response options for plotting
#group_by(Item) %>% mutate(Name = paste0(Item," (",max(as.numeric(Score)-1),")"; Converting from factor to numeric adds 1, which we subtract to return to original value


# recode(Item, Substrates = "1a1", Products = "1b1", `Chiral Carbon` = "1c1", `Enter/Leave Active Site` = "1d1",
#        `Pushing Mechanism 1` = "1e1", `Pushing Mechanism 2` = "1e2",`Pushing Mechanism 3` = "1e3", `Pushing Mechanism 4` = "1e4"),
## Plot
HKPreItemICPlot <- HKPreItemSummary %>%
  ggplot(aes(x = IC, y = Percent, fill = Score)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Percent >= 7, Percent, ""), y = pos), color = "white", size = 6) +
  scale_fill_manual(values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c"))) + #scale_fill_brewer(palette = "BuPu", direction = -1) +
  xlab("") + coord_flip() +
  theme_bw(base_size = 22) +
  facet_wrap(~Name, ncol = 2)

###############################################

#### Saving Objects and Exporting Plots & Datasets ####
## Plots
ggsave(HKPreExamScatterPlot, file = "HK Results/HKPreExamScatterPlot.png", height = 6, width = 12)
ggsave(HKPreItemICPlot, file = "HK Results/HKPreItemICPlot.png", height = 15, width = 15)

## Objects
save(HKPre,HKPostSelfRate,HKPreStudentScores,HKPreItemSummary,            # Data/Tables
     HKPreScoreDescrips,HKPrettest,HKPreOutcomeCorrs,   # Means and Correlations
     HKPreExamScatterPlot,HKPreItemICPlot,                     # Plots
     file = "HK Results/HKPreOutput.RData")
