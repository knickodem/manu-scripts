##############################
#                            #
#       SP2 - In-class       #
#  Problem Solving Activity  # 
#                            #
##############################



#######################################################
#### Loading Initial Data, Packages, and Functions ####

#### Gathering Data ####
source("Gather Data.R")

#### Additional Packages Not in Gather ####
library(purrr)

FilteredCorrs <- function(data, time){
  
  fcrs <- data %>%
    filter(IC == time) %>%
    select(.,-Group,-Semester,-IC) %>%
    rquery.cormat(.,type = "flatten",usena = "pairwise.complete.obs", graph = FALSE) %>%
    mutate(IC = time)
  
  return(fcrs)
  
}

ScorePlot <- function(plot){
  
  plot <- plot +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = ifelse(Percent >= 7, Percent_Round, ""), y = pos), color = "white", size = 8) +
    xlab("") + ylab("Percent of Groups") + coord_flip() +
    theme_bw(base_size = 22)
  
}

##################################################################

#####################################################
#### Transforming Activity Data and Student Info ####

#### Notes ####
# Section 001 groups have IDs containing Group whereas section 002 have IDs containing Grp
# For fall 2018 the missing group 2 is because groups 1 and 2 were combined - so P. Hoffarth, M Schlief, B. Clarke and S. Irber completed the activity together.


#### Activity Data ####
SP2In <- Rubrics %>%
  filter(Activity == "SP - Day 2" & Time == "In-class") %>%  
  mutate(ID = ifelse(ResponseId=="R_2rjPYpXtC87Nrqa","Group 8",ID),                  # Changing ID from Group 9 to Group 8 based on re-entered data 
         IC = case_when(Semester %in% c("Fall 2017","Fall 2018") ~ "Treatment",   # Defining Treatment and Control Semesters
                        Semester %in% c("Spring 2016","Fall 2016") ~ "Control") %>%
           factor(., levels = c("Treatment","Control")),
         Semester = as_factor(Semester) %>%
           fct_recode(., C1 = "Spring 2016", C2 = "Fall 2016", T1 = "Fall 2017", T2 = "Fall 2018") %>%
           fct_relevel("C1","C2","T1","T2"),
         Item = str_remove(Item,"_")) %>% 
  filter(Finished == 1 & str_detect(ID,"001", negate = TRUE)) %>%# duplicated or irrelevant entries; (001 entries served to confirm identities of previously entered data and Spring 2016 group 3 was partially duplicated)
  filter(Item != "9a1")  # only asked in Spring 2016 apparently
 


# ## Loading data from Excel
# SP2InOrig <- RubricsOrig %>% select(Finished,ResponseId,Semester:ID,starts_with("SP2_In")) %>% # Keeping identifiers and SP2 in-class items
#   mutate_at(vars(Semester,Activity,Time),as_factor) %>%                    # Converting 
#   filter(Activity == "SP - Day 2") %>%
#   filter(str_detect(ID,"Gr"))           # Removing Test cases


#### Demographic, Exam, and Group information ####
StudentInfo <- StudentInfo %>%
  mutate(Card_Sort_Group = ifelse(ID %in% c("irber003","clar1632"), 1, Card_Sort_Group)) # For fall 2018 the missing group 2 is because groups 1 and 2 were combined
  
## Exam and Course Performance outcomes in long format
SPOutcomesLong <- StudentInfo %>%
  select(Name:IC,Group = Card_Sort_Group,Sex,Ethnicity,Final_Course_Grade = Final_Course_Percent,Exam_1:Final_Exam) %>%
  gather(Outcome, Percent, Final_Course_Grade:Final_Exam) %>%
  mutate(Outcome = str_replace_all(Outcome,"_"," "),
         Group = ifelse(is.na(Section), paste0("Group ", Group),
                        ifelse(Section == "002", paste0("Grp", Group), paste0("Group ", Group))),
         IC = as_factor(IC) %>% fct_recode(Treatment = "Intervention") %>%
           fct_relevel("Treatment","Control"),
         Semester = as_factor(Semester) %>%
           fct_recode(., C1 = "Spring 2016", C2 = "Fall 2016", T1 = "Fall 2017", T2 = "Fall 2018") %>%
           fct_relevel("C1","C2","T1","T2"))

#############################################################################

##############################################
#### Analyzing SP2 Performance with Exams ####

#### Group-level SP2 Performance ####
SP2GroupScores <- SP2In %>% group_by(IC, Semester, ID) %>%
  summarize(SP2 = sum(Response, na.rm = TRUE)) %>% ungroup()

## SP2 Performance by Intervention/Control
SP2Descrips <- SP2GroupScores %>%
  Get_Descriptives(SP2, IC)
SP2ttest <- t.test(SP2 ~ IC, data = SP2GroupScores)

## Group-level Exam Performance and SP2 total score
SP2ScoreGroupSummary <- SPOutcomesLong %>%
  filter(Outcome %in% c("Exam 2","Final Exam") & !is.na(Group)) %>%
  Get_Descriptives(Percent, IC, Semester, Group, Outcome) %>%
  left_join(SP2GroupScores %>% rename(Group = ID), by = c("IC","Semester","Group"))

## Correlations between Outcomes
OutcomeCorrsTemp2 <- SP2ScoreGroupSummary %>%
  select(IC, Semester, Group, SP2, Outcome, Mean) %>%
  spread(Outcome, Mean)

## Correlations of PS activity score with Exams by IC
OutcomeCorrs2 <- OutcomeCorrsTemp2 %>%
  FilteredCorrs("Treatment") %>%
  bind_rows(FilteredCorrs(OutcomeCorrsTemp2, "Control"))

## Extracting correlation labels to add to scatterplot below
scatlabs2 <- OutcomeCorrs2 %>%
  filter(row == "SP2") %>%
  rename(Outcome = column) %>%
  mutate(SP2 = 19,
         Mean = case_when(IC == "Treatment" ~ 58,
                          IC == "Control" ~ 62),
         r = paste("r =", CorSig))

## Scatterplots grouped by IC
SP2ExamScatterPlot <- SP2ScoreGroupSummary %>%
  mutate(IC = fct_rev(IC)) %>%
  ggplot(aes(x = SP2, y = Mean, color = IC)) +
  geom_smooth(method = "lm", se = FALSE, size = 2, linetype = 1) + #
  geom_point(aes(group = IC, shape = IC), size = 4) + #shape = 1
  scale_x_continuous(name = "PS Activity Score", limits = c(11, 21),breaks = seq(12, 20, 2)) +
  scale_y_continuous(name = "Percent Correct", limits = c(52, 97),breaks = seq(60, 90, 10)) +
  scale_color_brewer(palette = "Set2", direction = -1) +
  geom_text(data = scatlabs2, aes(label = r), size = 8, show.legend = FALSE) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", strip.background = element_rect(fill = "white"), legend.title = element_blank()) +
  facet_wrap(~Outcome)

####################################################################################

#############################
#### Item-level analysis ####

sp2ItemRecodes <- c(Q2A = "2a1", Q2B = "2b1", Q2C = "2c1", Q2D = "2d1",
                    Q3.1 = "3a1", Q3.2 = "3a3", Q4 = "4a1", Q5 = "5a1", Q6 = "6a1")

#### Item Score Freq and % by Treatment/Control ####
SP2ItemSummary <- SP2In %>%
  filter(!is.na(Response)) %>%
  mutate(Item = as_factor(Item) %>%
           fct_recode(!!!sp2ItemRecodes)) %>%
  group_by(IC, Item, Response) %>%
  summarize(Responses = n()) %>%
  mutate(Total = sum(Responses),
         Percent = Responses / Total * 100,
         Percent_Round = round(Percent),
         Score = factor(Response, levels = c(0,1,2,3)),
         pos = cumsum(Percent) - 0.5*Percent) %>% # Location where Percent will be printed on the bar
  group_by(Item) %>%
  mutate(Name = paste0(Item," (",max(as.numeric(Score) - 1),")")) %>%
  ungroup() %>%
  mutate(Score = fct_rev(Score)) # Ordering response options for plotting

#### Q2 Plots ####
## Score
PS_ItemScore_Q2 <- SP2ItemSummary %>%
  filter(str_detect(Item, "Q2")) %>%
  ggplot(aes(x = fct_rev(IC), y = Percent, fill = Score)) %>%
  ScorePlot() +
  scale_fill_manual(values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c"))) +
  facet_wrap(~Name, ncol = 2) +
  theme(legend.position = "top", strip.background = element_rect(fill = "white")) +
  guides(fill = guide_legend(reverse = TRUE))

## 2B Responses
PS2B <- read_excel("../Rubric Responses/SP2_InClass.xlsx", sheet = "Q2B Responses")
PS2BFormat <- PS2B %>%
  mutate_if(is.character, as_factor) %>%
  group_by(IC) %>%
  mutate(pos = cumsum(Percent) - 0.5*Percent,
         Percent_Round = round(Percent)) %>%
  ungroup() %>%
  mutate(Circled_Response = fct_rev(Circled_Response)) # Ordering response options for plotting

PS2B_Plot <- ggplot(PS2BFormat, aes(x = fct_rev(IC), y = Percent, fill = Circled_Response)) %>%
  ScorePlot() +
  scale_fill_manual(name = "Circled Response",
                    values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c"))) +
  theme(legend.position = "top", legend.direction = "horizontal") +
  guides(fill = guide_legend(nrow = 2, byrow = FALSE, reverse = TRUE))

#### Non-Q2 Items ####
PS_ItemScore_Q3to6 <- SP2ItemSummary %>%
  filter(!str_detect(Item, "Q2")) %>%
  ggplot(aes(x = fct_rev(IC), y = Percent, fill = Score)) %>%
  ScorePlot() +
  scale_fill_manual(values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c"))) +
  facet_wrap(~Name, ncol = 2) +
  theme(legend.direction = "horizontal",
        legend.justification=c(1,0), legend.position=c(1,.05),
        strip.background = element_rect(fill = "white")) +
  guides(fill = guide_legend(reverse = TRUE))


#### Comparing Item Score and Exam Performance #### Don't really end up using this
SP2ItemswithExam <- SP2In %>% rename(Group = ID) %>%
  left_join(SP2ScoreGroupSummary %>%
              select(Semester,Group,Outcome,Mean),
            by = c("Semester","Group"))

## shortcut function for getting spearman correlation
Get_ItemExamCorr <- function(data,ItemNum,Exam){
  
  filtdat <- data %>%
    filter(Item == ItemNum & Outcome == Exam)
  
  cor(filtdat$Response,filtdat$Mean, method = "spearman", use = "pairwise.complete.obs")
  
}

SP2ItemExamCorrs <- data.frame(Item = unique(SP2ItemswithExam$Item),
                            Exam_2 = map_dbl(unique(SP2ItemswithExam$Item),~Get_ItemExamCorr(SP2ItemswithExam,.x,"Exam 2")),
                            Final_Exam = map_dbl(unique(SP2ItemswithExam$Item),~Get_ItemExamCorr(SP2ItemswithExam,.x,"Final Exam")))

ExambySP2ItemScore <- SP2ItemswithExam %>%
  group_by(Item,Response,Outcome) %>%
  summarize(n = sum(!is.na(Mean)),
            Mean_Score = mean(Mean,na.rm = TRUE),
            SD = sd(Mean, na.rm = TRUE))

###############################################3


#### Saving Objects and Exporting Plots & Datasets ####
## Plots
Q2Joint <- cowplot::plot_grid(PS_ItemScore_Q2,PS2B_Plot, nrow = 2, labels = c("A", "B"), rel_heights = c(1.4, 1))
ggsave(Q2Joint, file = "PS_ItemScore_Q2_2BResponse.tiff", height = 10, width = 11)
ggsave(PS_ItemScore_Q3to6, file = "PS_ItemScore_Q3to6.tiff", height = 7, width = 10)
ggsave(SP2ExamScatterPlot, file = "Exam_PS_ScatterPlot.tiff", height = 6, width = 12)




## Objects
save(SP2In,SPOutcomesLong,SP2ScoreGroupSummary,            # Data/Tables
     SP2ItemSummary,SP2ItemswithExam,
     SP2Descrips,SP2ttest,OutcomeCorrs2,SP2ItemExamCorrs,   # Means and Correlations
     SP2ExamScatterPlot,PS_ItemScore_Q3to6,                # Plots
     Q2Joint,PS_ItemScore_Q2,PS2B_Plot,                   
     file = "SP Results/SP2Output.RData")

