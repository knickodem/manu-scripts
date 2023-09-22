##############################
#                            #
#       SP1 - In-class       #
# Model Exploration Activity #
#                            #
##############################


########################################
#### Loading packages and functions ####

#### Gathering Rubric Data ####
source("Gather Data.R")

#### Additional Packages Not in Gather ####
library(purrr)

#### Density plots ####
Get_DenPlot <- function(data, xvar, fillvar, xname=xvar, fillname=fillvar,
                        xmin, xmax, xseq, fillcolors = "Set2"){
  
  dp <- ggplot(data=data,aes_string(x = xvar, fill = fillvar)) +
    geom_density(alpha = .3) +
    scale_x_continuous(name = xname, limits = c(xmin, xmax+.5), breaks = seq(xmin, xmax, xseq)) +
    ylab("Density") +
    theme_bw(base_size = 20)
  
  if(length(fillcolors)>1){
    
    dp <- dp +
      scale_fill_manual(name = fillname, values = fillcolors)
    
  } else{
    
    dp <- dp +
      scale_fill_brewer(name = fillname, palette = fillcolors)
  }
  return(dp)
}

#### Correlation by semester shortcut ####
FilteredCorrs <- function(data, time){
  
  fcrs <- data %>%
    filter(Semester == time) %>%
    select(.,-Group,-Semester) %>%
    rquery.cormat(.,type = "flatten", usena = "pairwise.complete.obs", graph = FALSE) %>%
    mutate(Semester = time)
  return(fcrs)
  
}

ObTypePlot <- function(data){
  
  
  otp <-   ggplot(data = data, aes(x = fct_rev(Semester), y = Percent, fill = Ob.Type)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(aes(label = ifelse(Percent >= 7, Percent, ""), y = pos), color = "black", size = 8) +
    xlab("") + ylab("Percent of Groups") + coord_flip() +
    theme_bw(base_size = 20) +
    theme(strip.background = element_rect(fill = "white"),
          legend.position = "top", legend.direction = "horizontal") # legend.justification=c(1,1), legend.position=c(.85,1), 
  
}

###########################################



#######################################
#### Loading and Transforming Data ####

#### SP1 Rubric Responses ####
sp1Orig <- read_excel("../Rubric Responses/SP1_InClass.xlsx", sheet = "Responses")

## Transforming to long
# Observation Types
sp1obtype <- sp1Orig %>%
  select(Semester, Time, Group, ends_with("_Ob.Type")) %>%
  gather(Item, Ob.Type,ends_with("_Ob.Type")) %>%
  mutate(Item = str_remove(Item, "_Ob.Type"))

# Scores, then joining OT
sp1long <- sp1Orig %>%
  select(-ends_with("_Ob.Type")) %>%
  gather(Item, Score, starts_with("Q")) %>%
  left_join(sp1obtype, by = c("Semester","Time","Group","Item")) %>%
  mutate(Score = na_if(Score, "B") %>% as.numeric(),
         Group = as.character(Group),
         Semester = fct_recode(Semester, T1 = "Fall 2017", T2 = "Fall 2018"))

## Exam and Course Performance outcomes in long format
SPOutcomesLong <- StudentInfo %>%
  select(Name:IC, Group = ESICI_Group, Sex,Ethnicity,Final_Course_Grade = Final_Course_Percent,Exam_1:Final_Exam) %>%
  gather(Outcome, Percent, Final_Course_Grade:Final_Exam) %>%
  mutate(Outcome = str_replace_all(Outcome,"_"," "),
         Semester = as_factor(Semester) %>%
           fct_recode(., C1 = "Spring 2016", C2 = "Fall 2016", T1 = "Fall 2017", T2 = "Fall 2018") %>%
           fct_relevel("C1","C2","T1","T2"))

##############################################################

###############################
#### Total score analysis  ####

#### Student-level Exam Performance ####
## Descriptives
OutcomeDescripsbySemester <- SPOutcomesLong %>%
  Get_Descriptives(Percent, Semester, Outcome, digits = 1)

## Output table
OutcomeTable <- OutcomeDescripsbySemester %>%
  filter(Outcome == "Final Exam") %>%
  select(Semester, n) %>% unique() %>%
  mutate(n = as.character(n),
         Outcome = "n") %>%
  spread(Semester, n) %>%
  bind_rows(OutcomeDescripsbySemester %>%
              filter(Outcome %in% c("Exam 2", "Final Exam")) %>%
              mutate(Temp = paste0(Mean," (",SD,")")) %>%
              select(Semester,Outcome,Temp) %>%
              spread(Semester,Temp))

## Density Plot
OutcomesbySemesterPlot <- SPOutcomesLong %>%
  filter(Outcome %in% c("Exam 2", "Final Exam")) %>%
  Get_DenPlot(xvar = "Percent",fillvar = "Semester",xname = "Percent Correct",
              xmin = 0, xmax = 100, xseq = 10) +
  facet_wrap(~Outcome) +
  theme(strip.background = element_rect(fill = "white"))
  #theme(legend.justification=c(1,0), legend.position=c(.95,.05))


#### Final exam 7A and 7B performance ####
## Data
Q7responses <- read_excel("Final Exam Question Analysis S16 F16 F17 F18 6oct2019.xlsx", sheet = "Item 7AB")
Q7format <- Q7responses %>%
  mutate_if(is.numeric, round) %>%
  group_by(IC, Item) %>%
  mutate(Total = sum(Count),
         Percent = round(Count / Total *100),
         pos = cumsum(Percent) - 0.5*Percent) %>%
  ungroup()

## 7A Plot
Q7A_Plot <- Q7format %>%
  filter(Item == "7A") %>%
  mutate_if(is.character, as_factor) %>%
  mutate(Score = fct_rev(Score)) %>%
  ggplot(., aes(x = fct_rev(IC), y = Percent, fill = Score)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Percent >= 7, Percent, ""), y = pos), color = "white", size = 8) +
  scale_fill_manual(values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c")),
                    guide = guide_legend(reverse = TRUE)) + 
  xlab("") + ylab("Percent of Students") + coord_flip() +
  theme_bw(base_size = 22) +
  theme(legend.position = "top")


  # guides(fill = guide_legend(nrow = 2, byrow = FALSE, reverse = TRUE))

## 7B Plot
Q7B_Plot <- Q7format %>%
  filter(Item == "7B") %>%
  mutate_if(is.character, as_factor) %>%
  mutate(Score = fct_rev(Score)) %>%
  ggplot(., aes(x = fct_rev(IC), y = Percent, fill = Score)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Percent >= 7, Percent, ""), y = pos), color = "white", size = 8) +
  scale_fill_manual(name = "", values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c")),
                    guide = guide_legend(reverse = TRUE)) + 
  xlab("") + ylab("Percent of Students") + coord_flip() +
  theme_bw(base_size = 22) +
  theme(legend.position = "top")
  
## Joint
Q7_Plot <- cowplot::plot_grid(Q7A_Plot, Q7B_Plot, ncol = 2, rel_widths = c(.7, 1),labels = c("7A", "7B"))

## MANOVA
FinalExam2multivar <- SPOutcomesLong %>%
  filter(Outcome %in% c("Exam 2", "Final Exam")) %>%
  spread(Outcome, Percent) %>%
  lm(cbind(`Exam 2`, `Final Exam`) ~ Semester, data = .)
summary(FinalExam2multivar)
anova(FinalExam2multivar)
summary.aov(FinalExam2multivar)
sjstats::omega_sq(summary.aov(FinalExam2multivar)[[1]]) # by hand: (3*(327.24 - 90.02)/(3*327.24 + (124 - 3) * 90.02)) or ((981.7 - 3*90.02))/(981.7+10802.7+90.2)
sjstats::omega_sq(summary.aov(FinalExam2multivar)[[2]])
  

#### Group-level performance - SP1 with Exams ####
SP1GroupScores <- sp1long %>% group_by(Semester, Group) %>%
  summarize(SP1 = sum(Score, na.rm = TRUE)) %>% ungroup()

SP1Descrips <- SP1GroupScores %>%
  Get_Descriptives(SP1, Semester)
SP1ttest <- t.test(SP1 ~ Semester, data = SP1GroupScores)

## SP Group Performance, including SP1 total score
SPScoreGroupSummary <- SPOutcomesLong %>%
  filter(Outcome %in% c("Exam 2","Final Exam") & !is.na(Group)) %>%
  Get_Descriptives(Percent, Semester, Group, Outcome) %>%
  mutate(Semester = fct_recode(Semester, T1 = "Fall 2017", T2 = "Fall 2018")) %>%
  left_join(SP1GroupScores, by = c("Semester","Group"))
  
## Correlations between Outcomes
OutcomeCorrsTemp <- SPScoreGroupSummary %>%
  select(Semester, Group, SP1, Outcome, Mean) %>%
  spread(Outcome, Mean)

## Correlations of ME activity score with Exams by Semester
OutcomeCorrs <- OutcomeCorrsTemp %>%
  FilteredCorrs("T1") %>%
  bind_rows(FilteredCorrs(OutcomeCorrsTemp, "T2"))

## Extracting correlation labels to add to scatterplot below
scatlabs <- OutcomeCorrs %>%
  filter(row == "SP1") %>%
  rename(Outcome = column) %>%
  mutate(SP1 = 33,
         Mean = case_when(Semester == "T1" ~ 65,
                          Semester == "T2" ~ 60),
         r = paste("r =", CorSig))

## Scatterplots grouped by semester
SPExamScatterPlot <- SPScoreGroupSummary %>%
  ggplot(aes(x = SP1, y = Mean, color = Semester)) +
  geom_smooth(method = "lm", se = FALSE, size = 2, linetype = 1) + #
  geom_point(aes(group = Semester, shape = Semester), size = 4) + #shape = 1
  scale_x_continuous(name = "ME Activity Score", limits = c(14,39),breaks = seq(15,35,5)) +
  scale_y_continuous(name = "Percent Correct", limits = c(55,95),breaks = seq(60,90,10)) +
  scale_color_brewer(palette = "Set2") +
  geom_text(data = scatlabs, aes(label = r), size = 8, show.legend = FALSE) +
  theme_bw(base_size = 20) +
  theme(legend.position = "top", strip.background = element_rect(fill = "white")) + #legend.justification=c(1,0), legend.position=c(.95,.05)
  facet_wrap(~Outcome)

####################################################################################

#############################
#### Item-level analysis ####

sp1ItemRecodes <- c(Q3A = "Q3Ai", `Q3A: Chymotrypsin` = "Q3Aii", `Q3A: Elastase` = "Q3Aiii", `Q3A: Trypsin` = "Q3Aiv",
                    `Q4A: Chymotrypsin+Substrate` = "Q4Aii", `Q4A: Elastase+Substrate` = "Q4Aiv", `Q4A: Trypsin+Substrate` = "Q4Avi",
                    Q4Avii = "Q4A-VII")

sp1ItemLevels <- c("Q2", "Q3A", "Q3A: Chymotrypsin", "Q3A: Elastase", "Q3A: Trypsin", "Q3Bi", "Q3Bii", "Q3C",
                   "Q4Ai", "Q4A: Chymotrypsin+Substrate", "Q4Aiii", "Q4A: Elastase+Substrate", "Q4Av", "Q4A: Trypsin+Substrate", "Q4Avii",
                   "Q4B", "Q4C")

#### Item Score Freq and % by Semester ####
SPItemSummary <- sp1long %>%
  filter(!is.na(Score)) %>%
  group_by(Semester,Item,Score)%>%
  summarize(Responses = n()) %>%
  mutate(Total = sum(Responses),
         Percent = Responses / Total * 100,
         Percent_Round = round(Percent),
         Score = factor(Score, levels = c(0, 1, 2, 3)),
         pos = cumsum(Percent) - 0.5*Percent) %>%         # Location where Percent will be printed on the bar
  ungroup() %>%
  mutate(Item = as_factor(Item) %>%
           fct_recode(!!!sp1ItemRecodes) %>%
           fct_relevel(sp1ItemLevels)) %>% 
  group_by(Item) %>%
  mutate(Name = paste0(Item," (", max(as.numeric(Score) - 1), ")")) %>%
  ungroup() %>%
  mutate(Score = fct_rev(Score),
         Name = as_factor(Name) %>% fct_relevel("Q4Avii (1)", after = 14)) # Ordering response options for plotting

## Plot
SP1ItemSemesterPlot <- SPItemSummary %>%
  ggplot(aes(x = fct_rev(Semester), y = Percent, fill = Score)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Percent >= 7, Percent_Round, ""), y = pos), color = "white", size = 8) +
  scale_fill_manual(values = rev(c("#b3cde3","#8c96c6","#8856a7","#810f7c"))) + #scale_fill_brewer(palette = "BuPu", direction = -1) +
  xlab("") + ylab("Percent of Groups") + coord_flip() +
  theme_bw(base_size = 22) +
  facet_wrap(~Name, ncol = 2) +
  theme(strip.background = element_rect(fill = "white"))


#### Item Observation Type by Semester ####
SPItemObTypeSummary <- sp1long %>%
  mutate(Ob.Type = str_replace_all(Ob.Type, "S", "G") %>% 
           str_replace("T", "S") %>%
           na_if("NONE")) %>%
  filter(!is.na(Ob.Type)) %>%
  group_by(Semester,Item,Ob.Type)%>%
  summarize(Responses = n()) %>%
  mutate(Total = sum(Responses),
         Percent = round(Responses / Total *100),
         Ob.Type = factor(Ob.Type, levels = c("E","G","S","EG","ES","GS","EGS"))) %>%
  arrange(Semester,Item,Ob.Type) %>%
  group_by(Semester, Item) %>%
  mutate(pos = cumsum(Percent) - 0.5*Percent) %>%  # Location where Percent will be printed on the bar
  ungroup() %>%
  mutate(Ob.Type = fct_rev(Ob.Type),
         Item = as_factor(Item) %>%
           fct_recode(!!!sp1ItemRecodes) %>%
           fct_relevel(sp1ItemLevels))

# TypePalette <- RColorBrewer::brewer.pal(name = "Set1", n = 7)[c(1, 7, 5, 2, 3, 4)]
TypePalette2 <- RColorBrewer::brewer.pal(name = "Set2", n = 7)

SP1ItemObTypeSemesterPlot_Q2 <- SPItemObTypeSummary %>%
  filter(str_detect(Item, "Q2")) %>%
  ObTypePlot() +
  scale_fill_manual(name = "Type", values = TypePalette2, guide = guide_legend(reverse = TRUE))

SP1ItemObTypeSemesterPlot_Q3 <- SPItemObTypeSummary %>%
  filter(str_detect(Item, "Q3")) %>%
  ObTypePlot() +
  scale_fill_manual(name = "Type", values = TypePalette2, guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~Item, ncol = 3)

SP1ItemObTypeSemesterPlot_Q4 <- SPItemObTypeSummary %>%
  filter(str_detect(Item, "Q4")) %>%
  ObTypePlot() +
  scale_fill_manual(name = "Type", values = rev(TypePalette2[c(3,2,1,4,5,6)]), guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~Item, ncol = 3)

SP1ItemObTypeSemesterPlot_All <- SPItemObTypeSummary %>%
  mutate(Item = fct_rev(Item)) %>%
  ObTypePlot() +
  scale_fill_manual(name = "Type", values = rev(TypePalette2[c(3,2,1,4,5,6)]), guide = guide_legend(reverse = TRUE)) +
  facet_wrap(~Item, ncol = 3, as.table = FALSE)

#### Comparing Item Score and Exam Performance ####
SP1ItemswithExam <- sp1long %>%
  left_join(SPScoreGroupSummary %>%
              select(Semester,Group,Outcome,Mean),
            by = c("Semester","Group"))

## shortcut function for getting spearman correlation
Get_ItemExamCorr <- function(data,ItemNum,Exam){
  
  filtdat <- data %>%
    filter(Item == ItemNum & Outcome == Exam)
  
  cor(filtdat$Score,filtdat$Mean, method = "spearman", use = "pairwise.complete.obs")
  
}

ItemExamCorrs <- data.frame(Item = unique(SP1ItemswithExam$Item),
                            Exam_2 = map_dbl(unique(SP1ItemswithExam$Item),~Get_ItemExamCorr(SP1ItemswithExam,.x,"Exam 2")),
                            Final_Exam = map_dbl(unique(SP1ItemswithExam$Item),~Get_ItemExamCorr(SP1ItemswithExam,.x,"Final Exam")))



ExambyItemScore <- SP1ItemswithExam %>%
  group_by(Item,Score,Outcome) %>%
  summarize(n = sum(!is.na(Mean)),
            Mean_Score = mean(Mean,na.rm = TRUE),
            SD = sd(Mean, na.rm = TRUE))
  



#### Saving Objects and Exporting Plots & Datasets ####
## Plots
ggsave(SP1ItemSemesterPlot, file = "ME_Item_Score_Plot.tiff", height = 17, width = 17)
ggsave(SP1ItemObTypeSemesterPlot_All, file = "ME_AllItem_Misconception_Plot.tiff", height = 10, width = 15)
ggsave(SP1ItemObTypeSemesterPlot_Q2, file = "ME_Q2_Misconception_Plot.tiff", height = 4, width = 4)
ggsave(SP1ItemObTypeSemesterPlot_Q3, file = "ME_Q3_Misconception_Plot.tiff", height = 4, width = 11)
ggsave(SP1ItemObTypeSemesterPlot_Q4, file = "ME_Q4_Misconception_Plot.tiff", height = 4, width = 11)
ggsave(OutcomesbySemesterPlot, file = "Exam_Semester_DensityPlot.tiff", height = 8, width = 12) #file = "SP Results/FinalExam2bySemesterPlot.png", height = 7, width = 12)
ggsave(SPExamScatterPlot, file = "Exam_ME_ScatterPlot.tiff", height = 6, width = 12)
ggsave(Q7_Plot, file = "FinalExam_Q7_Plot.tiff", height = 3, width = 14)

## Objects
save(SPOutcomesLong,SPScoreGroupSummary,SPItemSummary,               # Data/Tables
     SPItemObTypeSummary,SP1ItemswithExam,
     OutcomeCorrs,ItemExamCorrs,                                     # Correlations
     SP1ItemSemesterPlot,SP1ItemObTypeSemesterPlot_All,                  # Plots
     OutcomesbySemesterPlot,SPExamScatterPlot,   
     file = "SP Results/SP1Output.RData")

# load("SP Results/SP1Output.RData")



  
  