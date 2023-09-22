###########################
#                         #
#       Gather Data       #
#                         #
###########################

library(checkpoint)
checkpoint("2020-10-21")

#### Packages ####
library(haven)
library(tidyr)
library(dplyr)
library(stringr)
library(forcats)
library(ggplot2)
library(readxl)


###############################################
#### Rubric data downloaded from Qualtrics ####

## Loading data in its original form
RubricsOrig <- read_sav("../Rubric Responses/ESICI Rubrics_August_22_2019.sav") %>%
  mutate_at(vars(ResponseId,ID),as.character)

## Saving names of variables from the Preliminary block
PrelimVars <- RubricsOrig %>% select(Finished:ResponseId,Semester:ID) %>% names()

## Converting NAs, filtering test cases, and removing unnecessary SPSS variables
RubricConverted <- RubricsOrig %>%
  filter(!(DistributionChannel %in% c("preview","test"))) %>%           # Removing test cases
  mutate_at(vars(Semester,Activity,Exam_Time,Time),as_factor) %>%       # Converting from numeric to labels 
  mutate_if(is.character, ~ifelse(. %in% c("","99","NA"), NA, .)) %>%   # Converting reponses of 99, blank, or "NA" to NA
  mutate_if(is.numeric, ~na_if(.,99)) %>%
  select(one_of(PrelimVars),HK_Pre_1a1:last_col())

#### Transform Data to Long Format and Joining Together ####
## Reflection Items
RubricsReflect <- RubricConverted %>% select(one_of(PrelimVars),contains("Reflect")) %>%
  gather(Item,Reflection,contains("Reflect")) %>%
  mutate(Item = str_sub(Item,-9,-1)) %>%
  filter(!is.na(Reflection))

## Self-Rating Items
RubricsSelfRate <- RubricConverted %>% select(one_of(PrelimVars),contains("SelfRate")) %>%
  gather(Item,Rating,contains("SelfRate")) %>%
  mutate(Item = str_sub(Item,-10,-1)) %>%
  filter(!is.na(Rating))

## Rubric Responses and Comments
# Comments
RubricsComments <- RubricConverted %>% select(one_of(PrelimVars),ends_with("Comment")) %>%
  gather(Item,Comment,ends_with("Comment")) %>%
  mutate(Item = str_remove(Item,"_Comment"))

# Rubric responses, then adding comments
Rubrics <- RubricConverted %>%
  select(-ends_with("Comment"),-contains("SelfRate"),-contains("Reflect")) %>%
  gather(Item,Response,-one_of(PrelimVars)) %>%
  left_join(RubricsComments,by = c(PrelimVars,"Item")) %>%  # Not sure if this is the best way to join
  filter(!is.na(Response)) %>%
  mutate(Item = str_sub(Item,-4,-1))

####################################################


##################################################
#### Demographic, Exam, and Group information ####

# #### 2016 - 2017 ####
# # Note: appear to be missing information from Nikki Ripp and Roy Montgomery from fall 2016
# Demos1617 <- read_excel("../Biochemistry Students_demographics_S16 F16 F17.xlsx") %>% # Demographic info and exam scores
#   rename(ID = x500, ACT_Math = `ACT-math`, Exam_1 = E1, Exam_2 = E2, Exam_3 = E3)
# names(Demos1617) <- str_replace_all(names(Demos1617)," ","_")
# 
# #### 2018 ####
# Demos18 <- read_excel("../C_BIOC3321F2018grades&stuDemogrEtcInfo_011819_deidentified_final.xlsx", sheet = "F18 001") %>%
#   bind_rows(read_excel("../C_BIOC3321F2018grades&stuDemogrEtcInfo_011819_deidentified_final.xlsx", sheet = "F18 002")) %>%
#   select(ID = X500, Sex = GENDER, Ethnicity = ETHNICITY, ACT_Math = ACT_MATH,
#          Cumulative_GPA = CUM_GPA_END_OF_F2018, Final_Course_Percent = `Final course grade (%)`,
#          Exam_1 = `Exam 1 percent`, Exam_2 = `Exam 2 percnet`, Exam_3 = `Exam 3 percent`, Final_Exam = `Final exam percent`) %>%
#   mutate(Semester = "Fall 2018",
#          Final_Letter = case_when(Final_Course_Percent >= 92 ~ "A",
#                                   Final_Course_Percent < 92 & Final_Course_Percent >= 90 ~ "A-",
#                                   Final_Course_Percent < 90 & Final_Course_Percent >= 87 ~ "B+",
#                                   Final_Course_Percent < 87 & Final_Course_Percent >= 83 ~ "B",
#                                   Final_Course_Percent < 83 & Final_Course_Percent >= 80 ~ "B-",
#                                   Final_Course_Percent < 80 & Final_Course_Percent >= 77 ~ "C+",
#                                   Final_Course_Percent < 77 & Final_Course_Percent >= 73 ~ "C",
#                                   Final_Course_Percent < 73 & Final_Course_Percent >= 70 ~ "C-",
#                                   Final_Course_Percent < 70 & Final_Course_Percent >= 65 ~ "D",
#                                   Final_Course_Percent < 65 ~ "F"))
# 
# #### Group Info and Combining Together ####
# ## Condition and Group info
# GroupInfo <- read_excel("../Final Exam Item and Total Scores.xlsx") %>%                 
#   select(Name:IC,ESICI_Group) %>% unique()        
# 
# ## Combining together
# StudentInfo <- bind_rows(Demos1617,Demos18) %>%                         # 126 observations from the Demos and 130 from the Groups (44 obs are from 2015 and not expected to match)
#   inner_join(GroupInfo, by = c("ID", "Semester")) %>%
#   select(Name,ID,Semester,Section,IC,Group = ESICI_Group,everything())
# anti <- GroupInfo %>% anti_join(bind_rows(Demos1617,Demos18),by = c("ID", "Semester"))

# # Exporting demographics for use in Card Sorting Project
# library(openxlsx)
# write.xlsx(StudentInfo, file = "StudentInfo.xlsx")

StudentInfo <- read_excel("../StudentInfo.xlsx")

##################################################

######################################################
#### Functions used regularly in analysis scripts ####

#### Getting summary descriptives on a continuous variable ####
## unquoted grouping variables should be specified in ...
Get_Descriptives <- function(data,ContinuousVariable,...,digits=5){
  
  groups <- quos(...)
  CV <- enquo(ContinuousVariable)
  
  data_descrip <- data %>% group_by(!!!groups) %>%
    summarize(n = sum(!is.na(!!CV)),
              Median = median(!!CV, na.rm=TRUE),
              Mean = mean(!!CV, na.rm=TRUE),
              SE = sd(!!CV, na.rm=TRUE)/sqrt(sum(!is.na(!!CV))),
              SD = sd(!!CV, na.rm=TRUE),
              Min = min(!!CV, na.rm=TRUE),
              Max = max(!!CV, na.rm=TRUE)) %>% ungroup() %>%
    mutate(SE = ifelse(Min==0 & Max==1,(Mean*(1-Mean))/sqrt(n),SE),
           SD = ifelse(Min==0 & Max==1,NA,SD)) %>%
    mutate_if(is.numeric,~round(.,digits=digits))
  
  return(data_descrip)
}

#### Computing of correlation matrix ####
## adapted from http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        digits=2,
                        usena="complete.obs",
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL,
                        alpha = .05, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  ## Correlation matrix
  cormat<-format(round(cor(x, use = usena, ...),digits),nsmall=digits)
  pmat<-format(round(cor.pmat(x, ...),digits),nsmall=digits)
  # Reorder correlation matrix
  # ord<-corrMatOrder(cormat, order="hclust")
  # cormat<-cormat[ord, ord]
  # pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  # sym<-symnum(cormat, abbr.colnames=FALSE)
  
  ## Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  
  ## Get lower/upper triangle or flatten
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
    return(list(r=cormat, p=pmat)) #,sym=sym
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    # sym=t(sym)
    return(list(r=cormat, p=pmat)) #,sym=sym
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat) %>%
      mutate(CorSig = paste0(cor,ifelse(as.numeric(as.character(p)) < alpha, "*", " ")))
    return(cormat)
  }
  
}
