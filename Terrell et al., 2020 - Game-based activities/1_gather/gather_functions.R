#############################################
#                                           #
#                                           #
#    Gather and Transform - Card Sorting    #
#                                           #
#                                           #
#############################################



# ------------------------------------------------------------------------
# IDs and Groups
# ------------------------------------------------------------------------


IDs <- read_excel("Total analysis sheets/BIOC S16 F16 F17 F18 Names and X500.xlsx") %>%
  rename(ID = X500,Term = `Enrolled term`)

Groups <- read_excel("Total analysis sheets/BIOC Groups S16 F16 F17 F18.xlsx", skip=1) %>%
  mutate(Group = str_remove(Group, "Group "))
maxNperGroup <- max(c(str_count(Groups$Pre,","), str_count(Groups$Post,",")), na.rm=TRUE) + 1 # Determining maximum number of students in a group


IDGroups <- Groups %>%
  mutate_at(vars(Pre,Post),~str_replace_all(.,", ",",")) %>%
  mutate(Group_Change = ifelse(Pre == Post,0,1)) %>%                              # Did membership in the group change from pre to post? A change is almost always due to a student dropping out
  gather(Time,Students,Pre,Post) %>%
  separate(Students, into = paste("Student",1:maxNperGroup,sep="_"), sep=",") %>%
  gather(Temp,Name,starts_with("Student_")) %>%
  inner_join(IDs, by = c("Name","Term")) %>%
  separate(Term,into = c("Semester","Section"), sep = " \\(") %>%
  mutate(Section = str_remove(Section,"\\)")) %>%
  select(-Temp) %>%
  filter(Group!="13") # Group 13 was discontinued; These are the only students who had multiple groups.

## Checking join before doing inner_join
# GroupNoID <- anti_join(IDGroups,IDs,by = c("Name","Term")) %>% drop_na(Name) # See notes in 0_general_functions.R for changes
# IDNoGroup<- anti_join(IDs,IDGroups,by = c("Name","Term")) # Resolved
# numberscheck <- IDGroups %>% select(-Time) %>% unique() %>% count(Semester,ID)
# check2 <- IDGroups %>% group_by(Name,ID,Group) %>% summarize(n = n()) %>% mutate(n2 = n())



# ------------------------------------------------------------------------
# Card-Level Data
# ------------------------------------------------------------------------

#########################
#### 2016-2017 cards ####

#### Load Data ####
## With color-coded correctness on a 0 - 5 scale
InitialColorCards <- Read_Color_Data("Spring 2016") %>%
  bind_rows(Read_Color_Data("Fall 2016")) %>%
  bind_rows(Read_Color_Data("Fall 2017")) %>%
  bind_rows(Read_Color_Data("Alverno")) %>%
  filter(!(Card %in% c("Average","Median","Mode","Sum")) & !is.na(Card)) # Removing blank or vestigial summary rows

## Cards not used in all semesters
CardCount <- InitialColorCards %>% count(Card) # As expected based on the info provided in "Comparing data from 2016 to 2017.xlsx".
NotAllDeckCards <- CardCount[CardCount$n < 4,]$Card


#### Formatting and Scoring ####
## Convert Group Pre and Post responses to long format,
## separate complex data columns into simple data columns, and recode colors to scores
TheColorCards <- InitialColorCards %>%
  gather(Temp,Color_Value,ends_with("_color")) %>%
  separate(Temp,c("Var1","Group","Time","Var2"),sep="_") %>%
  mutate(IC = car::recode(Semester,"'Fall 2017'='Intervention';c('Spring 2016','Fall 2016')='Control';else=NA",
                          as.factor=TRUE,levels=c("Intervention","Control")),
         Time = car::recode(Time,"'pre'='Pre';'post'='Post'",as.factor=TRUE,levels=c("Pre","Post")),
         Card_ID = str_sub(Card,1,4),
         Card_Description = str_sub(Card,6,-5),
         Card_Category = str_sub(Card,-3,-1),
         Score = car::recode(Color_Value,"3=NA;c(2,40,-4142)=5;c(4,48,50)=4;8=3;7=2;45=1;18=0",as.numeric=TRUE)) %>%
  select(Semester,IC,Group,Time,starts_with("Card"),Score) %>%
  filter(!is.na(Score)) # removing rows that don't have a Score

# check <- TheCards %>% filter(is.na(Score)) %>%
#   count(Semester,Group,Time)


#####################################################


####################
#### 2018 cards ####

#### Load Data ####
## With correct/incorrect pile designation

## Correct Answers
# The 2017 and CombinedCorrected sheets contain the same information
# 2 cards (1193 and 2066) are listed twice because they can be in one of two piles
CorrectCards18 <- read_excel("Total analysis sheets/Instructor Decks.xlsx",sheet = "CombinedCorrected") %>%
  filter(Year == 2017) %>%
  mutate(Card_ID = str_sub(Card,1,4),
         Card_Description = str_sub(Card,6,-5),
         Card_Category = str_sub(Card,-3,-1))


# TheCorIncCards3 <- Read_CorInc_Data("Total analysis sheets/Post sort Fall 2018.xlsx",3,CorrectCards18,"Fall 2018","Post")

## Fall 2018 Pre and Post Cards
# Contains cards that were in the wrong pile, then compares to correct pile for scoring all cards 1/0
TheCorIncCards <- map_dfr(1:11, ~Read_CorInc_Data(file = "Total analysis sheets/Pre sort Fall 2018.xlsx",sheet = .x,
                                                  AnswerKey = CorrectCards18, semester = "Fall 2018",prepost = "Pre")) %>%
  bind_rows(map_dfr(1:11, ~Read_CorInc_Data(file = "Total analysis sheets/Post sort Fall 2018.xlsx",sheet = .x,
                                            AnswerKey = CorrectCards18, semester = "Fall 2018",prepost = "Post"))) %>%
  mutate(Students = ifelse(str_detect(Students,"incorrect cards in each pile"),NA,Students))

## Cards in wrong pile (Pre/Post sort Fall 2018), but I have no record of these being cards from Instructor Decks.xlsx
NotCards <- TheCorIncCards %>% filter(is.na(Pile)) # Corrected


########################################


# ------------------------------------------------------------------------
# Pile-Level Data
# ------------------------------------------------------------------------

## Only applicable to 2016-2017 data when students were allowed to create their own piles
## In 2018, students were told there were 4 piles with specific names

#### Load Data ####
## And remove blank or irrelevant rows
InitialPiles <- read_excel("Total analysis sheets/2016 total stat data.xlsx",sheet="Group data") %>%
  bind_rows(read_excel("Total analysis sheets/2017 total stat data.xlsx")) %>%
  rename(Semester=X__1,Group=X__2) %>%
  filter(!is.na(Semester) & Semester!="Stats")

#### Formatting ####
## Extract percentages and put in long format
PercentPiles <- InitialPiles %>% select(Semester,Group,starts_with("%")) %>%
  gather(Temp,Percent,starts_with("%")) %>%
  separate(Temp,into=c("Name_Type","Time"),sep=" - ") %>%
  mutate(Name_Type = str_remove(Name_Type,"% piles named based on "))

## Extract total piles, put in long format, join percentages, and calculate number of piles for each name type
ThePiles <- InitialPiles %>% select(Semester,Group,starts_with("number")) %>%
  gather(Time,Total_Piles,starts_with("number")) %>%
  mutate(Time = str_remove(Time,"number of piles ")) %>%
  inner_join(PercentPiles,by=c("Semester","Group","Time")) %>%
  mutate(Piles = round(Total_Piles*Percent/100),
         Group = str_remove(Group,"Group "),
         Time = car::recode(Time,"'pre'='Pre';'post'='Post'",as.factor=TRUE,levels=c("Pre","Post")),
         IC = car::recode(Semester,"'Fall 2017'='Intervention';c('Spring 2016','Fall 2016')='Control';else=NA",
                            as.factor=TRUE,levels=c("Intervention","Control"))) %>%
  select(Semester,IC,Group,Time,everything())

########################################

# ------------------------------------------------------------------------
# Exam Data
# ------------------------------------------------------------------------

###########################
#### Exams 1, 2, and 3 ####

#### Load Data ####
# Load by sheet, then combine by Name and semester
Exam123Orig <- read_excel("Total analysis sheets/Exam 1, 2, and 3 - card sorting question.xlsx",sheet=1,na=c("","NA")) %>%
  full_join(read_excel("Total analysis sheets/Exam 1, 2, and 3 - card sorting question.xlsx",sheet=2,na=c("","NA")),
            by=c("Name","Semester")) %>%
  full_join(read_excel("Total analysis sheets/Exam 1, 2, and 3 - card sorting question.xlsx",sheet=3,na=c("","NA")),
            by=c("Name","Semester")) %>%
  select(-contains("For fall"))

## Put in long format
Exam123long <- Exam123Orig %>% gather(Item,ResponseOrig,-Name,-Semester)
## Extracting correct response
Exam123Correct <- Exam123long %>% filter(Name=="Correct Response") %>%
  select(Item,Correct=ResponseOrig)

## Filtering Correct Response and recoding correct responses that are in a different order from Correct Response
# RespFreqCheck <- Exam123long %>%
#   group_by(Item, ResponseOrig) %>%
#   summarize(n = n())
Exam123long <- Exam123long %>% filter(Name!="Correct Response") %>%
  left_join(Exam123Correct,by="Item") %>%
  mutate(Response = car::recode(ResponseOrig,
                                 "'IV,I'='I,IV';'VIII,III'='III,VIII';'I,G'='G,I';'H,B'='B,H';'J,D'='D,J';'I,D'='D,I';c('C,E,D,I,J','E,J,I,C,D','D,J,E,C,I')='C,D,E,I,J'"))

## Scoring exam
Exam123scored <- Exam123long %>%
  mutate(Score = ifelse(Response == Correct, 1, 0)) %>%
  mutate(Score = ifelse(Semester=="Fall 2018" & Item=="Signaling carbohydrates" & Response=="D,I",
                        1, Score)) # Correct Response is different in 2018 than 2017

#### Adding x500 to merge with other files ####
Exam123 <- Exam123scored %>%
  left_join(IDGroups %>% select(-Time) %>% unique(), by = c("Name", "Semester")) %>%
  select(Name,ID,Semester,Section,Group,everything())

# IDnoExam123 <-  IDGroups %>% select(-Time) %>% unique() %>% anti_join(Exam123scored, by = c("Name", "Semester")) # All 2016
# anticheck <- Exam123scored %>% anti_join(IDGroups %>% select(-Time) %>% unique(), by = c("Name", "Semester")) # 1 student who did not complete semester
###########################################


####################
#### Final Exam ####

#### Load Data ####
FinalExamOrig <- read_excel("Total analysis sheets/Final exam visualization question data.xlsx")

## Separate by semester, rename and reorganize columns, then bind back together
FinalExamReformat <- Format_Final_Exam(FinalExamOrig,X__1:Totals) %>%
  bind_rows(Format_Final_Exam(FinalExamOrig,X__4:Totals__1)) %>%
  bind_rows(Format_Final_Exam(FinalExamOrig,X__7:Totals__2)) %>%
  bind_rows(Format_Final_Exam(FinalExamOrig,X__10:Totals__3)) %>%
  bind_rows(Format_Final_Exam(FinalExamOrig,X__13:Totals__4)) %>%
  bind_rows(Format_Final_Exam(FinalExamOrig,X__16:Totals__5) %>%
              mutate(Totals = as.character(Totals)))

## Same Semester/ID combo?; each Semester/ID combo should have 8 responses
MultiID <- FinalExamReformat %>% count(Semester,ID) ## SS, Fall 2015 only repeat now

#### Scoring ####

## Students with more than 1 response to an item
MultiResponse <- FinalExamReformat %>% group_by(Semester,ID,Item,Response) %>%
  summarize(nResp = n()) %>% mutate(nItem = n())
FixMultiResponse <- MultiResponse %>%
  filter(nItem > 1) %>%
  mutate(Multi_Response = "Multiple") %>%
  select(-starts_with("n"))

## Rescoring multiple responses to same item as incorrect, then removing duplicate rows
FinalExamScored <- FinalExamReformat %>%
  left_join(FixMultiResponse, by = c("Semester", "ID", "Item", "Response")) %>%
  mutate(Score = ifelse(is.na(Multi_Response), Score, 0),
         Response = ifelse(is.na(Multi_Response), Response, "Multiple")) %>%
  unique() %>%
  mutate(Diff = as.numeric(Totals) - Score,
         ID = tolower(ID))

## Adding Student information
FinalExam <- FinalExamScored %>%
  left_join(IDGroups %>% select(-Time) %>% unique(), by = c("ID", "Semester")) %>%
  mutate(IC = car::recode(Semester,"c('Fall 2018','Fall 2017')='Intervention';c('Spring 2016','Fall 2016')='Control';else=NA",
                          as.factor=TRUE,levels=c("Intervention","Control")),
         Sex = str_to_title(Sex)) %>%
  select(Name,ID,Semester,Section,IC,Group,everything())

IDnoExam <-  IDGroups %>% select(-Time) %>% unique() %>% anti_join(FinalExamScored, by = c("ID", "Semester")) # Emily Radke
anticheck <- FinalExamScored %>% anti_join(IDGroups %>% select(-Time) %>% unique(), by = c("ID", "Semester")) # All abbreviation IDs rather than x500s

##############################################