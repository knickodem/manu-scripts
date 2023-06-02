
#### TO-DO:
# # Need to make sure variables are numeric or factor rather than character otherwise SPSS codes NA as "" rather than missing
# # can use labelled package functions to make na values explicit
# 
# table(testing_labels$SPED_PLACEMENT_2, useNA = "always")
# str(allinone$SPED_PLACEMENT_2)
# 
# nchar(allinone$RELATED_SPED_SERVICES[[4]]) # need to set string width to maximum char width, otherwise defaults to 255 and creates extra variables
# str(testing_labels$RELATED_SPED_SERVICES)
# 
# purrr::map_chr(names(allinone),
#                ~is.factor(allinone[[.x]]))

# write_sav(allinone, "testing_labels.sav")
# testing_labels <- read_sav("testing_labels.sav")

library(dplyr)
library(haven)
library(forcats)

# setwd("/Volumes/GoogleDrive/My Drive/IES RCT MIAMI")

##################################################################
# Read administrative data and calculate summary variables
##################################################################

## course and test performance
administrative.orig <- readxl::read_excel("00 Data/Administrative Data/UNC Data requested for students - 10-28-2022.xlsx")

## grade and exceptionality
exceptionality <- readr::read_csv("00 Data/Administrative Data/Disability Identification/ROSTER_ADMIN_CONSENTS_ESE PP2_Updated.csv") %>%
  mutate(ID = as.character(`Student ID`)) %>%
  filter(!is.na(ID)) %>%
  select(ID, Grade, ESE)

# # who is in each dataset?
# # admin IDs have leading 0s
# admin.no.except <- unique(administrative.orig$ID[!sub("^0+", "", administrative.orig$ID) %in% exceptionality$ID]) # 6 in admin, not except
# except.no.admin <- exceptionality$ID[!exceptionality$ID %in% sub("^0+", "", administrative.orig$ID)] # 15 in except, not admin


## gathering effort, conduct, and grade information for each period within course within student
# recoding effort, conduct, and grade values to be on consistent scales where higher values indicated better effort/conduct/grade
period <- administrative.orig %>%
  select(ID, Course = `COURSE TITLE`, starts_with("PERIOD")) %>%
  tidyr::pivot_longer(cols = -c(ID, Course), names_to = c("Period", "Type"), names_prefix = "PERIOD ", names_sep = " ", values_to =  "Temp") %>%
  mutate(Grade = case_when(Temp == "1" ~ 3, # reverse coding effort so higher value indicates better effort
                           Temp == "2" ~ 2,
                           Temp == "3" ~ 1,
                           Temp == "A" ~ 4, # recoding A-F Conduct and grades as 4, 3, 2, 1, and 0, respectively
                           Temp == "B" ~ 3,
                           Temp == "C" ~ 2,
                           Temp == "D" ~ 1,
                           Temp == "F" ~ 0,
                           Temp == "E" ~ 4, # recoding Kindergarten grades
                           Temp == "G" ~ 3,
                           Temp == "S" ~ 2,
                           Temp == "M" ~ 1,
                           Temp == "U" ~ 0))

## calculating final effort, conduct, and grade for each course
course <- period %>%
  group_by(ID, Type, Course) %>%
  summarize(Mean = mean(Grade, na.rm = TRUE),
            .groups = "drop_last")

# How does the calculated final grade compare to final grade provided in dataset?
# curious <- course %>%
#   filter(Type == "GRADE") %>%
#   left_join(administrative.orig %>% select(ID, Course = `COURSE TITLE`, Final = `FINAL GRADE`) %>% unique(),
#             by = c("ID", "Course"))
# table(curious$Mean, curious$Final, useNA = "always")
# F = 0 - .99; D = 1 - 1.49; C = 1.5 - 2.49; B = 2.5 -3.49; A = 3.5 - 4 # confirmed in student handbook

## Aggregating scores across course (i.e., one value per type per student)
final <- course %>%
  summarize(Final = mean(Mean, na.rm = TRUE)) %>%
  tidyr::spread(Type, Final) %>%
  rename(GPA = GRADE) %>%
  ungroup()


## combining all administrative data into one dataset with one row per student
administrative <- final %>%
  full_join(administrative.orig %>%
              select(ID, EXCEPTIONALITY, SECTION_504 = `SECTION 504`,
                     ABSENCES_EXCUSED = `ABS. EXCUSED`, ABSENCES_UNEXCUSED = `ABS. UNEXCUSED`,
                     FSA_ELA_SCORE = `FSA ELA SCR`, FSA_ELA_LEVEL = `FSA ELA LVL`,
                     FSA_MATH_SCORE = `FSA MATH SCR`, FSA_MATH_LEVEL = `FSA MATH  LVL`,
                     FSA_SCIENCE_SCORE = `SCIENCE SCR`, FSA_SCIENCE_LEVEL = `SCIENCE LVL`) %>%
              unique(), by = "ID") %>%
  mutate(ID = sub("^0+", "", ID)) %>%  # remove leading 0s to match exceptionality and survey data IDs
  full_join(exceptionality, by = "ID") %>%  # joining grade and exceptionality
  mutate(across(matches("^ABSE|^FSA"), as.numeric)) %>% # warnings refer to converting "." to NA, which is what we want
  mutate(EXCEPTIONALITY_TYPE = case_when(!is.na(ESE) ~ ESE,
                                         is.na(ESE) ~ EXCEPTIONALITY) %>%  # ESE and EXCEPTIONALITY had cases the other was missing and no discrepancies
           factor(., levels = c("F", "G", "J", "K", "L", "P", "T", "V")) %>%
           fct_recode(`F - Speech Impairment` = "F", `G - Language Impairment` = "G",
                               `J - Emotional/Behavioral Disabilities` = "J", `K - Specific Learning Disability` = "K", 
                               `L - Gifted` = "L", `P - Autistic` = "P",
                               `T - Developmentally Delayed` = "T", `V - Other Health Impairment` = "V") %>%
           fct_explicit_na("None"),
         Grade = factor(Grade, levels = c("K", 1:5)),
         SECTION_504 = fct(SECTION_504, levels = c("Y", "N"), na = "")) %>%
  select(-ESE, -EXCEPTIONALITY) %>%
  rename(STUDENT_ID = ID)

# table(administrative$EXCEPTIONALITY_TYPE, administrative$ESE, useNA = "always")


##################################################################
# Read and Transform IEP Information
##################################################################

## New column names for each data sheet in IEP Tracking
iep.cols <- readxl::read_excel("00 Data/Administrative Data/IEPs/IEP_data_column_names_20230124.xlsx")

## Raw IEP Tracking data
iep.orig <- lapply(c("Sheet0", "IEP", "504", "Gifted"), function(x){
  readxl::read_excel("00 Data/Administrative Data/IEPs/IEP_data_20230124.xlsx", 
                     sheet = x, na = c("", "n/a"))})
# lapply(iep.orig, View)

####  Transforming each sheet where necessary   ####
# Transformations followed investigation of discrepancies and redundancies (see below for details)

## Sheet0 - Largely summary variables compiled from other sheets
sheet0.1 <- iep.orig[[1]][3:nrow(iep.orig[[1]]), 2:11] %>% # removing first 2 rows and keeping 10 columns
  purrr::set_names(iep.cols$Sheet0_R_Name[1:10]) %>%
  mutate(disability_id = gsub("gifted", "L - Gifted", disability_id) %>% # not sure why two rows did not have consistent coding with other rows
           gsub("sld", "K - Specific Learning Disabled", .),
         STUDENT_ID = gsub("\\D", "", STUDENT) %>% # extract student id to merge with other datasets
           sub("^0+", "", .),                      # remove leading 0s should they exist
         STUDENT_ID = ifelse(STUDENT_ID == "631974" & disability_id == "K - Specific Learning Disabled", "649933", STUDENT_ID), # same ID was used on both students with one being incorrect
         STUDENT_ID = ifelse(STUDENT_ID == "927735" & secondary_label == "ADHD", "565763", STUDENT_ID), # same ID was used on both students with one being incorrect
         TEACHER_ID = sub(" -.*", "", TEACHER)) %>% # extracting teacher id to merge with other datasets
  filter(!(STUDENT_ID == "749163" & is.na(IEP_CODE)) &       # removing mostly blank case (more complete case exists)
           !(STUDENT_ID == "818794" & grepl("---", STUDENT)) & # removing mostly blank case (more complete case exists)
           !(STUDENT_ID == "866681" & IEP_CODE == 3) &          # removing incorrect case (correct case exists)
           !(STUDENT_ID == "866946" & grepl("GIFTED", perc_sped_services)) &  # removing duplicate case that only differs by capitalization
           !(STUDENT_ID == "758409" & grepl("^Support/Facilitation", related_sped_services))) %>%  # removing incorrect case (correct case exists)
  unique() %>%                                                     # removing a duplicate case
  select(STUDENT_ID, everything(),
         -c(educ_placement)) # using SPED_PLACEMENT from iep sheet and GIFTED_PLACEMENT from gifted sheet

## IEP - details from student IEPs
iep.0 <- iep.orig[[2]][3:nrow(iep.orig[[2]]), 2:ncol(iep.orig[[2]])] %>% # removing first two rows and date column
  purrr::set_names(iep.cols$IEP_R)
# which columns are blank and can potentially be removed?
iep.blank <- iep.0 %>% select(where(~all(is.na(.x)))) %>% names()
iep.1 <- iep.0 %>%
  mutate(STUDENT_ID = gsub("\\D", "", STUDENT) %>% # extract student id to merge with other datasets
           sub("^0+", "", .),                      # remove leading 0s should they exist
         STUDENT_ID = ifelse(STUDENT_ID == "631974" & primary_label == "2", "649933", STUDENT_ID), # same ID was used on both students with one being incorrect
         STUDENT_ID = ifelse(STUDENT_ID == "927735" & secondary_label == "10", "565763", STUDENT_ID), # same ID was used on both students with one being incorrect
         SPED_PLACEMENT = gsub("[\r\n]", "", educ_placement) %>% # need to remove non-printing characters
           factor() %>%
           fct_recode(`General Education (0% SPED)` = "1", `Inclusion (1 - 40% SPED)` = "2",
                               `Self-contained (41 - 100% SPED)` = "3")) %>%
  filter(!(STUDENT_ID == "758409" & grepl("^Support/Facilitation", related_sped_services)) & # removing incorrect case (correct case exists))
         STUDENT_ID != "6319740631974") %>%  # removing duplicate case (other case has correct ID)
  select(STUDENT_ID, everything(),
         -all_of(iep.blank[1:4]),           # removing all NA columns but keeping ELL_minutes
         -c(TEACHER, SCHOOL, disability_id, # using sheet0 variable    
            educ_placement))                # using SPED_PLACEMENT 

# table(iep.1$educ_placement, iep.1$SPED_PLACEMENT, useNA = "always")

## 504 - details from 504s
five04.1 <- iep.orig[[3]][, 2:ncol(iep.orig[[3]])] %>% # removing date column
  purrr::set_names(iep.cols$Five04_R[1:13]) %>%
  mutate(STUDENT_ID = gsub("\\D", "", STUDENT) %>% # extract student id to merge with other datasets
           sub("^0+", "", .)) %>%                  # remove leading 0s should they exist
  select(STUDENT_ID, everything(),
         -c(TEACHER, SCHOOL, disability_id)) # using sheet0 variable

## Gifted - details from gifted plans
gifted.1 <- iep.orig[[4]][3:nrow(iep.orig[[4]]), ] %>%  # removing first 2 rows
  purrr::set_names(iep.cols$Gifted_R[1:12]) %>%
  mutate(STUDENT_ID = gsub("\\D", "", STUDENT) %>% # extract student id to merge with other datasets
           sub("^0+", "", .), # remove leading 0s should they exist
         GIFTED_PLACEMENT = case_when(Full_Gifted == 1 ~ "Full-Gifted", # these categories were mutually exclusive, so combined for efficiency
                                      Inclusion == 1 ~ "Inclusion",
                                      Pull_Out_Part_Time == 1 ~ "Pull Out Part-Time"),
         primary_label = tolower(primary_label),
         GIFTED_FOCUS_AREA = case_when(grepl("language", primary_label) ~ "Language Arts/English", # codes differed by upper/lower case and spelling
                                       grepl("math", primary_label) ~ "Mathematics",
                                       grepl("science", primary_label) ~ "Science",
                                       !is.na(primary_label) ~ "Unspecified"),
         IEP_CODE = ifelse(STUDENT_ID == "866681", 4, IEP_CODE)) %>% # should be 4 (IEP and EP) rather than 3 (EP only) to match sheet0 and iep.1
  filter(!(STUDENT_ID == "818794" & grepl("---", STUDENT)) & # removing blank case (complete case for student still exists)
           !(STUDENT_ID == "866946" & grepl("GIFTED", perc_sped_services))) %>%  # removing duplicate case that only differs by capitalization
  unique() %>%  # deleting a duplicate
  select(STUDENT_ID, everything(),
         -c(TEACHER, SCHOOL, disability_id, # using sheet0 variable
            educ_placement, Full_Gifted:Pull_Out_Part_Time, # replaced by GIFTED_PLACEMENT
            primary_label))  # replaced by GIFTED_FOCUS_AREA

# lapply(list(sheet0.1, iep.1, five04.1, gifted.1), names)

# ## any student not in sheet0 but in one of the other ones?
# table(iep.1$STUDENT_ID %in% sheet0.1$STUDENT_ID)
# table(five04.1$STUDENT_ID %in% sheet0.1$STUDENT_ID)
# table(gifted.1$STUDENT_ID %in% sheet0.1$STUDENT_ID)
# thus, we can use left_join; full join returns all cases which helps see if any unmatched

## combining all four datasets into one - should ultimately have same # of rows as sheet0
allinone <- full_join(rename_with(sheet0.1, .cols = -STUDENT_ID, .fn = ~paste0(., "_1")),
                      rename_with(iep.1, .cols = -STUDENT_ID, .fn = ~paste0(., "_2")), by = c("STUDENT_ID")) %>%
  full_join(rename_with(five04.1, .cols = -STUDENT_ID, .fn = ~paste0(., "_3")), by = c("STUDENT_ID")) %>%
  full_join(rename_with(gifted.1, .cols = -STUDENT_ID, .fn = ~paste0(., "_4")), by = c("STUDENT_ID")) %>%
  mutate(IEP_CODE = as.numeric(IEP_CODE_1),
         PRIMARY_CODE = case_when(grepl("ADHD", primary_label_1) ~ 3,
                                   !is.na(primary_label_2) ~ as.numeric(primary_label_2),
                                   !is.na(GIFTED_FOCUS_AREA_4) ~ 9,
                                   disability_id_1 == "L - Gifted" ~ 9),
         SECONDARY_CODE = case_when(grepl("[\r\n]", secondary_label_2) ~ 5,
                                    secondary_label_2 == "10" ~ 3, # ADHD with no 504 should be coded 3 (OHI)
                                     !is.na(secondary_label_2) ~ as.numeric(secondary_label_2)),
         TERTIARY_CODE = as.numeric(tertiary_label_2) %>% ifelse(. == 10, 3, .), # ADHD with no 504 should be coded 3 (OHI)
         PRIMARY_LABEL = primary_label_1,
         ADDITIONAL_LABELS = secondary_label_1,
         SPED_PLACEMENT_2 = if_else(is.na(SPED_PLACEMENT_2) & grepl("100%", perc_sped_services_1),  # filling in some NA values using info across sheets 
                                    "General Education (0% SPED)", as.character(SPED_PLACEMENT_2)) %>%
           fct(., levels = c("General Education (0% SPED)", "Inclusion (1 - 40% SPED)", "Self-contained (41 - 100% SPED)")),
         PERC_GENED = case_when(!is.na(perc_gened_2) ~ as.numeric(perc_gened_2),         # filling in some NA values using info across sheets
                                SPED_PLACEMENT_2 == "General Education (0% SPED)" ~ 100,
                                GIFTED_PLACEMENT_4 == "Full-Gifted" ~ 0,
                                grepl("28%|26%", perc_sped_services_4) ~ 28), # one case had a typo of 26 (gifted % was still 72%)
         PERC_SPED = case_when(!is.na(perc_sped_services_2) ~ as.numeric(perc_sped_services_2),
                               !is.na(perc_sped_services_3) ~ 0),     # all were 100% gen ed; we are not including gifted students
         # The related_sped_services_1 on Sheet0 is intended as a combination of the same column on the other 3 sheets,
         # but the original related_sped_services_1 has some copy-paste errors (e.g., "100% gen ed" instead of listing services)
         # and inconsistencies (e.g., "20 hours of gifted hours per week" and "20 Hours per week - Gifted Services")
         RELATED_SPED_SERVICES = case_when(!is.na(related_sped_services_2) ~ related_sped_services_2,
                                           !is.na(related_sped_service_3) ~ related_sped_service_3,
                                           !is.na(related_sped_services_4) ~ related_sped_services_4,
                                           !is.na(related_sped_services_1) ~ related_sped_services_1),
         GIFTED_PLACEMENT_4 = fct(GIFTED_PLACEMENT_4, levels = c("Full-Gifted", "Pull Out Part-Time", "Inclusion")),
         GIFTED_FOCUS_AREA_4 = as_factor(GIFTED_FOCUS_AREA_4)) %>%
  labelled::add_value_labels(IEP_CODE = c(`IEP Only` = 1, `504 Only` = 2, `EP Only` = 3, `IEP+EP` = 4, `504+EP` = 5),
                             PRIMARY_CODE = c(Autism = 1, SLD = 2, OHI = 3, EBD = 4, `Language Impairment` = 5, `Developmentally Delayed` = 6,
                                               Speech = 7, `Traumatic Brain Injury` = 8, Gifted = 9),
                             SECONDARY_CODE = c(Autism = 1, SLD = 2, OHI = 3, EBD = 4, `Language Impairment` = 5, `Developmentally Delayed` = 6,
                                               Speech = 7, `Traumatic Brain Injury` = 8, Gifted = 9, ADHD = 10, `Seizure Disorder` = 11, ELL = 12, ODD = 13),
                             TERTIARY_CODE = c(Autism = 1, SLD = 2, OHI = 3, EBD = 4, `Language Impairment` = 5, `Developmentally Delayed` = 6,
                                                Speech = 7, `Traumatic Brain Injury` = 8, Gifted = 9, ADHD = 10, `Seizure Disorder` = 11, ELL = 12, ODD = 13)) %>%
  # select(where(~!(all(is.na(.))))) %>% # any blank columns? Nope
  select(STUDENT_ID, TEACHER_ID_1, SCHOOL_1, disability_id_1, IEP_CODE:RELATED_SPED_SERVICES,
         SPED_PLACEMENT_2, GIFTED_PLACEMENT_4, GIFTED_FOCUS_AREA_4,
         all_of(paste0(names(iep.1)[10:75], "_2")),                       # unique IEP columns - all of the colorful columns in original dataset
         Extended_Time_3, Adapt_Physical_Environ_3, Adapt_Presentation_3) %>% # unique 504 columns
  rename_with(~stringr::str_remove(., "_[0-9]$")) # remove the numeric suffix I added to allow for comparing variables across sheets
  # select(STUDENT_ID, order(colnames(.)))



#### investigation of discrepancies and redundancies across IEP tracking data sheets #####

##  Multiple rows for same student
## How many students have multiple rows?
# 3 of these are also multiple in iep, 1 in gifted - problems have been resolved
# multiple.rows <- sheet0.1 %>%
#   count(STUDENT_ID) %>%
#   filter(n > 1)
# 
# thelabels <- allinone %>%
#   filter(STUDENT_ID %in% multiple.rows$STUDENT_ID) %>%
#   select(STUDENT_ID, disability_id_1, primary_label_1:primary_label_3, PRIMARY_LABEL,
#          secondary_label_1, secondary_label_2, tertiary_label_2,
#          GIFTED_PLACEMENT_4, SPED_PLACEMENT_2)
# Change rows for 631974 (V only) with primary label K to be ID 649933 (K and V)
# Change rows for 927735 (speech only) with ADHD to be ID 565763
# Duplicate rows for 758409 with one row more accurate
# Multiple IEP_CODE for 858779, so we adjusted IEP_CODE coding so we could combine rows
# Others had one row that was largely blank, which was deleted.


#### NOTE: remainder of this section is sorted by variable

# ## subset that helps answer all of the issues presented below
# allinone %>%
#   select(STUDENT_ID, disability_id_1, starts_with("IEP_CODE"), GIFTED_FOCUS_AREA_4, primary_label_1:primary_label_3,
#          PRIMARY_CODE, PRIMARY_LABEL, SECONDARY_CODE, ADDITIONAL_LABELS, TERTIARY_CODE,
#          secondary_label_1, secondary_label_2, tertiary_label_2, starts_with("related_sped")) %>%
#   View()

## disability_id - use sheet0 variable (has complete information)
# nocode <- allinone$STUDENT_ID[is.na(allinone$disability_id_1)]
# View(allinone[allinone$STUDENT_ID %in% nocode,])
# View(exceptionality[exceptionality$ID %in% nocode,])
# # Should the 3 students on the 504 sheet have a disability_id, possibly V - Other Health Impaired like other students with ADHD (see Sheet0)?
# # These are the only students without a disability_id on Sheet0 (with the except of two students who have no data whatsoever and two students with multiple rows one of which does have a disability_id)
# # All 3 are labeled as L - Gifted in the exceptionality dataset

## educ_placement - rename to sped_placement and use iep sheet. Gifted students will be coded separately based on the full-time, part-time, inclusion variables
# Done
# table(allinone$educ_placement_1, gsub("[\r\n]", "", allinone$educ_placement_2), useNA = "always")
# # educ_placement in iep sheet is coded accurately based on perc_sped_services column.
# # Full-gifted, inclusion, and pull out part time are mutually exclusive
# table(gifted.1$Full_Gifted, gifted.1$Inclusion, useNA = "always")
# table(gifted.1$Full_Gifted, gifted.1$Pull_Out_Part_Time, useNA = "always")
# table(gifted.1$Inclusion, gifted.1$Pull_Out_Part_Time, useNA = "always")


# perc_gen and perc_sped_services
# - use iep.1 variable as this is actually numeric, but NAs need to be calculated from sheet0 variable; five04 and gifted sheets replicate sheet0 variable for the most part
# table(allinone$educ_placement_1, allinone$perc_sped_services_1, useNA = "always")


## IEP_CODE - use sheet0 (has complete information, though value differs for 1 student in each of the other sheets 866681, 858779, 866681
# Corrected
# table(allinone$IEP_CODE_1, allinone$IEP_CODE_4, useNA = "always")
# 
# allinone %>%
#   filter(STUDENT_ID %in% c("866681", "858779")) %>%
#   select(STUDENT_ID, disability_id_1, SPED_PLACEMENT_2, GIFTED_PLACEMENT_4, IEP_CODE_1:IEP_CODE_4,
#          PRIMARY_LABEL:primary_label_3, secondary_label_1, secondary_label_2) %>%
#   View()

# there are 2 students where values in the IEP_CODE column (1 = IEP, 2 = 504, 3 = EP) differs by sheet in the data file.
# 858779 has a code of 1 on Sheet0 and 2 on sheet 504
# 866681 has two rows on Sheet0 as they have two disability codes (K - specific learning disabled and L - Gifted). Thus, this student has IEP_CODE values of 1 and 3 depending on sheet and row
# The initial plan was to have a single IEP_CODE column in the final dataset. How would you like these two students to be coded? Alternatively, we could create 3 columns - an IEP column, 504 column, and EP column - each coded 1 = Yes, 0 = No.


## primary_, secondary_, and tertiary_label
# use iep.1 variables as these have been systematically coded:
# Autism(1), SLD(2), OHI(3), EBD(4), Language(5), Developmentally Delayed(6), Speech(7), Traumatic Brain Injury(8), Gifted(9), ADHD(10), Siezure disorder(11), ELL(12), ODD(13)

# table(allinone$primary_label_1, allinone$PRIMARY_LABEL, useNA = "always")
# # Autism(1), SLD(2), OHI(3), EBD(4), Language(5), Developmentally Delayed(6), Speech(7), Traumatic Brain Injury(8), Gifted(9), ADHD(10), Seizure disorder(11), ELL(12), ODD(13)
# # ADHD - why is nobody labelled 10? Most either 3 or NA - All ADHD should be coded as 3 (according to Katie Graves), which would include NAs
# # ASD/autism - all labelled 1
# # Developmental Delay - all labelled 6
# # EBD - all labelled 4
# # gifted - all labelled 9
# # language impairment - all labelled 5
# table(allinone$secondary_label_1, allinone$secondary_label_2, useNA = "always")

# ## related sped services
# all.equal(allinone$related_sped_services_1, allinone$related_sped_services_2)
# all.equal(allinone$related_sped_services_1, allinone$RELATED_SPED_SERVICES)
# waldo::compare(allinone$related_sped_services_1, allinone$RELATED_SPED_SERVICES)

##################################################################
# Joining Administrative and IEP data
##################################################################

# adiep %>% select(EXCEPTIONALITY_TYPE, disability_id) %>% View()

adiep <- administrative %>%
  left_join(allinone, by = "STUDENT_ID") %>%
  mutate(EXCEPTIONALITY_TYPE = case_when(EXCEPTIONALITY_TYPE == "None" & grepl("J -", disability_id) ~ "J - Emotional/Behavioral Disabilities",
                                         EXCEPTIONALITY_TYPE == "None" & grepl("V -", disability_id) ~ "V - Other Health Impairment",
                                         TRUE ~ as.character(EXCEPTIONALITY_TYPE)) %>%
           as_factor() %>% fct_infreq() %>% fct_relevel("None", after = Inf)) %>%
  select(-disability_id) %>%
  rename_with(toupper) %>%
  mutate(across(ends_with("SUPPORT"), as.numeric)) %>%
  mutate(across(c(MEDICATION_ADMIN, OCCUP_THERAPY, BEHAVIOR_ASSESSMENT,
                  EXTENDED_TIME, ADAPT_PHYSICAL_ENVIRON), as.numeric)) %>%
  mutate(across(ends_with("MINUTES"), as.numeric)) %>%
  mutate(across(ends_with("PERSON"), as_factor)) %>%
  labelled::set_variable_labels(GRADE = "Grade",
                                CONDUCT = "Conduct grade (0=unsatisfactory; 4=excellent) averaged across classes",
                                EFFORT = "Effort grade (1=insufficient; 2=satisfactory; 3=outstanding) averaged across classes",
                                GPA = "Academic grade (0=F; 4=A) averaged across classes",
                                SECTION_504 = "Has a 504 Plan",
                                ABSENCES_EXCUSED = "Excused Absences",
                                ABSENCES_UNEXCUSED = "Unexcused Absences",
                                FSA_ELA_SCORE = "Standardized English/Language Arts score",
                                FSA_ELA_LEVEL = "English/Language Arts achievement level",
                                FSA_MATH_SCORE = "Standardized Math score",
                                FSA_MATH_LEVEL = "Math achievement level",
                                FSA_SCIENCE_SCORE = "Standardized Science score",
                                FSA_SCIENCE_LEVEL = "Science achievement level",
                                EXCEPTIONALITY_TYPE = "Exceptionality type from administrative data",
                                IEP_CODE = "Has IEP, 504, or EP",
                                PRIMARY_CODE = "Primary diagnosis on IEP, 504, or EP",
                                SECONDARY_CODE = "Secondary diagnosis on IEP, 504, or EP",
                                TERTIARY_CODE = "Tertiary diagnosis on IEP, 504, or EP",
                                PRIMARY_LABEL = "Description of primary diagnosis on IEP, 504, or EP",
                                ADDITIONAL_LABELS = "Description of additional diagnoses on IEP, 504, or EP",
                                PERC_GENED = "Percent of time in general education",
                                PERC_SPED = "Percent of time in special education",
                                RELATED_SPED_SERVICES = "Description of special education services",
                                SPED_PLACEMENT = "Type of special education placement",
                                GIFTED_PLACEMENT = "Type of gifted education placement",
                                GIFTED_FOCUS_AREA = "Focus area of gifted education",
                                MATH_SUPPORT = "Receives math support based on IEP",
                                MATH_MINUTES = "Minutes per month of math support",
                                MATH_PERSON = "Person delivering math support",
                                LA_SUPPORT = "Receives language arts support based on IEP",
                                LA_MINUTES = "Minutes per month of language arts support",
                                LA_PERSON = "Person delivering language arts support",
                                READING_SUPPORT = "Receives reading support based on IEP",
                                READING_MINUTES = "Minutes per month of reading support",
                                READING_PERSON = "Person delivering reading support",
                                LANGUAGE_SUPPORT = "Receives language support based on IEP",
                                LANGUAGE_MINUTES = "Minutes per month of language support",
                                LANGUAGE_PERSON = "Person delivering language support",
                                SPEECH_SUPPORT = "Receives speech support based on IEP",
                                SPEECH_MINUTES = "Minutes per month of speech support",
                                SPEECH_PERSON = "Person delivering speech support",
                                MEDICATION_ADMIN = "Receives medication administration based on IEP",
                                MEDICATION_MINUTES = "Minutes per month of medication administration",
                                MEDICATION_PERSON = "Person administering medication",
                                READING_IN_SCI_SUPPORT = "Receives reading in science support based on IEP",
                                READING_IN_SCI_MINUTES = "Minutes per month of reading in science support",
                                READING_IN_SCI_PERSON = "Person delivering reading in science support",
                                MATH_IN_SCI_SUPPORT = "Receives math in science support based on IEP",
                                MATH_IN_SCI_MINUTES = "Minutes per month of math in science support",
                                MATH_IN_SCI_PERSON = "Person delivering math in science support",
                                READING_IN_SS_SUPPORT = "Receives reading in social studies support based on IEP",
                                READING_IN_SS_MINUTES = "Minutes per month of reading in social studies support",
                                READING_IN_SS_PERSON = "Person delivering reading in social studies support",
                                FINE_MOTOR_SUPPORT = "Receives fine motor (writing) support based on IEP",
                                FINE_MOTOR_MINUTES = "Minutes per month of fine motor (writing) support",
                                FINE_MOTOR_PERSON = "Person delivering fine motor (writing) support",
                                OCCUP_THERAPY = "Receives occupational therapy based on IEP",
                                OCCUP_THERAPY_MINUTES = "Minutes per month of occupational therapy",
                                OCCUP_THERAPY_PERSON = "Person delivering occupational therapy",
                                BEHAVIOR_SKILLS_SUPPORT = "Receives on-task behavior skills support based on IEP",
                                BEHAVIOR_SKILLS_MINUTES = "Minutes per month of on-task behavior skills support",
                                BEHAVIOR_SKILLS_PERSON = "Person delivering on-task behavior skills support",
                                TASK_COMPLETION_SUPPORT = "Receives task completion support based on IEP",
                                TASK_COMPLETION_MINUTES = "Minutes per month of task completion support",
                                TASK_COMPLETION_PERSON = "Person delivering task completion support",
                                INDEPENDENT_FUNCT_SUPPORT = "Receives independent functioning support based on IEP",
                                INDEPENDENT_FUNCT_MINUTES = "Minutes per month of independent functioning support",
                                INDEPENDENT_FUNCT_PERSON = "Person delivering independent functioning support",
                                SOCIAL_SKILLS_SUPPORT = "Receives social skills support based on IEP",
                                SOCIAL_SKILLS_MINUTES = "Minutes per month of social skills support",
                                SOCIAL_SKILLS_PERSON = "Person delivering social skills support",
                                SEL_SUPPORT = "Receives SEL support based on IEP",
                                SEL_MINUTES = "Minutes per month of SEL support",
                                SEL_PERSON = "Person delivering SEL support",
                                SELF_ADVOCACY_SUPPORT = "Receives instruction in self-advocacy based on IEP",
                                SELF_ADVOCACY_MINUTES = "Minutes per month of instruction in self-advocacy",
                                SELF_ADVOCACY_PERSON = "Person delivering instruction in self-advocacy",
                                BEHAVIOR_ASSESSMENT = "Behavior assessment administered based on IEP",
                                BEHAVIOR_ASSESSMENT_MINUTES = "Minutes per month of behavior assessment",
                                BEHAVIOR_ASSESSMENT_PERSON = "Person administering behavior assessment",
                                COMMUNICATION_SUPPORT = "Receives communication support based on IEP",
                                COMMUNICATION_MINUTES = "Minutes per month of communication support",
                                COMMUNICATION_PERSON = "Person delivering communication support",
                                LEARNING_ACTIVITIES_SUPPORT = "Receives assistance with learning activities based on IEP",
                                LEARNING_ACTIVITIES_MINUTES = "Minutes per month of assistance with learning activities",
                                LEARNING_ACTIVITIES_PERSON = "Person assisting with learning activities",
                                ELL_SUPPORT = "Receives English Language Learner support based on IEP",
                                ELL_MINUTES = "Minutes per month of English Language Learner support",
                                ELL_PERSON = "Person delivering English Language Learner support",
                                ORGANIZATIONAL_SUPPORT = "Receives organizational skills support based on IEP",
                                ORGANIZATIONAL_MINUTES = "Minutes per month of organizational skills support",
                                ORGANIZATIONAL_PERSON = "Person delivering organizational skills support",
                                EXTENDED_TIME = "Recieves extended time based on 504 plan",
                                ADAPT_PHYSICAL_ENVIRON = "Recieves adapted physical enviornment based on 504 plan",
                                ADAPT_PRESENTATION = "Recieves adapted presentation based on 504 plan")

# warning  is for extended time for converting "gen ed" to NA, as it should be:
# table(allinone$Extended_Time, useNA = "always")

# ## checking join quality
# iep.no.admin <- allinone %>%
#   anti_join(administrative, by = "STUDENT_ID") # not 728495 who has a self-report, but no teacher report

# ## combine exceptionality type and disability ID?
# table(adiep$EXCEPTIONALITY_TYPE, adiep$disability_id, useNA = "always")
# # only disagreement is for students coded NA/none, so can combine; Exceptionality type is more comprehensive

##################################################################
# Read survey datasets and add summary variables
##################################################################


students  <- read_sav("00 Data/COMBINED T1_T2/BY SURVEY/01 - IES_STUDENTS_3-5_W1-W2_07-20-22.sav")


teacher  <- read_sav("00 Data/COMBINED T1_T2/BY SURVEY/02 - IES_TEACHERS_SELF_W1_W2_07-20-22.sav") %>%
  mutate(WAVES_PRESENT = case_when(!is.na(DATE_W1) & !is.na(DATE_W2) ~ 3,
                                   is.na(DATE_W1) & !is.na(DATE_W2) ~ 2,
                                   !is.na(DATE_W1) & is.na(DATE_W2) ~ 1,
                                   TRUE ~ 0))
teacher$CONDITION_ALL <- do.call(pmax, c(teacher[,names(teacher) %in% c("TREATMENT_CONDITION_W1", "TREATMENT_CONDITION_W2")], list(na.rm=TRUE)))

# table(teacher$WAVES_PRESENT, useNA = "always")

teacher_stud  <- read_sav("00 Data/COMBINED T1_T2/BY SURVEY/03 - IES_TEACHERS_STUD_REPORT_W1_W2_07-20-22.sav") %>%
  mutate(WAVES_PRESENT = case_when(!is.na(DATE_W1) & !is.na(DATE_W2) ~ 3,
                                   is.na(DATE_W1) & !is.na(DATE_W2) ~ 2,
                                   !is.na(DATE_W1) & is.na(DATE_W2) ~ 1,
                                   TRUE ~ 0))
teacher_stud$CONDITION_ALL <- do.call(pmax, c(teacher_stud[,names(teacher_stud) %in% c("TREATMENT_COND_W1", "TREATMENT_COND_W2")], list(na.rm=TRUE)))

# table(teacher_stud$WAVES_PRESENT, useNA = "always")


##################################################################
# Rename variables across datasets to join with each other
##################################################################


colnames(students) <- paste("ST_", colnames(students), sep = "")

colnames(teacher) <- paste("T_", colnames(teacher), sep = "")

colnames(teacher_stud) <- paste("TR_", colnames(teacher_stud), sep = "")


# Change the names of the ID variables to match across datasets
colnames(administrative)[1] <- "STUDENT_ID"

colnames(students)[1] <- "STUDENT_ID"

colnames(teacher)[1] <- "TEACHER_ID"

colnames(teacher_stud)[1] <- "STUDENT_ID"

##################################################################
# Join datasets and streamline variables
##################################################################

## join teacher reports of students and student self-reports
t1 <- full_join(teacher_stud, students, by = c("STUDENT_ID"))
# create single Multiple informants variable - Were there multiple teacher reports of a student in either wave?
t1$MULTIPLE_INFORMANTS <- do.call(pmax, c(t1[,names(t1) %in% c("TR_MULTIPLE_INFORMANTS_W1", "TR_MULTIPLE_INFORMANTS_W2")], list(na.rm=TRUE)))


## determine how to code final Teacher ID variable
# tcheck <- t1 %>%
#   select(STUDENT_ID, ST_TEACHER_ID_ALL, TR_TEACHER_ID) %>% 
#   mutate(TEACHER_ID = case_when(ST_TEACHER_ID_ALL == TR_TEACHER_ID ~ TR_TEACHER_ID,
#                                 is.na(TR_TEACHER_ID) & !is.na(ST_TEACHER_ID_ALL) ~ ST_TEACHER_ID_ALL,
#                                 !is.na(TR_TEACHER_ID) & is.na(ST_TEACHER_ID_ALL) ~ TR_TEACHER_ID,
#                                 is.na(TR_TEACHER_ID) & is.na(ST_TEACHER_ID_ALL) ~ NA_character_,
#                                 # ST_TEACHER_ID_ALL != TR_TEACHER_ID ~ TR_TEACHER_ID,
#                                 TRUE ~ "CHECK"))
# tcheck[tcheck$TEACHER_ID == "CHECK",] %>% View()

## determine how to code final School ID variable
# scheck <- t1 %>%
#   select(STUDENT_ID, contains("SCHOOL"))
# table(scheck$TR_SCHOOL_NO_W1, scheck$TR_SCHOOL_NO_W2, useNA = "always")
# table(scheck$ST_SCHOOL_W1, scheck$ST_SCHOOL_W2, useNA = "always")


## Update dataset with final teacher and school IDs
t2 <- t1 %>%
  mutate(TEACHER_ID = case_when(ST_TEACHER_ID_ALL == TR_TEACHER_ID ~ TR_TEACHER_ID,                   # had student report and a single teacher report
                                is.na(TR_TEACHER_ID) & !is.na(ST_TEACHER_ID_ALL) ~ ST_TEACHER_ID_ALL, # had a student report, but not a teacher report
                                !is.na(TR_TEACHER_ID) & is.na(ST_TEACHER_ID_ALL) ~ TR_TEACHER_ID, # had a teacher report, but not student report
                                is.na(TR_TEACHER_ID) & is.na(ST_TEACHER_ID_ALL) ~ NA_character_,  # for completeness; don't think it applies to anyone
                                ST_TEACHER_ID_ALL != TR_TEACHER_ID ~ TR_TEACHER_ID,               # cases of multiple informants
                                TRUE ~ "CHECK"),
         SCHOOL_ID = case_when(!is.na(ST_SCHOOL_ID_ALL) ~ ST_SCHOOL_ID_ALL,
                               is.na(ST_SCHOOL_ID_ALL) & !is.na(TR_SCHOOL_NO_W2) ~ TR_SCHOOL_NO_W2,
                               is.na(ST_SCHOOL_ID_ALL) & !is.na(TR_SCHOOL_NO_W1) ~ TR_SCHOOL_NO_W1))

## checks for accuracy
# table(t2$TEACHER_ID == "CHECK", useNA = "always")
# table(t2$SCHOOL_ID, useNA = "always")
# str(t2$SCHOOL_ID) # school ID still a factor with appropriate levels?
# table(t2$MULTIPLE_INFORMANTS, useNA = "always")
# table(dial.orig$MULTIPLE_INFORMANTS, useNA = "always")
# str(t2$MULTIPLE_INFORMANTS)

in.s.not.t <- t2$TEACHER_ID[!t2$TEACHER_ID %in% teacher$TEACHER_ID]  # 32 gave student report, not self
in.t.not.s <- teacher$TEACHER_ID[!teacher$TEACHER_ID %in% t2$TEACHER_ID] # 5 gave self, but no student reports
# t4[is.na(t4$STUDENT_ID), ]$TEACHER_ID == in.t.not.s

# anti <- select(allinone, STUDENT_ID, SCHOOL, TEACHER_ID) %>%
#   anti_join(t2 %>%
#               select(STUDENT_ID, TEACHER_ID, SCHOOL_ID) %>%
#               mutate(STUDENT_ID = as.character(STUDENT_ID)), by = "STUDENT_ID")


## Joining administrative data
t3 <- t2 %>%
  mutate(STUDENT_ID = as.character(STUDENT_ID)) %>%
  full_join(select(adiep, -TEACHER_ID), #Teacher info in student data more complete (see "full" below)
            by = "STUDENT_ID") %>%
  mutate(SCHOOL_ID = case_when(!is.na(SCHOOL_ID) ~ SCHOOL_ID, # adds school info for 3 students who only have iep/admin data
                               SCHOOL == "Toni Bilbao" ~ 6,
                               SCHOOL == "Coral Reef" ~ 1)) %>%
  select(-SCHOOL)

# # checking join
# iep.no.survey <- allinone$STUDENT_ID[!allinone$STUDENT_ID %in% as.character(t2$STUDENT_ID)] # 3
# admin.no.survey <- adiep$STUDENT_ID[!adiep$STUDENT_ID %in% as.character(t2$STUDENT_ID)] # 25
# survey.no.admin <- t2$STUDENT_ID[!t2$STUDENT_ID %in% as.numeric(adiep$STUDENT_ID)] # 59
# 
# full <- t2 %>%
#   select(STUDENT_ID, TEACHER_ID, SCHOOL_ID) %>%
#   mutate(STUDENT_ID = as.character(STUDENT_ID)) %>%
#   full_join(select(adiep, STUDENT_ID, SCHOOL, TEACHER_ID), by = "STUDENT_ID") %>%
#   mutate(SCHOOL_ID = case_when(!is.na(SCHOOL_ID) ~ SCHOOL_ID,
#                                SCHOOL == "Toni Bilbao" ~ 6,
#                                SCHOOL == "Coral Reef" ~ 1))

## joining teacher data
t4 <- full_join(t3, teacher, by = c("TEACHER_ID"))

## what's the difference b/t dataset produced here and the originally combined dataset
dial.initial <- read_sav("00 Data/COMBINED T1_T2/OLD/00 - IES_ALL_DATA_W1_W2_07-20-22.sav") %>%
  mutate(TEACHER_ID = stringr::str_remove_all(TEACHER_ID, "\n"))


to.drop <- names(t4)[!names(t4) %in% c(names(dial.initial), names(adiep))] # names in t4 but not in dial.initial, excluding administrative and iep
names(dial.initial)[!names(dial.initial) %in% names(t4)] # names in dial.initial not in t4
# initially the column differences indicate we need to combine columns to create SCHOOL_ID, CONDITION, and MULTIPLE_INFORMANTS columns.
# Also create STUDY_ID and waves present variables

# NOTE: to.drop will be used as a shortcut for removing variables in later code
#"TR_TEACHER_ID","TR_SCHOOL_NO_W1","TR_SCHOOL_NO_W2","TR_MULTIPLE_INFORMANTS_W1", "TR_MULTIPLE_INFORMANTS_W2", "TR_TREATMENT_COND_W1","TR_TREATMENT_COND_W2",     
# "TR_CONDITION_ALL","ST_TEACHER_ID_ALL","ST_SCHOOL_ID_ALL","ST_TREATMENT_COND_ALL","ST_SCHOOL_W1","ST_SCHOOL_W2","ST_TREATMENT_COND_W1"     
# "ST_TREATMENT_COND_W2","T_TREATMENT_CONDITION_W1",  "T_TREATMENT_CONDITION_W2","T_SCHOOL_W1","T_SCHOOL_W2","T_CONDITION_ALL" 

## Checking IDs
# # any teacher switch school?
table(teacher$T_SCHOOL_W1,teacher$T_SCHOOL_W2) # nope
table(is.na(t4$TEACHER_ID)) # the 25 cases with only admin data
table(is.na(t4$SCHOOL_ID)) # the 5 teachers who only gave self report, and 25(-3) students with only admin data
# View(t4[is.na(t4$SCHOOL_ID),]) 

## Remove faulty cases and finalize variables
t5 <- t4 %>%
  filter(!(TEACHER_ID == "MST003" & T_DURATION_SEC_W2 < 60)) %>% # Removing a duplicate teacher self-report (which was then joined to multiple students resulting in rows of NA)
  mutate(STUDY_ID = 1:nrow(.) + 10000, 
         STUDY_ID = labelled(STUDY_ID, label = "Unique Study ID for each observation regardless of survey"),
         CONDITION = case_when(!is.na(TR_CONDITION_ALL) ~ TR_CONDITION_ALL,          # previous checks found no instances of condition switching,
                               !is.na(ST_TREATMENT_COND_ALL) ~ ST_TREATMENT_COND_ALL,# so grabbing condition whenever it is present
                               !is.na(T_CONDITION_ALL) ~ T_CONDITION_ALL),
         STUDENT_ID = labelled(STUDENT_ID, label = "Student ID"),
         TEACHER_ID = labelled(TEACHER_ID, label = "Teacher ID"),
         SCHOOL_ID = case_when(!is.na(SCHOOL_ID) ~ SCHOOL_ID,
                               !is.na(T_SCHOOL_W1) ~ T_SCHOOL_W1,
                               TRUE ~ T_SCHOOL_W2)) %>%
  mutate(across(ends_with("WAVES_PRESENT"), ~ifelse(is.na(.), 0, .))) %>%
  mutate(ST_WAVES_PRESENT = labelled(ST_WAVES_PRESENT, label = "Waves present student survey",
                                     labels = c(`No student report` = 0, `Wave 1 Only` = 1, `Wave 2 Only` = 2, `Wave 1 and Wave 2` = 3)),
         TR_WAVES_PRESENT = labelled(TR_WAVES_PRESENT, label = "Waves present teacher report of students",
                                     labels = c(`No teacher report` = 0, `Wave 1 Only` = 1, `Wave 2 Only` = 2, `Wave 1 and Wave 2` = 3)),
         T_WAVES_PRESENT = labelled(T_WAVES_PRESENT, label = "Waves present teacher survey",
                                    labels = c(`No teacher report` = 0, `Wave 1 Only` = 1, `Wave 2 Only` = 2, `Wave 1 and Wave 2` = 3))) %>%
  labelled::set_value_labels(TR_RACE_HAITIAN_W1 = c(No = 0, Haitian = 1),
                             T_RACE_HAITIAN_W1 = c(No = 0, Haitian = 1),
                             TR_RACE_HISPANIC_W1 = c(No = 0, Hispanic = 1),
                             ST_GENDER_W1 = c(Boy = 0, Girl = 1, `Other (please describe)` = 2),
                             ST_HAITIAN_W1 = c(No = 0, Haitian = 1),
                             ST_HAITIAN_W2 = c(No = 0, Haitian = 1),
                             ST_HISPANIC_W1 = c(No = 0, Hispanic = 1),
                             ST_HISPANIC_W2 = c(No = 0, Hispanic = 1),
                             ST_RACE_ASIAN_W2 = c(No = 0, `Asian American` = 1),
                             ST_RACE_BLACK_W2 = c(No = 0, `Black or African American` = 1),
                             ST_RACE_NATIVE_W2 = c(No = 0, `Native American or Alaska Native` = 1),
                             ST_RACE_OTHER_W2 = c(No = 0, `Other race (please describe)` = 1),
                             ST_RACE_PAC_ISLAND_W2 = c(No = 0, `Hawaiian Native or Pacific Islander` = 1),
                             ST_RACE_WHITE_W2 = c(No = 0, `White or European American` = 1),
                             ST_RACE_DONT_KNOW_W2 = c(No = 0, `I don't know` = 1),) %>%
  select(STUDY_ID, STUDENT_ID, TEACHER_ID, SCHOOL_ID, CONDITION,  # identifying information
         MULTIPLE_INFORMANTS, ends_with("WAVES_PRESENT"),  # survey participation information
         GRADE, CONDUCT:ADAPT_PRESENTATION, # administrative and IEP information
         starts_with("TR_"), starts_with("ST_"), starts_with("T_"), everything(), # survey responses
         -all_of(to.drop),-TR_EXCEPTIONALITY_TYPE, -ST_EXCEPTIONALITY_TYPE_W1, -ST_EXCEPTIONALITY_TYPE_W2) # these are redundant and inferior to EXCEPTIONALITY_TYPE

## double check columns
names(t5)[!names(t5) %in% names(dial.initial)] # names in t5 but not in dial.initial (i.e., all the admin/iep variables)
names(dial.initial)[!names(dial.initial) %in% names(t5)] # names in dial.initial not in t5 (redundant exceptionality type variables)


# ## Checking if variable and value labels are correct
# source("Scripts/IG2_PnF.R") # just need codebook function
# Create_Codebook(t5, export_type = "excel", export_name = "00 - IES_ALL_DATA_W1_W2_X-X-23_Codebook.xlsx" )

# ## Checking if item class is correct
# # In SPSS, NAs for character variables are given values of "" rather than being treated as missing;
# # therefore, we want most variables to be numeric or factor
# item.types <- purrr::map_dfr(.x = names(t5),
#                              ~data.frame(Variable = .x,
#                                          Classes = paste0(class(t5[[.x]]), collapse = ", ")))


## Need to check in t5 if below issues are resolved or what other issues might have occurred

# - Is student 728495 in the class for teacher ST009? Student has self-report data in t1_2, but no teacher; in dial.initial there is the TeacherID and nothing else even though ST009 has responses
#   t5[t5$STUDENT_ID == 728495, ] %>% View() # Yes
# - In the combined data file (00 - IES_ALL_DATA_W1_W2_07-20-22.sav) teacher CRT999 is linked to 10 students. For 2 students there are teacher-report of student and teacher self-report data in Wave 2;
# however, teacher CRT999 is not present in the initial teacher file (02 - IES_TEACHERS_SELF_W1_W2_07-20-22.sav") or teacher-report of students file (01 - IES_STUDENTS_3-5_W1-W2_07-20-22.sav);
# So where did these data come from for the 2 students? Additionally, teacher CRT999 is present in the initial student file () for the same 10 students as
# the combined data file, but this initial file suggests these students had different teachers based on the ST_TEACHER_NOT_IN_ROSTER_W1 column. Who is teacher CRT999?
# - In the combined data file, the teacher data for teacher CRT010 is not joined with their students 7488226 and 732540 (2 of 12 students), and teacher TBT011 with students 702037 and 769592 (2 of 17 students);
#   however, the data is joined in the dataset after running the code 0-Code_to_merge_all_data.R. So something happens between the dataset output 
#   from 0-Code_to_merge_all_data.R to 00 - IES_ALL_DATA_W1_W2_07-20-22.sav (which seems to have been updated after the _v2 file). Do you have R or SPSS syntax that documents those the changes?

# 869259 - CRT012
# 690453 - CRT012 
# Any teacher ID with **999 was assigned to students that we could not identify a teacher. We did not create the IDs for the extra teachers because they were not part of the study.
# Thus, these IDs should only appear in the 3-5th grade student self-reports and should not have teacher reports of any kind. For instance, CRT999 represents multiple teachers at school CRT.


# t5[t5$STUDENT_ID == 690453, ] %>% View()
# t5[t5$TEACHER_ID == "CRT999", ] %>% View()
# t3[t3$TEACHER_ID == "CRT999", ] %>%
#   select(STUDENT_ID, TEACHER_ID, all_of(to.drop)) %>% View()
# t5[t5$TEACHER_ID == "TBT011", ] %>%
#   select(STUDENT_ID, TEACHER_ID, starts_with("T_")) %>% View()



##################################################################
# Save dataset
##################################################################

## IMPORTANT: This dataset is NOT meant for analysis. It contains all legitimate
##            teacher report of students, student self-reports, teacher self-reports, administrative, and IEP data
##            combined into a single dataset.
##            - CONSEQUENTLY, there are multiple rows for the same student if more than one teacher
##              gave a student report for that student. The R script file "1-Psychometric_W1W2.R"
##              randomly selects a report for these students.
##            - There is also administrative data for students with no survey reports of any kind.
##              These students are removed in the same script file to create the analytic dataset
##              with one row per student.
##            - Additionally, column names for scale variables get changed after the psychometric
##              investigations are complete.
##            LASTLY, the R script file "2-Scoring.R" calculates scale scores and codes demographic
##            variables across waves. These variables are added to the dataset to create the final
##            analytic dataset. See "2-Scoring.R" for current naming convention.


## NOTE: UPDATE FILE DATA BEFORE EXPORTING ##
# write_sav(t5, "00 Data/COMBINED T1_T2/00 - IES_ALL_DATA_W1_W2_X-X-23.sav")

