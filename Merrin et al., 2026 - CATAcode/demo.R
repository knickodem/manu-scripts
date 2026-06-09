########################################
#                                      #
#              CATAcode:               #
#   A Principled Approach for Coding   #
#      Check-All-That-Apply Items      #
#                                      #
########################################

#install.packages("CATAcode")
library(CATAcode)
library(dplyr)
library(gtsummary)
library(flextable)

# ----- Custom function for formatting output tables  -----

format_flex <- function(df, digits = 2, width = NULL){
  
  if(is.data.frame(df)){
    numericcols <- which(unlist(lapply(df, is.numeric)))
    ftab <- flextable(df)
    ftab <- colformat_double(ftab, j = numericcols, digits = digits)
  } else{
    ftab <- df
  }
  
  ftab <- flextable::font(ftab, fontname = "Times New Roman", part = "all")
  ftab <- flextable::padding(ftab, padding = 0, part = "all")
  ftab <- autofit(ftab)
  if(!is.null(width)){
    ftab <- fit_to_width(ftab, width)
  }
  
  return(ftab)
}
##################################################################

# ----  Importing Data  ----

## longitudinal - included in the CATAcode package
data("sources_race")
longitudinal <- sources_race

## cross-sectional
cross <- readRDS("cross_race.rds")

##################################################################

# ----    Raw Frequencies   ----

## Table 1 - cross-sectional data
cross_raw <- cross |>
  # Alphabetizing groups
  select(Another_Race, Asian, Black, Latin_American, Multiracial,
         Native_American, Pacific_Islander, White) |> 
  rename_with(.fn = ~gsub("_", " ", .)) |> 
  mutate(across(.cols = everything(), ~tidyr::replace_na(., 0))) |>
  tbl_summary(missing = "no", digits = everything() ~ 0) |>
  as_flex_table() |>
  format_flex()
# Note: Columns split and "Total" row added manually

## Table 2 - Longitudinal data
longitudinal_raw <- longitudinal |>
  select(Wave, Asian, Black, Hispanic, Multiracial,
         Native_American, Pacific_Islander, White) |>
  rename_with(.fn = ~gsub("_", " ", .)) |> 
  mutate(across(.cols = Asian:White, ~tidyr::replace_na(., 0))) |>
  tbl_summary(by = "Wave", digits = everything() ~ 0) |>
  as_flex_table() |>
  format_flex()
# Note: Total row added manually

length(unique(longitudinal$ID)) # 6442

##################################################################

#  -----   Demonstration  -----

####  Cross-sectional   ####

## preparing
cross_prep <- cata_prep(cross, id = ID, cols = Black:Another_Race)

## all
# Note: Another Race is shortened to Other to reduce width of Table 3 below
cross_all <- cross_prep |>
  mutate(Category = if_else(Category == "Another_Race", "Other", Category)) |>
  cata_code(id = ID, categ = Category,
            resp = Response, approach = "all", endorse = 1,
            new.name = "Race_Ethnicity")

names(table(cross_all$Race_Ethnicity)) # 85

## Table 3
cross_all_summary <- cross_all |>
  filter(!is.na(Race_Ethnicity)) |>
  tbl_summary(
    include = "Race_Ethnicity",
    missing = "no",
    statistic = everything() ~ "{n}") |>
  as_flex_table() |>
  format_flex()
# Note: Manually replaced values < 5 with "< 5"

## coding
# Note. new.name is different than in the manuscript to facilitate
#       creation of Table 4
cross_multiple <- cata_code(data = cross_prep, id = ID,
                            categ = Category, resp = Response,
                            approach = "multiple", endorse = 1,
                            new.name = "Multiple", multi.name = "Multiracial")
cross_priority <- cata_code(data = cross_prep, id = ID,
                            categ = Category, resp = Response,
                            approach = "priority", endorse = 1,
                            priority = c("Native_American", "Pacific_Islander", "Latin_American"),
                            new.name = "Priority", multi.name = "Multiracial")

## Table 4 - comparing results and preparing output
cross_comp <- full_join(cross_multiple, cross_priority, by = "ID") |> 
  tidyr::pivot_longer(cols = -ID) |>
  tbl_summary(include = "value", by = "name", digits = everything() ~ 0) |>
  as_flex_table() |>
  format_flex()
# Note. Manually replaced percentages < 1 with "<1%"
####################################################

####   Longitudinal  ####

## prep
longitudinal_prep <- cata_prep(data = longitudinal, id = ID,
                       cols = c(Asian, Black:White), time = Wave)

## all
longitudinal_all <- cata_code(longitudinal_prep, id = ID,
                      categ = Category, resp = Response,
                      approach = "all", new.name = "Race_Ethnicity")

longitudinal_waves <- longitudinal_all |>
  filter(!is.na(Race_Ethnicity)) |>
  count(Wave, Race_Ethnicity)
purrr::map(.x = 1:4, ~longitudinal_waves |> filter(Wave == .x) |> nrow())
# Number of combinations each wave, 69, 62, 62, 57
length(names(table(longitudinal_all$Race_Ethnicity))) # 88

## counts
long_counts <- cata_code(longitudinal_prep, id = ID,
                         categ = Category, resp = Response,
                         approach = "counts", endorse = 1)
count_ID_summary <- long_counts |> count(ID)
table(count_ID_summary$n)
count_race_summary <- long_counts |>
  ungroup() |> 
  count(Category)


## coding
# Note. new.name is different than in the manuscript to facilitate
#       creation of Table 5
long_multiple <- cata_code(data = longitudinal_prep, id = ID,
                           categ = Category, resp = Response,
                           approach = "multiple", endorse = 1,
                           new.name = "Multiple", multi.name = "Multiracial")
long_priority <- cata_code(data = longitudinal_prep, id = ID,
                           categ = Category, resp = Response,
                           approach = "priority", endorse = 1, time = Wave,
                           priority = c("Native_American", "Pacific_Islander"),
                           new.name = "Priority", multi.name = "Multiracial")
long_mode <- cata_code(data = longitudinal_prep, id = ID,
                       categ = Category, resp = Response,
                       approach = "mode", endorse = 1, time = Wave,
                       new.name = "Mode", multi.name = "Multiracial")

## Table 5 - comparing results and preparing output
longitudinal_comp <- full_join(long_multiple, long_priority, by = "ID") |>
  full_join(long_mode, by = "ID") |>
  tidyr::pivot_longer(cols = -ID) |>
  mutate(name = factor(name, levels = c("Multiple", "Priority", "Mode"))) |>
  tbl_summary(include = "value", by = "name", digits = everything() ~ 0) |>
  as_flex_table() |>
  format_flex()
# Note. Manually replaced percentages < 1 with "<1%"

# mode with priority
long_mode_priority = cata_code(data = longitudinal_prep, id = ID, categ = Category, resp = Response, approach = "mode", endorse = 1, time = Wave,
                               new.name = "Race_Ethnicity", multi.name = "Multiracial",
                               priority = c("Black", "Native_American"))

##############################################

# exporting tables
save_as_docx(values = list(`Table 1` = cross_raw, `Table 2` = longitudinal_raw,
                           `Table 3` = cross_all_summary,
                           `Table 4` = cross_comp, `Table 5` = longitudinal_comp),
             path = "Manuscript_tables.docx")
