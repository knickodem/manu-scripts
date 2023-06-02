########################################
#                                      #
#         Sources of Strength          #
#    Implementation Data Preparation   #
#                                      #
########################################

# Further implementation information: https://drive.google.com/drive/folders/1OF6e76OTnhiCaH2UPrRTzKPzxsnjtqPN

# Implementation logs for initial trainings:
# Engagement - Were Peer Leaders engaged and participatory? SD (1) to SA (5)
# Involvement - Were Adult Advisors involved and connected?


## Loading packages and functions
source("Scripts/PnF.R")

# -------    Importing Student-level Data ----------------

load("Output/SoS_Data.RData") # loaded for ds2 (ds3 just has factor scores which we don't need here)
dsclean <- read_sav("SoS Data and Codes/w1-w4 SoS_Cleaned_05-17-22.sav") # needed to add back in networking variables


## Selecting variables for analysis & keeping only SoS students
dstxwide <- ds2 %>%
  mutate(schoolid = zap_labels(School),
         school_name = as_factor(School)) %>%
  filter(Tx == 1) %>%                                              # keeping only SoS students
  select(StudentID, school_name, schoolid, Tx,                  # Identifiers
         Grade, one_of(DemoVars), starts_with("AGE_"),             # Demographics; GRADE_W1 = RGRADE_R1 (using Kelly's Grade variable instead)
         HasASurvey_W1:HasASurvey_W4,                              # Has Survey - Created by Kelly based on whether missing on Age, Friend1, or scale scores for general well-being, intent help others & help seeking general belief
         starts_with("Active_Exposure"), starts_with("Passive_Exposure"),
         -ends_with("sum"), -ends_with("Avg")) %>%
  rename_with(.cols = contains("_Score"), .fn = ~str_remove(., "_Score")) %>%
  left_join(dsclean %>%
              select(StudentID, starts_with("PLInd"),
                     starts_with("TotAdultNoms_"),
                     PpPLY1_W1:nindegPLCumulW4_W4, -ends_with("_sum")),
            by = "StudentID")


## long format & calculating calculating passive and active exposure scores
dstxlong <- dstxwide %>%
  tidyr::gather("Variable", "Value", matches("_W[0-9]$")) %>%
  mutate(wave = stringr::str_sub(Variable, start = -1) %>% as.numeric(),
         Variable = stringr::str_remove(Variable, "_W[0-9]|_w[0-9]"),
         Variable = stringr::str_remove(Variable, "W[0-9]")) %>% # the Cumul variables have the wave listed twice
  tidyr::spread(Variable, Value) %>%
  filter(!is.na(wave)) %>%
  mutate(across(AGE:TotPLY2, .fns = as.numeric))

## exposure for all students and for non-PL students by school and wave
schoolexp <- dstxlong %>%
  filter(wave != 1) %>%
  group_by(wave, school_name) %>%
  skimr::skim(Active_Exposure, Passive_Exposure) %>%
  bind_rows(dstxlong %>%
              filter(wave != 1 & PLIndEver == 0) %>%
              rename(nonPL_Active_Exposure = Active_Exposure, nonPL_Passive_Exposure = Passive_Exposure) %>%
              group_by(wave, school_name) %>%
              skimr::skim(nonPL_Active_Exposure, nonPL_Passive_Exposure)) %>%
  bind_rows(dstxlong %>%
              filter(wave != 1 & PLIndEver == 1) %>%
              rename(PL_Active_Exposure = Active_Exposure, PL_Passive_Exposure = Passive_Exposure) %>%
              group_by(wave, school_name) %>%
              skimr::skim(PL_Active_Exposure, PL_Passive_Exposure))
  

# plotting exposure distribution
# Not sure why the plot implies passive scores > 6 when schoolexp shows there is not
schoolexp.dist <- dstxlong %>%
  select(school_name, wave, Active_Exposure, Passive_Exposure) %>%
  gather(Exposure, Score, Active_Exposure, Passive_Exposure) %>%
  mutate(wave = as_factor(wave)) %>%
  # filter(Exposure == "Passive_Exposure") %>%
  ggplot(aes(x = Score, y = wave, fill = wave)) +
  ggridges::geom_density_ridges() +
  scale_x_continuous(limits = c(0, 8), breaks = seq(0, 8, 2)) +
  scale_fill_brewer(palette = "Set2") +
  ggridges::theme_ridges() + 
  theme(legend.position = "none") +
  facet_grid(vars(Exposure), vars(school_name))

# exposure by PL status
# Note. Ns reported in table are based on PLIndEver, not actual available information
# For instance, table(is.na(dstxwide$Active_Exposure_W2), dstxwide$PLIndEver)


plexp <- dstxwide %>%
  mutate(PLIndEver = as_factor(PLIndEver)) %>%
  select(PLIndEver, Active_Exposure_W2:Passive_Exposure_W4, -Passive_Exposure_W1) %>%
  tbl_summary(by = c("PLIndEver"),
            type = list(all_categorical() ~ "continuous"),
            statistic = list(all_continuous() ~ "{mean} ({sd})"),
            missing = "no") %>% 
  add_p(test = list(all_continuous() ~ "kruskal.test")) %>% # kruskal is the non-parametric equivalent to anova which is more suitable given the distributions
  add_overall() %>%
  # add_q(method = "fdr") %>%  # false discovery rate is the same as Benjamini & Hochberg; only corrects for the 6 tests within each table which doesn't make much difference
  as_flex_table()

## kruskal-wallis - if we need test statistic and df
pl.exp.names <- select(dstxwide, starts_with("Active_"), starts_with("Passive"), -ends_with("W1")) %>% names()
pl.exp.test <- map(pl.exp.names, ~kruskal.test(dstxwide[[.x]], dstxwide$PLIndEver)) %>%
  map_dfr(.x = ., ~data.frame(statistic = .x$statistic,
                              df = .x$parameter,
                              p = .x$p.value)) %>%
  cbind(data.frame(Exposure = pl.exp.names), .)
rownames(pl.exp.test) <- NULL

######################################################################


# -------    Importing Implementation Data ----------------

#### R compatible column names for imported datasets ####
# Naming convention for imp00av take the format who_what_thing_stat_wave
# who - Peer Leaders (pl), Adult Advisor (aa), Students (std)
# what - Initial training (it), Meetings (meet), Campaign (camp)
# thing - engagement (eng), attendance (att), involvement (inv), activities (act), modules (mod)
# stat - duration (dur), percent (perc), proportion (prop), count (n though this is sometimes omitted or averaged [avg])
clean.list <- map(.x = c("Original Dataset", "Meeting Details", "Campaign Details", "AA Training", "PL Training"),
                  ~readxl::read_excel("../00 ALL IMPLEMENTATION DATA/Documentation/Implementation Variables to R Names.xlsx", sheet = .x)) %>%
  set_names(c("Original Dataset", "Meetings", "Campaigns", "AA Training", "PL Training"))

## creating vectors to use in rename function
clean.vect <- map(.x = clean.list, function(x){
  cn <- x$original_name
  names(cn) <- x$short_name
  return(cn)})

#### Original combined/cleaned dataset ####
## Any differences between the 2 quantitative data files in 00 ALL IMPLEMENTATION DATA and 1 in 02 Implementation Data W1-W4 cleaned?
## Answer: Use either imp00 data - confirmed by Alberto; extra files have been deleted from my computer to help with organization

# imp02folder <- read_sav("../02 Implementation Data W1-W4 cleaned/Quantitative_Implementation_SoS_ALL_WAVES_2-14-2020.sav")
# imp00quant <- read_sav("../00 ALL IMPLEMENTATION DATA/Quantitative Dataset/Quantitative_Implementation_SoS_ALL_WAVES_2-14-2020.sav")
imp00av <- read_sav("../00 ALL IMPLEMENTATION DATA/IMPLEMENTATION DATA CLEANED/Quantitative_Implementation_SoS_ALL_WAVES_2-14-2020.sav")

# all.equal(imp00quant, imp00av) # TRUE
# names(imp02folder)[!names(imp02folder) %in% names(imp00av)] # none
# names(imp00av)[!names(imp00av) %in% names(imp02folder)] # SchoolID and % PL from school variables

## Creating data dictionary
# dictionary <- labelled::generate_dictionary(imp00av, details = "basic") %>%
#   labelled::convert_list_columns_to_character() %>%
#   mutate(value_labels = stringr::str_remove_all(value_labels, "\\[") %>%
#            stringr::str_replace_all(., "\\]", " ="))
# openxlsx::write.xlsx(x = dictionary, file = paste0("Quantitative Implementation Data Dictionary_Temp.xlsx"), asTable = FALSE)

## long format with one row per school per wave
imp00wave <- imp00av %>%
  rename(!!!clean.vect$`Original Dataset`) %>%
  gather("Variable", "Value", -c(school_n, school_name, schoolid)) %>%
  mutate(wave = stringr::str_sub(Variable, start = -1) %>% as.numeric(),
         Variable = stringr::str_remove(Variable, "_W[0-9]|_w[0-9]")) %>%
  tidyr::spread(Variable, Value)


#### Meeting Details ###
## gathering meeting details sheet from each wave into a single dataframe
meetimp.orig <- map_dfr(.x = 1:4, ~readxl::read_excel("../00 ALL IMPLEMENTATION DATA/Implementation data sources/Implementation data master sheet.xlsx", 
                                sheet = paste0("Meetings_details_W", .x), na = "NA", skip = 1, # skip needed b/c we are adding the column names directly
                                col_names = clean.list$Meetings$short_name) %>%
                   mutate(wave = .x)) %>%
  mutate(year = ifelse(wave %in% 1:2, 1, 2))

## summarizing data by year
meetimp <- meetimp.orig %>%
  group_by(school_name, year) %>%
  summarize(meet_n = sum(!is.na(meeting_number)),
            pl_meet_dur_avg = mean(pl_meet_dur, na.rm = TRUE),
            pl_meet_att_avg = mean(pl_meet_att, na.rm = TRUE),
            pl_meet_act_avg = mean(pl_meet_act, na.rm = TRUE),
            pl_meet_maxn = max(pl_meet_att, na.rm = TRUE)) # max PL based on meeting attendance

#### Campaign Details ###
## gathering campaign details sheet from each wave into a single dataframe
campimp.orig <- map_dfr(.x = 1:4, ~readxl::read_excel("../00 ALL IMPLEMENTATION DATA/Implementation data sources/Implementation data master sheet.xlsx", 
                                                   sheet = paste0("Campaigns_details_W", .x), na = "NA", skip = 1, # skip needed b/c we are adding the column names directly
                                                   col_names = clean.list$Campaigns$short_name) %>%
                       mutate(wave = .x)) %>%
  mutate(year = ifelse(wave %in% 1:2, 1, 2)) %>%
  filter(!is.na(school_name))  # last row of pl_maxn reads"**MAXIMUM NUMBER OF PL WHO ATTENDED PL MEETINGS"

## summarizing data by year
campimp <- campimp.orig %>%
  group_by(school_name, year) %>%
  summarize(school_n = mean(school_n, na.rm = TRUE), # school_n in all datasets is based on W1
            camp_n = sum(!is.na(campaign_number)),
            camp_act_avg = mean(camp_act, na.rm = TRUE),
            pl_camp_inv_avg = mean(pl_camp_inv_n, na.rm = TRUE),
            std_reached_avg = mean(std_reached_n, na.rm = TRUE),
            pl_camp_maxn = max(pl_camp_inv_n, na.rm = TRUE)) # max PL based on campaign involvement


#### Adult Advisor Initial Training ####
## gathering sheet from each wave into a single dataframe
aaitimp.orig <- map2_dfr(.x = c(1,3), .y = list(c(clean.list$`AA Training`$short_name, "drop"), clean.list$`AA Training`$short_name), 
                       ~readxl::read_excel("../00 ALL IMPLEMENTATION DATA/Implementation data sources/Implementation data master sheet.xlsx", 
                                           sheet = paste0("W", .x, "-AA_training"), na = "NA", skip = 1, # skip needed b/c we are adding the column names directly
                                           col_names = .y) %>% # wave 1 has an extra column we don't need
                         mutate(wave = .x)) %>%
  mutate(year = ifelse(wave == 1, 1, 2))

## Data already summarized by year, so just cleaning
aaitimp <- aaitimp.orig %>%
  select(year, school_name, aa_it_dur:aa_n)

#### Peer Leader Initial Training ####
plitimp.orig <- map_dfr(.x = c(1,3), ~readxl::read_excel("../00 ALL IMPLEMENTATION DATA/Implementation data sources/Implementation data master sheet.xlsx", 
                                                     sheet = paste0("W", .x, "-initial_PL_training"), na = "NA", skip = 1, # skip needed b/c we are adding the column names directly
                                                     col_names = clean.list$`PL Training`$short_name) %>%
                         mutate(wave = .x)) %>%
  mutate(year = ifelse(wave == 1, 1, 2)) %>%
  filter(!is.na(school_name)) #last row of pl_it_dur has "**From all meetings of the semester, the max attendance to..."

## Data already summarized by year, so just cleaning
plitimp <- plitimp.orig %>%
  select(year, school_name, pl_it_dur:pl_it_n)


#### Combining datasets ####
## all implemenation by year variables
impyear <- full_join(aaitimp, plitimp, by = c("year", "school_name")) %>%
  full_join(meetimp, by = c("year", "school_name")) %>%
  full_join(campimp, by = c("year", "school_name")) %>%
  # mutate(pl_maxn = case_when(is.na(pl_meet_maxn) ~ pl_camp_maxn,
  #                            is.na(pl_camp_maxn) ~ pl_meet_maxn,
  #                            pl_meet_maxn >= pl_camp_maxn ~ pl_meet_maxn,
  #                            TRUE ~ pl_camp_maxn),
  mutate(aa_pl_ratio = pl_meet_maxn / aa_n,
         pl_active_prop = pl_meet_maxn / pl_it_n,
         pl_trained_perc = pl_it_n / school_n,
         pl_perc = pl_meet_maxn / school_n,
         pl_meet_att_perc = pl_meet_att_avg / pl_meet_maxn,
         pl_camp_inv_perc = pl_camp_inv_avg / pl_meet_maxn,
         std_reached_perc = std_reached_avg / school_n,
         school_name = str_remove_all(school_name, " High| School| HS| Jr\\. Sr\\.| Academy") %>%
           recode(`CEC Early College` = "CEC Denver", `Colorado Early Colleges-Parker` = "CEC Parker",
                  `John F Kennedy` = "JFK", `Vail Ski and Snowboard` = "Vail Ski & Snowboard"), # names need to match schoolexp
         school_n100 = school_n / 100,
         aa_pl_ratio_z = scale(aa_pl_ratio)[,1]) %>%
  mutate(across(.cols = c(pl_active_prop, pl_trained_perc, pl_perc, pl_meet_att_perc, pl_camp_inv_perc, std_reached_perc), ~.*100))

table(impyear$pl_meet_maxn, impyear$pl_camp_maxn, useNA = "always")

## comparing by wave and by year datasets
# only variables not included in impyear are duration, but they don't have any variability anyway (see imp00wave)
names(impyear)[!names(impyear) %in% names(imp00wave)]
names(imp00wave)[!names(imp00wave) %in% names(impyear)]


## joining exposure
# by wave - in long format
impexpwave <- imp00wave %>%
  left_join(schoolexp %>%
              mutate(exposure = tolower(skim_variable) %>%
                       str_replace(., "exposure", "exp") %>% paste0("std_", .),
                     wave = wave - 1) %>%    # Thus w2 exposure merged with w1 imp and w4 exp with w3 imp
              select(exposure, wave, school_name, numeric.mean) %>%
              spread(exposure, numeric.mean), by = c("school_name", "wave"))

# by year - long format which only includes school-level exposure in waves 2 and 4
impexpyear.long <- impyear %>%
  left_join(schoolexp %>%
              filter(wave %in% c(2, 4)) %>%
              mutate(exposure = tolower(skim_variable) %>%
                       str_replace(., "exposure", "exp") %>% paste0("std_", .),
                     year = case_when(wave == 2 ~ 1,
                                      wave == 4 ~ 2)) %>%
              select(exposure, year, school_name, numeric.mean) %>%
              spread(exposure, numeric.mean), by = c("school_name", "year"))


# by year - wide format all waves of school-level exposure
impexpyear.wide <- impyear %>%
  gather(Variable, Value, -c(year, school_name, school_n, school_n100)) %>%
  mutate(Variable = paste0(Variable, "_y", year)) %>%
  select(-year) %>%
  spread(Variable, Value) %>%
  left_join(schoolexp %>%
              mutate(exposure = tolower(skim_variable) %>%
                       str_replace(., "exposure", "exp") %>%
                       paste0("std_", ., "_w", wave)) %>%
              select(exposure, school_name, numeric.mean) %>%
              spread(exposure, numeric.mean), by = c("school_name"))

# ------ Saving datasets --------------
save(dstxlong, dstxwide,
     schoolexp, schoolexp.dist, plexp, pl.exp.test,
     impexpwave, impexpyear.long, impexpyear.wide,
     file = "Output/Implmentation_Exposure_Data.RData")



###########################################################


#############################################################################

