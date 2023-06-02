##########################################
#                                        #
#           Sources of Strength          #
#    Implementation & Exposure By Year   #
#                                        #
##########################################

## Loading packages and functions
source("Scripts/PnF.R")
empty_cols <- function(x) x[, colSums(is.na(x)) != nrow(x)]

## Importing data
load("Output/Implmentation_Exposure_Data.RData")

schdemos <- readxl::read_excel("SoS Data and Codes/RSVP2 School Stats.xlsx", sheet = "RReady") %>%
  mutate(Rural = ifelse(Rural_Urban == "Urban", 0, 1)) %>%
  select(school_name = School, Rural)

impexpyear.long <- impexpyear.long %>%
  mutate(pl_it_dur = as.numeric(pl_it_dur)) %>% # not sure why it was not numeric
  left_join(schdemos, by = "school_name")

impexpyear.wide <- impexpyear.wide %>%
  mutate(across(.cols = -school_name, as.numeric)) %>%
  left_join(schdemos, by = "school_name")


# -------   Implementation Trends   ----------------

####  Descriptive Statistics  #####
## All variables
imp.descrips <- impexpyear.long %>%
  group_by(year) %>%
  skimr::skim(aa_it_dur:std_pl_passive_exp) %>%
  filter(!is.na(numeric.mean)) %>%
  rename_with(~str_remove(., "numeric."), starts_with("numeric.")) %>%
  select(-p25, -p75) %>%
  mutate(across(complete_rate:p100, ~round(., 2)))

## Creating subsets of summary statistics and data to help
## identify variables with variation and correlated with school-level exposure
## These are deemed the "potentially important" variables

## Adult Advisors
aa.descrips <- imp.descrips %>%
  filter(str_detect(skim_variable, "^aa|exp$")) %>%
  select(-skim_type,-complete_rate)

aa.data <- impexpyear.long %>%
  select(year, school_n, school_name, starts_with("aa_"), ends_with("exp"))

aa.cors <- map(c(1,2), ~aa.data[aa.data$year == .x,] %>%
                 select(-c(school_name, year)) %>%
                 cor(., use = "pairwise.complete.obs"))

lapply(aa.cors, round, 2)

# potentially important: aa_n (or % if we can calculate it), aa_pl_prop, aa_it_mod (only reasonable IT variable, but not sure this one is even useful)

# gathering into one object for saving
aa.assoc <- list(Descriptives = aa.descrips,
                 Data = aa.data,
                 Correlations = aa.cors)

## Peer Leaders
pl.descrips <- imp.descrips %>%
  filter(str_detect(skim_variable, "^pl|exp$") | skim_variable == "meet_n") %>%
  select(-skim_type,-complete_rate)

pl.data <- impexpyear.long %>%
  select(year, school_n, school_name, starts_with("pl_"), meet_n, ends_with("exp"))

pl.cors <- map(c(1,2), ~pl.data[pl.data$year == .x,] %>%
                 select(-c(school_name, year)) %>%
                 cor(., use = "pairwise.complete.obs"))

lapply(pl.cors, round, 2)

# potentially important: pl_perc, pl_active_prop, pl_maxn,
#                        pl_meet_n, pl_meet_act_avg, pl_meet_att_perc, pl_camp_inv_perc

# gathering into one object for saving
pl.assoc <- list(Descriptives = pl.descrips,
                 Data = pl.data,
                 Correlations = pl.cors)

## Campaigns/Students
# std_reached is a rough estimate from adult advisors and probably not reliable; the exposure measure will be a better indicator anyway

cs.descrips <- imp.descrips %>%
  filter(str_detect(skim_variable, "^camp|^std")) %>%
  select(-skim_type,-complete_rate)

cs.data <- impexpyear.long %>%
  select(year, school_n, school_name, matches("^camp|^std"))

cs.cors <- map(c(1,2), ~cs.data[cs.data$year == .x,] %>%
                 select(-c(school_name, year)) %>%
                 cor(., use = "pairwise.complete.obs"))

lapply(cs.cors, round, 2)

# potentially important: camp_n, camp_act_avg

# gathering into one object for saving
cs.assoc <- list(Descriptives = cs.descrips,
                 Data = cs.data,
                 Correlations = cs.cors)

## Subset of more critical variables - these are subject to change
crit.vars <- impexpyear.long %>%
  select(aa_n, aa_pl_ratio, aa_it_mod,
         pl_perc, pl_active_prop, pl_meet_maxn,
         meet_n, pl_meet_act_avg, pl_meet_att_perc, pl_camp_inv_perc,
         camp_n, camp_act_avg) %>% names()

####  Correlations #####
# If the implementation variables are all positively correlated, we can attempt to create an implementation latent variable
# Want to identify which schools were higher or lower on implementation

## By year
impcorrs <- map(c(1, 2), ~impexpyear.long[impexpyear.long$year == .x, c("school_n", crit.vars, "std_active_exp", "std_passive_exp",
                                                              "std_nonpl_active_exp", "std_nonpl_passive_exp", "std_pl_active_exp", "std_pl_passive_exp")] %>%
                  empty_cols() %>%
                  cor(., use = "pairwise.complete.obs"))
lapply(impcorrs, round, 2)

# Visual options
impcorr.plots <- map(impcorrs, ~ggcorrplot::ggcorrplot(.x, type = "lower", 
                                                       hc.order = FALSE, lab = TRUE, colors = c("#E46726", "white", "#6D9EC1")))

## For supplemental materials
# uses only variables that will be featured in Table 5
impcorrs.sm <- map(c(1, 2), ~impexpyear.long %>%
                     filter(year == .x) %>%
                     select(`AA N` = aa_n, `Percent PL` = pl_perc, `PL-AA Ratio` = aa_pl_ratio, `Meeting N` = meet_n,
                            `Avg PL Meeting Att` = pl_meet_att_perc, `Campaign N` = camp_n, `School Size` = school_n, Rural,
                            `Participatory Exposure` = std_active_exp, `Non-participatory Exposure` = std_passive_exp) %>%
                     empty_cols() %>% cor(., use = "pairwise.complete.obs"))

impcorr.sm.plots <- map(impcorrs.sm, ~ggcorrplot::ggcorrplot(.x, type = "lower", 
                                                          hc.order = FALSE, lab = TRUE, colors = c("#E46726", "white", "#6D9EC1")))




#### Principal Components Analysis ####
imp.pca <- map(.x = c(1,2), ~prcomp(as.formula(paste("~", paste(crit.vars, collapse = " + "))),
                                    data = impexpyear.long[impexpyear.long$year == .x,], scale. = TRUE))

library(factoextra)
lapply(imp.pca, fviz_eig)
lapply(imp.pca, function(x){fviz_pca_var(x,
                                         col.var = "contrib", # Color by contributions to the PC
                                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                         repel = TRUE     # Avoid text overlapping
)})
fviz_pca_ind(imp.pca[[1]],
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
lapply(imp.pca, function(x) round(get_eigenvalue(x), 2))
lapply(imp.pca, screeplot, type = "lines")

## Conclusion:
# Scree plots (eigenvalues) don't indicate a clear cutoff for components, particularly not 1 or 2

#######################################################

## Rank order 11 schools on each variable at each wave and come up with non-parametric composite ranking? Color code in excel for visual analysis?
# impexpyear.long %>%
#   filter(year == 2) %>%
#   select(year, school_name, school_n, all_of(crit.vars), ends_with("exp")) %>%
#   rename_with(.cols = ends_with("exp"), ~paste0(., "_W4")) %>% # make sure W2 and W4 corresponds with year 1 and year 2, respectively
#   openxlsx::write.xlsx('Implementation by Year and School2.xlsx')

## Sources of Strength Program Implementation by School for Publication
# Name formatting occurs in Imp3-Report_Year
impbyschool.temp <- impexpyear.long %>%
  select(year, school_name, school_n, aa_n, pl_perc, aa_pl_ratio, meet_n, pl_meet_att_perc, camp_n) %>%
  gather(variable, value, -year, -school_name)

impbyschool.msd <- impbyschool.temp %>%
  group_by(year, variable) %>%
  summarize(Mean = mean(value, na.rm = TRUE),
            SD = sd(value, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(combo = paste(variable, year, sep = "_y"),
         est = paste0(format(round(Mean, 2), nsmall = 2), " (", format(round(SD, 2), nsmall = 2), ")"),
         school_name = "Mean (SD)") %>%
  select(school_name, combo, est) %>%
  spread(combo, est) %>%
  select(Name = school_name, School_n = school_n_y1, everything(), -school_n_y2)

imbyschool <- impbyschool.temp %>%
  mutate(combo = paste(variable, year, sep = "_y")) %>%
  select(school_name, combo, value) %>%
  spread(combo, value) %>%
  select(Name = school_name, School_n = school_n_y1, everything(), -school_n_y2) %>%
  arrange(School_n) %>%
  mutate(across(c(starts_with("aa_pl"), contains("_per_y"), contains("perc_y")), ~format(round(., 1), nsmall = 1))) %>%
  mutate(across(c(starts_with("aa_n"), starts_with("camp_"), starts_with("meet_"), "School_n"), as.character)) %>%
  select(Name, School_n, starts_with("aa_n"), starts_with("pl_perc"), starts_with("aa_pl"), starts_with("meet"),
         starts_with("pl_meet"), starts_with("camp")) %>%
  bind_rows(impbyschool.msd)
  


# ----  Implementation to Student-level Exposure  -------

#### adding school-level implementation and exposure to student-level data ####
## long format - excludes wave 3
dstxlongy <- dstxlong %>%
  filter(wave %in% c(2, 4)) %>%
  mutate(year = case_when(wave == 2 ~ 1,
                          wave == 4 ~ 2)) %>%
  left_join(impexpyear.long %>% select(school_name, year, school_n, school_n100,
                                       aa_pl_ratio_z,all_of(crit.vars), ends_with("_exp")),
            by = c("school_name", "year")) %>%
  select(year, school_name, school_n, school_n100, Tx, StudentID, everything())


## wide format - includes exposure at all 3 waves
dstxwidey <- left_join(dstxwide, impexpyear.wide, by = "school_name")


#### Multilevel Models ####
## ICC of exposure at each wave
exp.icc <- map_dfr(.x = c("Active_Exposure_W2", "Passive_Exposure_W2", "Active_Exposure_W4", "Passive_Exposure_W4"),
                   ~lmer(formula(paste0(.x, " ~ 1 + (1|school_name)")), data = dstxwidey) %>%
                     performance::icc(by_group = TRUE), .id = "Outcome") %>%
  mutate(Outcome = c("Active_Exposure_W2", "Passive_Exposure_W2", "Active_Exposure_W4", "Passive_Exposure_W4")) %>%
  select(-Group)

## Bivariate association between each implementation variable and each student-level exposure by wave
# adjusting for school size
num.vars <- length(crit.vars)
exp.mods <- map2(.x = c(rep(c(paste0(crit.vars, "_y1")),2), rep(c(paste0(crit.vars, "_y2")), 2)),
                 .y = rep(c("Active_Exposure_W2", "Passive_Exposure_W2", "Active_Exposure_W4", "Passive_Exposure_W4"), each = num.vars),
                 ~lmer(formula(paste0(.y, " ~ 1 + ", .x, " + school_n100 + Rural + (1|school_name)")), data = dstxwidey))

## extracting parameter estimates
exp.params <- map(exp.mods, ~parameters::model_parameters(.x) %>%
                    select(Parameter, Coefficient, CI_low, CI_high, p) %>%
                    mutate(across(.cols = Coefficient:CI_high, ~format(round(., 2), nsmall = 2))))

## Formatting parameter estimates for table and plot outcomes
exp.tab.prep <- map(exp.params, ~.x %>%
                      filter(!str_detect(Parameter, "\\(") & !Parameter %in% c("school_n100", "Rural")) %>% # not including intercept and school size
                      mutate(pstar = case_when(p < .01 ~ "**",
                                               p < .05 ~ "*",
                                               TRUE ~ ""),
                             b_unstd = paste0(Coefficient, " [", CI_low, ", ", CI_high, "]", pstar),
                             Variable = str_remove(Parameter, "_y[0-9]")))


## output tables
exp.tab.long <- Reduce(bind_rows, exp.tab.prep) %>%
  mutate(Outcome = rep(c("Active_Exposure_W2", "Passive_Exposure_W2", "Active_Exposure_W4", "Passive_Exposure_W4"), each = num.vars),
         pstar = factor(pstar))

exp.tab.wide <- left_join(Reduce(bind_rows, exp.tab.prep[1:num.vars]) %>% select(Variable, b_unstd) %>% rename(Active_W2 = b_unstd),
                          Reduce(bind_rows, exp.tab.prep[(num.vars+1):(num.vars*2)]) %>% select(Variable, b_unstd) %>% rename(Passive_W2 = b_unstd), by = "Variable") %>%
  left_join(Reduce(bind_rows, exp.tab.prep[(num.vars*2+1):(num.vars*3)]) %>% select(Variable, b_unstd) %>% rename(Active_W4 = b_unstd), by = "Variable") %>%
  left_join(Reduce(bind_rows, exp.tab.prep[(num.vars*3+1):(num.vars*4)]) %>% select(Variable, b_unstd) %>% rename(Passive_W4 = b_unstd), by = "Variable")

## Plot of bivariate associations 
# All variables
exp.plot <- exp.tab.long %>%
  mutate(across(Coefficient:CI_high, as.numeric)) %>%
  ggplot(aes(x = Coefficient, y = Variable, colour = pstar)) +
  geom_pointrange(aes(xmin = CI_low, xmax = CI_high)) +
  labs(y = "Implementation Variable") +
  theme_bw() +
  facet_wrap(~Outcome)

# For Supplemental Materials
exp.sm.plot <- exp.tab.long %>%
  mutate(Outcome = str_replace_all(Outcome, "Active_Exposure_", "Participatory ") %>%
           str_replace_all(., "Passive_Exposure_", "Non-participatory "),
         Variable = str_replace(Variable, "aa_n", "AA N") %>%
           str_replace(., "pl_perc", "Percent PL") %>%
           str_replace(., "aa_pl_ratio", "PL-AA Ratio") %>%
           str_replace(., "meet_n", "Meeting N") %>%
           str_replace(., "pl_meet_att_perc", "Avg PL Meeting Attendance") %>%
           str_replace(., "camp_n", "Campaign N")) %>%
  mutate(`p-value` = case_when(p < .01 ~ "< .01",
                               p < .05 ~ "< .05",
                               TRUE ~ ">= .05")) %>%
  filter(!Variable %in% c("aa_it_mod", "pl_meet_maxn", "pl_meet_act_avg",
                          "pl_active_prop","pl_camp_inv_perc","camp_act_avg")) %>%
  mutate(across(Coefficient:CI_high, as.numeric)) %>%
  ggplot(aes(x = Coefficient, y = Variable, colour = `p-value`)) +
  geom_pointrange(aes(xmin = CI_low, xmax = CI_high)) +
  labs(y = "Implementation Variable") +
  scale_colour_brewer(palette="Set1", direction=-1) +
  # scale_x_continuous(breaks = seq(-.3, .3, .1)) +
  theme_bw() +
  facet_wrap(~Outcome)




# exp.perform <- map(exp.con, ~performance::model_performance(.x))


####################################################

## grabbing variable definitions from first tab
critvar.tab <- readxl::read_excel("Output/Reports/Implementation/Implementation by School.xlsx", sheet = "Variables") %>%
  flextable() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  padding(padding = 1, part = "all") %>%
  autofit()

# ------ Saving objects for reporting --------------
save(dstxlongy, dstxwidey, impexpyear.long, impexpyear.wide,
     schoolexp, schoolexp.dist, plexp, pl.exp.test,
     aa.assoc, pl.assoc, cs.assoc, crit.vars, critvar.tab,
     imp.descrips, impcorrs, impcorr.plots, impcorrs.sm, impcorr.sm.plots,
     exp.icc, exp.mods, exp.params, exp.tab.prep,
     exp.tab.long, exp.tab.wide, exp.plot, exp.sm.plot, imbyschool,
     file = "Output/Implmentation_Exposure_Year_Assoc.RData")

