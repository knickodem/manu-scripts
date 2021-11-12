##############################################
#                                            #
#           Paper 1 - Abuse and SH           #
#        Preliminary Analysis & Table 1      #
#                                            #
##############################################


source("Scripts/0-PnF.R")
library(skimr)
load("Output/SS_Data_For_Analysis.RData")
load("Output/MI_For_Analysis.RData")



#### How many participants do not have a sibling? ####
sibagg <- sswidewfs %>% select(starts_with("SIB_AGG")) %>% names()
sibscore <- scale_score(sswidewfs, items = sibagg, type = "mean", min.valid = NULL)

table(is.na(sibscore), sswide$Took_Survey1, useNA = "always")
51/1563



as_factor(sswide$AGE_W1) %>% str_remove(., " years") %>%
  as.numeric() %>% skimr::skim()


# ----- Investigating School Belonging Groups ----------------------

#### Approach 1: Creating dichotomous school belonging variable ####
## Based on average response to school belonging items across all waves

# wide
belong <- sswidewfs %>%
  mutate(
    School_Belonging = scale_score(., items = paste0("schb_scale_W", 1:4), type = "mean", min.valid = 3),
    Belong = ifelse(School_Belonging > 2.5, 1, 0))
table(belong$Belong, useNA = "always") # very small Not belong group

#### Approach 2: Clustering ####
# Note: Uses raw scale scores
## smaller dataset for clustering
kwide <- sswidewfs %>%
  select(subjno, schb_scale_W1:schb_scale_W4) %>%
  filter(rowSums(is.na(.[,paste0("schb_scale_W", 1:4)])) != 4)


## Number of clusters
wss_plot <- factoextra::fviz_nbclust(x = drop_na(kwide[-1]), kmeans, method = "wss", k.max = 8)
silhouette_plot <- factoextra::fviz_nbclust(x = drop_na(kwide[-1]), kmeans, method = "silhouette", k.max = 8)
gapstat_plot <- factoextra::fviz_nbclust(x = drop_na(kwide[-1]), kmeans, method = "gap_stat", k.max = 8)
# all indicators suggest 2 clusters; maybe 4 based on wss

## kmeans method - must have complete cases
kbelong <- map(.x = 2:4, ~kmeans(kwide[-1] %>% drop_na(), centers = .x, nstart = 25))

kmean_plot <- map_dfr(.x = kbelong, ~.x$centers %>%
                        as.data.frame() %>%
                        tibble::rowid_to_column("cluster"), .id = "k") %>%
  gather(wave, belonging, -cluster, -k) %>%
  mutate(wave = str_remove(wave, "schb_scale_"),
         cluster = factor(cluster),
         k = as.character(as.numeric(k) + 1)) %>%
  ggplot(aes(x = wave, y = belonging, group = cluster, colour = cluster)) +
  geom_line(size = 2) + geom_point(size = 3) +
  scale_y_continuous(name = "Belonging Mean Score", limits = c(0,4), breaks = seq(0,4,.5)) +
  theme_bw(base_size = 18) +
  facet_wrap(~k)


## kpod - newer approach to kmeans specifically designed to handle missing data (partially observed data)
set.seed(69302) 
# set.seed(69203)  # different seeds produce different patterns for k = 4 and/or different frequencies in each group; k = 2 is consistent
kpods <- map(.x = 2:4, ~as.matrix(kwide[-1]) %>%
               kpodclustr::kpod(k = .x))

kwide.pod <- kwide %>%
  mutate(k2 = kpods[[1]]$cluster,
         k3 = kpods[[2]]$cluster,
         k4 = kpods[[3]]$cluster)

kpod_plot <- kwide.pod %>%
  gather(wave, temp, schb_scale_W1:schb_scale_W4) %>%
  gather(k, cluster, k2:k4) %>%
  mutate(wave = str_remove(wave, "schb_scale_"),
         cluster = factor(cluster)) %>%
  group_by(k, cluster, wave) %>%
  summarize(belonging = mean(temp, na.rm = TRUE)) %>%
  ggplot(aes(x = wave, y = belonging, group = cluster, colour = cluster)) +
  geom_line(size = 2) + geom_point(size = 3) +
  scale_y_continuous(name = "Belonging Mean Score", limits = c(0,4), breaks = seq(0,4,.5)) +
  theme_bw(base_size = 18) +
  facet_wrap(~k)

# lapply(paste0("k", 2:4), function(x) table(kwide.pod[[x]], kwide.pod2[[x]]))


## k-medoids method - evidence it is more robust than k means
set.seed(69303) # patterns consistent with different seeds
kclus <- map(2:4, ~cluster::clara(kwide[-1], k = .x, correct.d = TRUE))

kwide.med <- kwide %>%
  mutate(k2 = kclus[[1]]$clustering,
         k3 = kclus[[2]]$clustering,
         k4 = kclus[[3]]$clustering)

kmedoid_plot <- kwide.med %>%
  gather(wave, temp, schb_scale_W1:schb_scale_W4) %>%
  gather(k, cluster, k2:k4) %>%
  mutate(wave = str_remove(wave, "schb_scale_"),
         cluster = factor(cluster)) %>%
  group_by(k, cluster, wave) %>%
  summarize(belonging = mean(temp, na.rm = TRUE)) %>%
  ggplot(aes(x = wave, y = belonging, group = cluster, colour = cluster)) +
  geom_line(size = 2) + geom_point(size = 3) +
  scale_y_continuous(name = "Belonging Mean Score", limits = c(0,4), breaks = seq(0,4,.5)) +
  theme_bw(base_size = 18) +
  facet_wrap(~k)


## Examining cluster sizes
lapply(kbelong, "[[", "size")
lapply(kpods, function(x) table(x$cluster))
lapply(kclus, "[[", "clusinfo") 

## Approach aggreement
# map2(kbelong, paste0("k", 2:4), ~table(.x$cluster, kwide.pod[[.y]]))
# map2(kbelong, paste0("k", 2:4), ~table(.x$cluster, kwide.med[[.y]]))
map(paste0("k", 2:4), ~table(kwide.med[[.x]], kwide.pod[[.x]]))


## adding 2-cluster kpod to data
# wide
sswidewfs <- sswidewfs %>%
  left_join(select(kwide.pod, subjno, High_Belong = k2), by = "subjno") %>%
  mutate(High_Belong = High_Belong - 1, # changes coding from 1/2 to 0/1 for regressions
         High_Belong = ifelse(rowSums(is.na(.[paste0("schb_scale_W", 1:4)])) == 4, NA, High_Belong))

# long
sslong <- sslong %>%
  left_join(select(sswidewfs, subjno, High_Belong), by = "subjno")

## Mean school belongingness by group
sslong %>%
  group_by(High_Belong, Wave) %>%
  skimr::skim(schb_scale)

##################################################################################

# ---- Other Preliminary Investigations ----------------------------------

#### Missingness by cluster ####
miss.predictors <- c("Family_Conflict", "Abuse", "Sibling_Aggression", "emp_scale_W1", "Female",
                     "Black", "White", "OtherR", "High_Belong", "GRADES_W1") # Hispanic as referent
lapply(miss.predictors, function(x) table(sswidewfs$SCHOOL_ID_W1, is.na(sswidewfs[[x]])))
table(sswidewfs$SCHOOL_ID_W1, sswidewfs$Gender, useNA = "always")

## double-checking belongingness coding
# sswidewfs %>% group_by(High_Belong) %>% skim(schb_scale_W1:schb_scale_W4)

#### Delinquency spaghetti plots ####
set.seed(321)
sampA <- sample(sswidewfs[rowSums(is.na(sswidewfs[paste0("del_scale_W", 1:4)]))!= 4,]$subjno, 25)
sampB <- sample(sswidewfs[rowSums(is.na(sswidewfs[paste0("del_scale_W", 1:4)]))!= 4,]$subjno, 25)
sampC <- sample(sswidewfs[rowSums(is.na(sswidewfs[paste0("del_scale_W", 1:4)]))!= 4,]$subjno, 25)
sampD <- sample(sswidewfs[rowSums(is.na(sswidewfs[paste0("del_scale_W", 1:4)]))!= 4,]$subjno, 25)

del.spaghetti <- sslong %>%
  filter(subjno %in% c(sampA, sampB, sampC, sampD)) %>%
  mutate(Year = case_when(Wave == 1 ~ 0,
                          Wave == 2 ~ .5,
                          Wave == 3 ~ 1.5,
                          Wave == 4 ~ 2.5),
         Sample = case_when(subjno %in% sampA ~ "A",
                            subjno %in% sampB ~ "B",
                            subjno %in% sampC ~ "C",
                            subjno %in% sampD ~ "D")) %>%
  ggplot(aes(x = Year, y = del_scale, group = subjno)) +
  geom_line() +
  stat_smooth(aes(group = 1), size = 2, method = "loess") +
  scale_y_continuous(name = "Scale Score", limits = c(0, 4), breaks = seq(0, 4, .5)) +
  theme_bw(base_size = 18) +
  facet_wrap(~Sample, ncol = 2)

sh.highschb.spaghetti <- sslong %>%
  filter(High_Belong == 1) %>%
  # filter(subjno %in% c(sampA, sampB, sampC, sampD)) %>%
  mutate(Year = case_when(Wave == 1 ~ 0,
                          Wave == 2 ~ .5,
                          Wave == 3 ~ 1.5,
                          Wave == 4 ~ 2.5)) %>%
  ggplot(aes(x = Year, y = sh_scale, group = subjno)) +
  geom_line() +
  stat_smooth(aes(group = 1), size = 2, method = "loess", level = .90) +
  scale_y_continuous(name = "Scale Score", limits = c(0, 6), breaks = seq(0, 6, .5)) +
  theme_bw(base_size = 18)
# sslong %>%
#   group_by(High_Belong, Wave) %>%
#   summarize(SD = sd(sh_scale, na.rm = TRUE))

#############################################################################

# --------------- Creating Mplus data files -------------------------------
mpluswide <- sswidewfs %>%
  select(subjno, School = SCHOOL_ID_W1, Gender:OtherR, High_Belong, Age = AGE_W1, Grades = GRADES_W1,
         Family_Conflict:sh_scale_W4) %>%  #sh1:schb4 to include factor scores
  rename(FamCon = Family_Conflict, SibAgg = Sibling_Aggression, SchBel = High_Belong) %>%
  rename_with(~str_replace_all(., "_scale_W", "_w")) %>%
  mutate(Race4 = as.numeric(Race4)) %>%
  mutate(across(where(is.numeric), ~replace_na(., -999))) %>%
  mutate(across(where(is.character), ~replace_na(., "-999")))

# checks
names(mpluswide) # must match names in mplus input files
map_dbl(.x = names(mpluswide), ~colSums(is.na(mpluswide[, .x]))) # NAs have been replaced

# ## Exporting files
# write.table(mpluswide, file = "mplus files/mpluswide.csv", row.names = FALSE, col.names = FALSE, sep = ",")
# write.table(mpluswide, file = "mplus files/mpluswide_colnames.csv", row.names = FALSE, col.names = TRUE, sep = ",")

# Gender
# mpluswide %>%
#   filter(Female == 1) %>%
#   write.table(., file = "mplus files/mpluswide_female.csv", row.names = FALSE, col.names = FALSE, sep = ",")

# Race
# mpluswide %>%
#   filter(Race4 == 4) %>%
#   write.table(., file = "mplus files/mpluswide_otherrace.csv", row.names = FALSE, col.names = FALSE, sep = ",")

# School Belonging
# mpluswide %>%
#   filter(SchBel == 1) %>%
#   write.table(., file = "mplus files/mpluswide_highschbel.csv", row.names = FALSE, col.names = FALSE, sep = ",")


#### Checking new export with previous ones ####
# mwnames <- c("subjno", "School", "Gender", "Female", "Race", "Race4",
#              "Black", "Hispanic", "White", "OtherR", "SchBel",
#              "Age", "Grades", "FamCon", "Abuse", "SibAgg",
#              "del_w1", "del_w2", "del_w3", "del_w4",
#              "dep_w1", "dep_w2", "dep_w3", "dep_w4",
#              "emp_w1", "emp_w2", "emp_w3", "emp_w4",
#              "schb_w1", "schb_w2", "schb_w3", "schb_w4",
#              "sh_w1", "sh_w2", "sh_w3", "sh_w4")
# mw2 <- readr::read_csv("mplus files/Latent Growth/mpluswide.csv",
#                        col_names = FALSE, na = "-999")
# names(mw2) <- mwnames
# 
# table(mw2$SchBel, mpluswide$SchBel, useNA = "always")
# table(mw2$del_w1, mpluswide$del_w1, useNA = "always")
# skim(mw2$del_w1)
# skim(mpluswide$del_w1) # re-run mpluswide without converting NA to -999

#############################################################33

# ---- ICC for all constructs ----
avg.cluster.size = nrow(sswidewfs) / length(unique(sswidewfs$SCHOOL_ID_W1))
all_icc <- map2_dfr(.x = c("dep", "emp", "del", "schb", "sh"),
                    .y = c("Depression", "Lack of Empathy","Delinquency",
                           "School Belonging",
                           "SH Perpetration"),
                    ~scale_icc(sswidewfs, scale = .x, logistic = FALSE) %>%
                      mutate(Construct = .y)) %>%
  mutate(Group = recode(Group, `subjno:SCHOOL_ID_W1` = "Student", SCHOOL_ID_W1 = "School")) %>%
  spread(Group, ICC) %>%
  mutate(Time = 1 - (Student + School),
         Total = School + Student + Time,
         Design_Effect = 1 + School*(avg.cluster.size - 1))


##########################################
####         Creating Table 1         ####

# ----   Scale Correlations   ---------------------

# cor(sswidewfs$White, sswidewfs$Black)
# table(sswidewfs$White, sswidewfs$Black)

#### With Raw Scores ####
## variables in the intended order
raw.corrder <- sswidewfs %>%
  select(sh_scale_W1:sh_scale_W4, #del_scale_W1:del_scale_W4,dep_scale_W1:dep_scale_W4,
         Family_Conflict, Abuse, Sibling_Aggression, emp_scale_W1,
         Female, Black, White, OtherR, High_Belong, GRADES_W1) %>% names()

## shortcut for recoding
raw.corrtop <- 1:length(raw.corrder)
names(raw.corrtop) <- raw.corrder

## correlations in long format
raw.corrs <- sswidewfs %>%
  select(all_of(raw.corrder)) %>%
  rquery.cormat(type = "flatten") %>%
  left_join(all.relis, by = c("row" = "scale")) %>%
  left_join(all.relis, by = c("column" = "scale"), suffix = c(".row", ".col")) %>%
  mutate(across(.cols = c(alpha.row:omega.col), ~ifelse(is.na(.), 1, .))) %>%
  mutate(dis.cor = cor / sqrt(omega.row * omega.col),
         column = factor(column, levels = raw.corrder)) %>%
  arrange(column) %>%
  mutate(column = as.character(column) %>%
           str_replace_all("_scale", "") %>%
           str_replace_all("sh_", "Sexual Harassment ") %>%
           str_replace_all("del_", "Delinquency ") %>%
           str_replace_all("dep_", "Depression ") %>%
           str_replace_all("emp_", "Lack of Empathy ") %>%
           str_replace_all("_", " ") %>%
           as_factor(.),
         top = dplyr::recode(row, !!!raw.corrtop))


## disattentuated
disatt <- bind_rows(data.frame(column = "Sexual Harassment W1", top = 1:14, temp = NA) %>%
                      spread(top, temp),
                    raw.corrs %>%
                      select(column, top, dis.cor) %>%
                      spread(top, dis.cor) %>%
                      mutate(`14` = NA)) %>%
  select(-column) %>% as.matrix()

## output table
raw.corrs.table <- bind_rows(data.frame(column = "Sexual Harassment W1", top = 1:14, temp = NA) %>%
                               spread(top, temp),
                             raw.corrs %>%
                               select(column, top, CorSig) %>%
                               spread(top, CorSig) %>%
                               mutate(`14` = NA)) %>%
  tibble::rownames_to_column("temp") %>%
  unite(col = "var", temp, column, sep = ". ")



# ---- Variable Descriptive Statistics ----
# adding skewness and kurtosis functions
sk <- skim_with(numeric = sfl(skew = ~parameters::skewness(x=.)[[1]], kurtosis = ~parameters::kurtosis(x=.)[[1]]))

## raw scale scores
raw.skimmed <- sk(sswidewfs, all_of(raw.corrder)) %>%
  skim_table1(raw.corrtop)

# ## factor scores
# fs.skimmed <- sk(sswidewfs, all_of(fs.corrder)) %>%
#   skim_table1(fs.corrtop)


#### Creating Final Table ####
raw.tab1 <- bind_rows(raw.corrs.table,
                      data.frame(var = "Descriptives", top = 1:14, temp = NA) %>%
                        spread(top, temp),
                      raw.skimmed)

# raw.tab1.flex <- flextable(raw.tab1) %>% autofit() %>% fit_to_width(10)
# save_as_docx(raw.tab1.flex, path = "table1.docx")


# ---- Hostile Home Scale Distribution ---------------
## Plot
hh.dist.plot <- purrr::map2(.x = c("Family_Conflict", "Abuse", "Sibling_Aggression"),
                            .y = c(.33, 1, .5),
                            ~ggplot(sswidewfs, aes_string(x = .x)) +
                              geom_histogram(binwidth = .y, color = "white") +
                              theme_bw(base_size = 16)) %>%
  set_names(c("Family_Conflict", "Abuse", "Sibling_Aggression"))

## Table
hh.dist.tab <- purrr::map(.x = c("Family_Conflict", "Abuse", "Sibling_Aggression"),
                          ~FreqProp(sswidewfs[[.x]], useNA = "always", varnames = "Score") %>%
                            mutate(Score = round(as.numeric(as.character(Score)), 2))) %>%
  set_names(c("Family_Conflict", "Abuse", "Sibling_Aggression"))

#### Saving Output ####
save(all_icc, hh.dist.plot, hh.dist.tab,
     raw.corrder, raw.corrs, raw.corrs.table,
     kwide, wss_plot, silhouette_plot, gapstat_plot,
     kbelong, kmean_plot, kpods, kpod_plot, kclus, kmedoid_plot,
     # fs.corrder, fs.corrs, fs.corrs.table,
     raw.skimmed, #fs.skimmed,
     raw.tab1, del.spaghetti, sh.highschb.spaghetti,
     file = "Output/Table_1_Info.RData")

load("Output/Table_1_Info.RData")

save(sswide, sslong, sswidewfs,
     file = "Output/SS_Data_For_Analysis_wFS.RData")

#####################################################

## Examining SH item correlations
# sh.items <- select(, all_of(paste0(paste(sh_items[1:7], rep(1:4, each = length(sh_items)), sep = "_W"),"_di")),
#                      starts_with("SH_PERP812")) %>% names()
# sh.gen.corrs <- select(, all_of(sh.items), Female) %>%
#     lavCor(., ordered = TRUE, group = "Female", estimator = "WLSMV", missing = "pairwise", cor.smooth = FALSE, output = "cor") %>%
#   set_names(c("Male", "Female")) %>% map(.x = ., flatten_cor_matrix)
# View(sh.gen.corrs$Female$r)

# corrplot::corrplot.mixed(tl.pos = "d", tl.col="black",
#                          order = "hclust")