###################################################
#                                                 #
#     Measuring altruism born of suffering        #
#                DIF Analysis                     #
#                                                 #
###################################################

# ------   Setup   -----------------------
# remotes::install_github("knickodem/DIFreport")
library(DIFreport)

## Load packages and custom functions
source("PnF.R")
load("FA_IRT_Results_test.RData")

# -----  Differential Item Functioning  ---------------

aa.dat <- aa.dat %>%
  mutate(Sex = as_factor(Sex) %>%
           fct_recode(Girl = "Female", Boy = "Male"))


##############################
####       Altruism       ####

####   DIF by Sex  ####
## prepare data
alt.sex.prep <- dif_prep(data = aa.dat, dif.groups = "Sex",
                         items = alt.items[-c(7,8)], poly.items = alt.items[-c(7,8)])

## loess plots
alt.sex.loess <- dif_analysis(alt.sex.prep, dif.methods = "loess", match.type = "Total")

## DIF Analysis
alt.sex.irt <- dif_irt(item.data = alt.sex.prep$item.data,
                       dif.groups = alt.sex.prep$dif.groups,
                       item.type = "graded",
                       anchors = NULL,
                       method = "MHRM",
                       Wald = TRUE)

alt.sex.irt$parameters <- get_irt_params(alt.sex.irt$adj.mod)

## Predicted item scores ignoring DIF
alt.sex.item.plot <- item_score_plot(alt.sex.irt$anchor.test$s2.mod)

## Estimate impact
alt.sex.impact <- dif_impact(alt.sex.irt, alt.sex.prep)

## Plot impact
alt.sex.impact$plot <- impact_plot(alt.sex.impact$effects)




####   DIF by Early v Middle  ####
## prepare data
alt.em.prep <- aa.dat %>%
  filter(Age_Group != "Late") %>%
  dif_prep(data = ., dif.groups = "Age_Group",
           items = alt.items[-c(7,8)], poly.items = alt.items[-c(7,8)])

## loess plots
alt.em.loess <- dif_analysis(alt.em.prep, dif.methods = "loess", match.type = "Total")

## DIF Analysis
alt.em.irt <- dif_irt(item.data = alt.em.prep$item.data,
                      dif.groups = alt.em.prep$dif.groups,
                      item.type = "graded",
                      anchors = NULL,
                      method = "MHRM",
                      Wald = TRUE)

alt.em.irt$parameters <- get_irt_params(alt.em.irt$anchor.test$s1.mod)

## Predicted item scores ignoring DIF
alt.em.item.plot <- item_score_plot(alt.em.irt$anchor.test$s2.mod)

## Estimate impact
alt.em.impact <- dif_impact(alt.em.irt, alt.em.prep)

# No DIF

####   DIF by Early v Late  ####
## prepare data
alt.el.prep <- aa.dat %>%
  filter(Age_Group != "Middle") %>%
  dif_prep(data = ., dif.groups = "Age_Group",
           items = alt.items[-c(7,8)], poly.items = alt.items[-c(7,8)])

## loess plots
alt.el.loess <- dif_analysis(alt.el.prep, dif.methods = "loess", match.type = "Total")

## DIF Analysis
alt.el.irt <- dif_irt(item.data = alt.el.prep$item.data,
                      dif.groups = alt.el.prep$dif.groups,
                      item.type = "graded",
                      anchors = NULL,
                      method = "MHRM",
                      Wald = TRUE)

alt.el.irt$parameters <- get_irt_params(alt.el.irt$adj.mod)

## Predicted item scores ignoring DIF
alt.el.item.plot <- item_score_plot(alt.el.irt$anchor.test$s2.mod)


## Estimate impact
alt.el.impact <- dif_impact(alt.el.irt, alt.el.prep)

## Plot impact
alt.el.impact$plot <- impact_plot(alt.el.impact$effects)



####   DIF by Middle v Late  ####
## prepare data
alt.ml.prep <- aa.dat %>%
  filter(Age_Group != "Early") %>%
  dif_prep(data = ., dif.groups = "Age_Group",
           items = alt.items[-c(7,8)], poly.items = alt.items[-c(7,8)])

## loess plots
alt.ml.loess <- dif_analysis(alt.ml.prep, dif.methods = "loess", match.type = "Total")

## DIF Analysis
alt.ml.irt <- dif_irt(item.data = alt.ml.prep$item.data,
                      dif.groups = alt.ml.prep$dif.groups,
                      item.type = "graded",
                      anchors = NULL,
                      method = "MHRM",
                      Wald = TRUE)

alt.ml.irt$parameters <- get_irt_params(alt.ml.irt$adj.mod)

## Predicted item scores ignoring DIF
alt.ml.item.plot <- item_score_plot(alt.ml.irt$anchor.test$s2.mod)


## Estimate impact
# alt.ml.impact <- dif_impact(alt.ml.irt, alt.ml.prep)
# error with fscores using the drop.mod; can't figure out why,
# so calculating using latent variable estimates from drop.mod


# shortcuts
item.data = alt.ml.prep$item.data
dif.groups = alt.ml.prep$dif.groups
dif.items = alt.ml.irt$dif.items
irt.output <- alt.ml.irt
item.type = "graded"
method = "MHRM"
irt.scoring = "EAP"

# run model dropping DIF items
drop.mod <- mirt::multipleGroup(item.data[-dif.items], model = 1, itemtype = item.type,
                                group = dif.groups,
                                method = method, SE = F, # don't need SE for scores (at least not EAP)
                                invariance = c('slopes', 'intercepts',
                                               "free_means", "free_var"))
## calculating effect size
# sample sizes
mirt.groups <- extract.mirt(drop.mod, "group")
nTC <- table(mirt.groups)
N <- sum(nTC)

# mean, var, sd
drop.coefs <- coef(drop.mod)
M <- c(drop.coefs[[1]]$GroupPars[[1]], drop.coefs[[2]]$GroupPars[[1]])
Var <- c(drop.coefs[[1]]$GroupPars[[2]], drop.coefs[[2]]$GroupPars[[2]])
SD <- sqrt(((nTC[1] - 1) * Var[1] + (nTC[2] - 1) * Var[2]) / (sum(nTC) - 2))

cluster.n <- icc <- 0
coeff <-  1 - (2 * (cluster.n - 1) * icc) / (N - 2)
effect.size <- ((M[2] - M[1]) / SD)* sqrt(coeff)

# Effect size SE (Hedges 2007 Eq 16)
part1 <- N / (nTC[1]* nTC[2]) * (1 + (cluster.n - 1) * icc)
part2.num <- (N - 2) * (1 - icc)^2 + cluster.n * (N - 2 * cluster.n) * icc^2 +
  2 * (N - 2 * cluster.n ) * icc * (1 - icc)
part2.denom <- 2 * (N - 2) * ((N - 2) - 2 * (cluster.n - 1) * icc)
effect.size.se <- sqrt(part1 + effect.size^2 * (part2.num / part2.denom))

names(effect.size) <- names(effect.size.se) <- NULL
drop.effects <- data.frame(effect.size = effect.size, effect.size.se = effect.size.se)

# gather models
dif.models <- list(no.dif.mod = irt.output$anchor.test$s1.mod,
                   drop.mod = drop.mod,
                   adj.mod = irt.output$adj.mod) # change to adj.mod in package (or subsequent runs)

# estimate scores
irt.all <- mirt::fscores(dif.models$no.dif.mod, method = irt.scoring)
# irt.drop <- mirt::fscores(dif.models$drop.mod, method = irt.scoring) # grab latent means instead?
irt.adj <- mirt::fscores(dif.models$adj.mod, method = irt.scoring)
total.all <- scale_score(item.data, items = names(item.data), type = "mean", min.valid = length(item.data))
total.drop <- scale_score(item.data[-dif.items], items = names(item.data)[-dif.items],
                          type = "mean", min.valid = length(item.data[-dif.items]))
scores <- data.frame(irt.all, irt.adj, total.all, total.drop)
score.type <- c("IRT: All Items", "IRT: DIF Omitted", "IRT: DIF Adjusted","Total: All Items", "Total: DIF Omitted")

# # check if hedges2007 gets same outuput
# Reduce(rbind, lapply(scores, effectsize::hedges_g, y = dif.groups, pooled_sd = TRUE))

# Compute effect sizes (and 95% ci)
effects <- data.frame(Reduce(rbind, lapply(scores, DIFreport::hedges2007, groups = dif.groups,
                                           pooled = TRUE)), row.names = NULL)
effects <- rbind(effects, drop.effects)
effects <- effects[c(1,5,2,3,4),] # reordering rows to match dif_impact output
effects <- cbind(data.frame(Type = score.type), effects)
effects$ci.lo <- effects$effect.size - effects$effect.size.se*qnorm(.975)
effects$ci.hi <- effects$effect.size + effects$effect.size.se*qnorm(.975)

alt.ml.impact <- list(effects = effects,
                      scores = cbind(scores, data.frame(dif.groups = dif.groups)),
                      dif.models = dif.models)

## Plot impact
alt.ml.impact$plot <- impact_plot(alt.ml.impact$effects)


###############################################################

#### Combined Tables and Figures ####

## Anchor Table
anchor.tabs <- map(.x = list(alt.sex.irt, alt.em.irt, alt.el.irt, alt.ml.irt),
                   ~.x$anchor.test$anchor.df %>%
                     tibble::rownames_to_column("Item") %>%
                     select(Item, Wald = W, adj.p))

anchor.out <- left_join(anchor.tabs[[1]], anchor.tabs[[2]], by = "Item", suffix = c(".sex", ".em")) %>%
  left_join(anchor.tabs[[3]], by = "Item") %>%
  left_join(anchor.tabs[[4]], by = "Item", suffix = c(".el", ".ml"))



## Effects Plot
effects.data <- bind_rows(alt.sex.impact$effects %>%
                            mutate(Group = "Girl - Boy"),
                          alt.em.impact$effects %>%
                            mutate(Group = "Early - Middle Adolescents"),
                          alt.el.impact$effects %>%
                            mutate(Group = "Late - Early Adolescents"),
                          alt.ml.impact$effects %>%
                            mutate(Group = "Late - Middle Adolescents"))

effects.plot <- effects.data %>%
  filter(Group != "Early - Middle Adolescents") %>%
  ggplot(data = ., aes(x = effect.size, y = Type, xmin = ci.lo, xmax = ci.hi)) +
  geom_vline(xintercept = 0, colour = "black" , size = 1 , linetype = 2) +
  geom_pointrange(size = 1) +
  xlab("Hedges's g [95% CI]") +
  ylab("Scoring Method") +
  # ggtitle(main) +
  facet_wrap(~Group, nrow = 1, scales = "free_x") +
  theme_bw(base_size = 18) +
  theme(strip.placement = "outside",
        strip.text.y = element_text(angle = 180, vjust = 1, face = "bold"),
        strip.background = element_blank())
# # panel.spacing = unit(0,"cm"),
# panel.background = element_blank(),
# # panel.border = element_blank(),
# panel.grid.minor=element_blank(),
# axis.line = element_line(colour = "black"),
# text = element_text(size=14))



# -----    Saving DIF Output   -------

save(alt.sex.prep, alt.em.prep, alt.el.prep, alt.ml.prep,
     alt.sex.loess, alt.em.loess, alt.el.loess, alt.ml.loess,
     alt.sex.irt, alt.em.irt, alt.el.irt, alt.ml.irt,
     alt.sex.item.plot, alt.em.item.plot, alt.el.item.plot, alt.ml.item.plot,
     alt.sex.impact, alt.em.impact, alt.el.impact, alt.ml.impact,
     anchor.out, effects.data, effects.plot,
     file = "DIF_Results_test.RData")

