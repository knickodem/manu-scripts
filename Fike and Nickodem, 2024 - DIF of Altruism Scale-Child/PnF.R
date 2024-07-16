###################################################
#                                                 #
#     Measuring altruism born of suffering        #
#            Packages and Functions               #
#                                                 #
###################################################

library(dplyr)
library(lavaan)
library(purrr)
library(forcats)
library(gtsummary)
library(flextable)
library(mirt)
library(ggplot2)


#### Item Frequencies (including NAs) for multiple items with similar response options ####
# data - items and grouping variables
# ... - unquoted names of grouping variables
# NAto0 - Convert NAs in frequency table to 0s?
get_item_freqs <- function(data, ..., NAto0 = TRUE){
  
  groups <- enquos(...)
  
  Freqs <- data %>%
    tidyr::gather(Item, Response, -c(!!!groups)) %>%
    group_by(!!!groups, Item, Response)%>%
    summarize(n = n(), .groups = "drop") %>%
    tidyr::spread(Response, n)
  
  if(NAto0 == TRUE){
    
    Freqs <- Freqs %>%
      mutate_if(is.numeric, ~tidyr::replace_na(., 0))
  }
  return(Freqs)
}


#### Calculating scale scores ####
# data - dataframe
# items - character vector
# min.valid - minimum number of valid responses required to calculate a scale score
scale_score <- function(data, items, type = c("sum", "mean"), min.valid = NULL){
  
  score <- rowSums(data[, items], na.rm = TRUE)
  
  if(type == "mean"){
    c <- rowSums(!is.na(data[, items]), na.rm = TRUE)
    score <- score / c
  }
  
  nacount <- rowSums(is.na(data[, items]), na.rm = TRUE)
  
  if(!is.null(min.valid)){
    score <- ifelse((length(items) - nacount) < min.valid, NA, score)
  } else {
    score <- ifelse(nacount == length(items), NA, score)
  }
  
  return(score)
}


# -----   IRT Model Fit Summaries   ----------

## shortcut for changing title of empirical plots
# plot - a lattice plot
# title - main title to print on plot
retitle_empplot <- function(plot, title){
  plot$main <- title
  return(plot)
}

## gathering factor loadings into a dataframe
# object - a mirt model object
# items - named vector; vector elements are the names of items used in the mirt model;
#         vector names are the form to which the items belong. If all items are from the same form then all vector names will be the same
# cut - loadings below this value will be converted to NA and hidden in reporting
factor_summary <- function(object, items, cut = .2){
  
  if(is.null(names(items))){
    fs <- cbind(data.frame(item = items), object$rotF, object$h2)
  } else {
    fs <- cbind(data.frame(type = names(items), item = items), object$rotF, object$h2)
  }
  
  fs <- fs %>%
    mutate(across(starts_with("F"), ~ifelse(abs(.x) < cut, NA, .x)))
  rownames(fs) <- NULL
  return(fs)
}


## preparing item parameters in various forms
# object - a mirt model object
# items - names of items used in the mirt model
get_item_params <- function(object, items){
  
  ## Parameters - long format
  mirt.long <- coef(object, IRTpars = TRUE, printSE = TRUE, as.data.frame = TRUE) %>%
    as.data.frame() %>% tibble::rownames_to_column("temp") %>%
    tidyr::separate(temp, into = c("item", "parameter"), sep = "\\.")
  
  # Saving parameters for test assembly (wide format)
  mirt.wide <- mirt.long %>%
    filter(item != "GroupPars") %>%
    select(-SE) %>%
    tidyr::spread(parameter, par) %>%
    mutate(item = factor(item, levels = items)) %>%
    arrange(item)
  
  ata.long <- mirt.wide %>%
    tidyr::gather(key = "threshold", value = "b", starts_with("b")) %>%
    mutate(threshold = gsub("b", "", threshold),
           grm.id = as.character(item)) %>%
    arrange(item) %>%
    tidyr::unite(col = item, item, threshold, sep = ".")
  
  return(list(mirt.long = mirt.long,
              mirt.wide = mirt.wide,
              ata.long = ata.long))
}

## gather scaling results for reporting
# mod1 - a mirt model object
# items - named or unnamed character vector; vector elements are the names of items used in the mirt model;
#         vector names (if provided) are the form to which the items belong. If no names are provided items are assumed to be from the same form
# mod2 - a second mirt model object to compare to mod1. All gathered results are from mod1
# cut - factor loadings below this value will be converted to NA and hidden in reporting
# gfit.type - global fit index
# ifit.type - item fit index
scaling_results <- function(mod1, items, mod2 = NULL, cut = .2, gfit.type = "C2", ifit.type = "PV_Q1"){
  
  ## Model Comparison
  if(!is.null(mod2)){
    
    comp <- list(`1-factor` = summary(mod1),
                 `2-factor` = summary(mod2), # default rotation is oblimin
                 Comparison = cbind(data.frame(Model = c("1-factor", "2-factor")), anova(mod1, mod2)))
    # prepping results for output report
    comp$`1-factor`$rotF <- factor_summary(comp$`1-factor`, items = items, cut = cut)
    comp$`2-factor`$rotF <- factor_summary(comp$`2-factor`, items = items, cut = cut)
  } else {
    comp <- list(`1-factor` = summary(mod1),
                 `2-factor` = NULL,
                 Comparison = NULL)
    # prepping results for output report
    comp$`1-factor`$rotF <- factor_summary(comp$`1-factor`, items, cut = cut)
  }
  
  ## Scale Info
  mrxx <- marginal_rxx(mod1) 
  scaleplots <- lapply(c("infoSE", "rxx","infotrace", "trace", "itemscore", "score"),
                       function(x) plot(mod1, type = x, as.table = TRUE))
  
  ## Global Model Fit
  gfit <- M2(mod1, na.rm = TRUE, type = gfit.type)
  
  ## Item info
  # Empirical plots
  empplot <- lapply(1:length(items),
                    function(x) itemfit(mod1, empirical.plot = x, as.table = TRUE))
  empplot <- purrr::map2(.x = empplot, .y = items, ~retitle_empplot(.x, paste("Empirical plot for", .y)))
  # empirical_plot(select(p35.int, int1:cint28), which.items = c(9) , smooth = TRUE) # too many NAs to work
  
  # Fit
  ifit <- itemfit(mod1, fit_stats = ifit.type,  na.rm = FALSE)
  ifit <- cbind(ifit, data.frame(p.adj = p.adjust(ifit$p.PV_Q1))) # BH p-value adjustment
  if(!is.null(names(items))){
    ifit <- cbind(data.frame(type = names(items)), ifit)
  }
  
  # Parameters
  params <- get_item_params(mod1, items)
  if(!is.null(names(items))){
    params$mirt.wide <- cbind(data.frame(type = names(items)), params$mirt.wide)
  }
  
  #### Gathering Results ####
  results <- list(Items = NULL,
                  `Model Summary` = comp,
                  `Scale Reliability` = mrxx,
                  `Scale Plots` = scaleplots,
                  `Empirical Plots` = empplot,
                  `Global Fit` = gfit,
                  `Item Fit` = ifit,
                  `Item Parameters` = params,
                  Multigroup = NULL)
  
  return(results)
  
}

# -----    DIF Functions    -------------------

# Item response theory DIF method
#
# Conducts DIF analysis in an item response theory framework using the iterative Wald procedure (Cao et al., 2017; Tay et al., 2015; Woods et al., 2013)
#
# item.data - data frame of item responses with subjects in rows and items in columns
# dif.groups - factor vector of group membership for which DIF is evaluated.
# item.type - For IRT, the type of model to fit for each item. The default is \code{"2PL"} for dichotomous items and \code{"graded"} for polytomous items.
#             See \code{\link[mirt]{mirt}} for more options and details.
# anchors - vector of column positions or names \code{data} indicating the anchor items. If specified, must be a subset of \code{items}.
# method - estimation method
# Wald - use Wald test (default) or likelihood ratio test?

dif_irt <- function(item.data, dif.groups, item.type,
                    anchors = NULL, method = "MHRM", Wald = TRUE){
  
  if(is.null(anchors)){
    #### Stage 1 - Identify anchors ####
    anchor.test <- tryCatch(expr = {
      find_anchors(item.data = item.data, dif.groups = dif.groups,
                   item.type = item.type, method = method, Wald = Wald)
    },
    error = function(e){
      message("Did not run IRT method. Possible empty cell(s) in the item by group
            frequency tables. If so, remove the item from measure.data")
      warning(paste("IRT", e))
      return("Error occurred")}
    )
    anchors <- anchor.test$anchors
  } else {anchor.test <- NULL}
  
  
  ## identify DIF candidate items
  all.items <- 1:ncol(item.data)  # need values for dif_type_irt
  names(all.items) <- names(item.data) # need names for invariance argument in multipleGroup
  candidates <- all.items[!all.items %in% anchors]
  
  #### Stage 2 - Identify DIF items ####
  if(length(candidates) > 0){
    
    # need to estimate information matrix for Wald test
    if(Wald == T) SE <- T else SE <- F 
    
    ## run model with anchors
    anchor.mod <- mirt::multipleGroup(item.data, model = 1, itemtype = item.type,
                                      group = dif.groups,
                                      method = method, SE = SE,
                                      invariance = c(names(anchors),
                                                     "free_means", "free_var"))
    
    ## Step 1 - Initial DIF
    step1 <- vector("list", length = length(candidates))
    for(i in 1:length(candidates)){
      step1[[i]] <- dif_type_irt(anchor.mod, item = candidates[[i]],
                                 dif.type = "omnibus", Wald = Wald)
    }
    step1.df <- Reduce(rbind, step1)
    
    # Benjamini–Hochberg procedure for false discovery rate < 5%
    step1.df$adj.p <- stats::p.adjust(step1.df$p, method = "BH")
    step1.df$DIF <- step1.df$adj.p < .05
    
    # items that remain candidates
    candidates2 <- candidates[names(candidates) %in% c(step1.df$item[step1.df$DIF == TRUE])]
    
    if(length(candidates2) > 0){ # if 0, no DIF items
      if(!identical(candidates, candidates2)){ # if the same, no need to run refinement
        
        names(step1.df)[3:7] <- paste0("initial.", names(step1.df)[3:7])
        
        ## Stage 2 - Refine/Purify
        # Re-estimate model with additional anchors
        anchor2.mod <- mirt::multipleGroup(item.data, model = 1, itemtype = item.type,
                                           group = dif.groups,
                                           method = method, SE = SE,
                                           invariance = c(names(item.data)[!names(item.data) %in% names(candidates2)],
                                                          "free_means", "free_var"))
        
        
        step2 <- vector("list", length = length(candidates2))
        for(i in 1:length(candidates2)){
          step2[[i]] <- dif_type_irt(anchor2.mod, item = candidates2[[i]],
                                     dif.type = "omnibus", Wald = Wald)
        }
        step2.df <- Reduce(rbind, step2)
        
        # Benjamini–Hochberg procedure for false discovery rate < 5%
        step2.df$adj.p <- stats::p.adjust(step2.df$p, method = "BH")
        step2.df$DIF <- step2.df$adj.p < .05
        
        names(step2.df)[3:7] <- paste0("refined.", names(step2.df)[3:7])
        
        ## Combining initial and refined output; excluding type column ("omnibus")
        item.irt <- merge(x = step1.df[-2],
                          y = step2.df[-2],
                          by = "item",
                          all.x = TRUE,
                          all.y = FALSE,
                          sort = FALSE)
        
        ## Ordering items
        # item.irt$item <- factor(item.irt$item, levels = names(item.data))
        # item.irt <- item.irt[order(item.irt$item), ]
        # row.names(item.irt) <- NULL
        dif.items <- candidates2[names(candidates2) %in% c(item.irt$item[item.irt$refined.DIF == TRUE])]
        
      } else {
        item.irt <- step1.df
        dif.items <- candidates2
        
      }
      
      ## Investigate specific type of DIF
      if(length(dif.items) > 0){
        
        # Re-estimate model with additional anchors
        anchor3.mod <- mirt::multipleGroup(item.data, model = 1, itemtype = item.type,
                                           group = dif.groups,
                                           method = method, SE = SE,
                                           invariance = c(names(item.data)[!names(item.data) %in% names(dif.items)],
                                                          "free_means", "free_var"))
        
        dif.type <- vector("list", length = length(dif.items))
        for(i in 1:length(dif.items)){
          dif.type[[i]] <- dif_type_irt(anchor3.mod, item = dif.items[[i]],
                                        dif.type = "both", Wald = Wald)
        }
        dif.type.df <- Reduce(rbind, dif.type)
        dif.type.df$p.adj <- stats::p.adjust(dif.type.df$p, method = "BH")
        dif.type.df$DIF <- dif.type.df$p.adj < .05
        
        
      } else {
        dif.items <- "No DIF was detected"
      }
      
      ## Merging initial and refined stage results
      irt <- list(anchor.test = anchor.test,
                  item.test = item.irt,
                  dif.items = dif.items,
                  dif.type = dif.type.df,
                  adj.mod = anchor3.mod) # model adjusting for DIF
      
    } else {
      
      irt <- list(anchor.test = anchor.test,
                  item.test = step1.df,
                  dif.items = "No DIF was detected",
                  dif.type = "None",
                  adj.mod = NULL) # model adjusting for DIF
    }
    
  }  else {
    
    irt <- list(anchor.test = anchor.test,
                item.test = "No item DIF was detected",
                dif.items = "No DIF was detected",
                dif.type = "None",
                adj.mod = NULL) # model adjusting for DIF
  }
  return(irt)
}


# Subroutine used in dif_irt for identifying anchor items using the iterative Wald procedure
# item.data - data frame of item responses with subjects in rows and items in columns
# dif.groups - factor vector of group membership for which DIF is evaluated.
# item.type - For IRT, the type of model to fit for each item. The default is \code{"2PL"} for dichotomous items and \code{"graded"} for polytomous items.
#             See \code{\link[mirt]{mirt}} for more options and details.
# anchors - vector of column positions or names \code{data} indicating the anchor items. If specified, must be a subset of \code{items}.
# method - estimation method
# Wald - use Wald test (default) or likelihood ratio test?

find_anchors <- function(item.data, dif.groups, item.type, method = "MHRM", Wald = T){
  
  if(Wald == T) SE <- T else SE <- F
  
  ## Step 1 syntax and model
  s1.syn <- paste0("F1 = 1-", length(item.data)) # this assumes a 1-dimensional model; need to make more general
  s1.mod <- mirt::multipleGroup(item.data, model = s1.syn, itemtype = item.type, group = dif.groups,
                                    method = method, SE = SE,
                                    invariance = c('slopes', 'intercepts',
                                                   'free_var','free_means'))
  
  ## extracting parameter values and generating syntax for step 2
  # save latent trait estimates for comparison group
  comp.pars <- coef(s1.mod)[[2]] # assumes only 2 groups
  
  # get starting values for step 2 model
  start.pars <- mirt::multipleGroup(item.data, model = s1.syn, itemtype = item.type,
                                    group = dif.groups, pars="values")
  npars <- nrow(start.pars) # number of parameters
  
  # replacing mean and variance values for comparison group with Step 1 values
  start.pars[c(npars-1, npars), "value"] <- c(comp.pars$GroupPars[1,]) # assumes parameters are the last two in the table
  
  # Step 2 model - fixes latent trait parameters
  s2.syn  <- paste0(s1.syn, "\nFIXED = (GROUP, MEAN_1), (GROUP, COV_11)")
  
  ## Running step 2 model - checking for any DIF
  s2.mod <- mirt::multipleGroup(item.data, model = s2.syn, itemtype = item.type,
                                group = dif.groups, method = method, SE = SE, pars = start.pars)
  
  ## Model comparisons
  comp.tab <- mirt::anova(s1.mod, s2.mod, verbose = FALSE)
  
  ## Step 3 (if necessary) - identify anchors
  if(comp.tab$p[[2]] < .05) {
    
    ## Number of items in the measure
    nitems <- ncol(item.data)
    
    anchor.list <- vector("list", length = nitems)
    for(i in 1:nitems){
      
      itemtype <- mirt::extract.mirt(s2.mod, "itemtype")[[i]]
      which.par <- dimnames(coef(s2.mod)[[1]][[i]])[[2]]
      
      anchor.list[[i]] <- mirt::DIF(s2.mod,
                            which.par = which.par,
                            items2test = i,
                            scheme = "add", #seq_stat = .05, max_run = 2,
                            Wald = Wald, # can only be TRUE if scheme = "add"
                            return_models = FALSE) 
    }
    
    # convert list to df
    anchor.df <- Reduce(rbind, anchor.list)
    
    # Benjamini–Hochberg procedure for false discovery rate < 5%
    anchor.df$adj.p <- stats::p.adjust(anchor.df$p, method = "BH")
    anchor.df$DIF <- anchor.df$adj.p < .05
    anchors <- which(anchor.df$DIF == FALSE)
    names(anchors) <- row.names(anchor.df)[anchors]
    
    anchor.test <- list(anchors = anchors,
                        anchor.df = anchor.df,
                        s1.mod = s1.mod,
                        s2.mod = s2.mod)
    
  } else {
    anchors <- 1:nitems
    names(anchors) <- names(item.data)
    anchor.test <- list(anchors = anchors,
                        anchor.df = NULL,
                        s1.mod = s1.mod, #no.dif.mod (fully constrained items; free latent mean and var)
                        s2.mod = s2.mod) #free item params
    
  }
  return(anchor.test)
}

# IRT item tests for uniform and nonuniform DIF
#
# model - mirt object
# item - character or index indicating item to test for DIF
# dif.type - one of "uniform", "nonuniform", "omnibus" (default; tests for both types simultaneously)
#            or "both" (tests both types separately)
# scheme - see mirt::DIF
# Wald - use Wald test (default) or likelihood ratio test?

dif_type_irt <- function(model,
                         item,    
                         dif.type = "omnibus", # "uniform", "nonuniform", "both"
                         scheme = "add",
                         Wald = TRUE){
  
  # identify if item is dichotomous or polytomous
  itemtype <- mirt::extract.mirt(model, "itemtype")[[item]]
  
  # extract possible item parameters to test
  which.par <- dimnames(coef(model)[[1]][[item]])[[2]]
  
  if(dif.type == "omnibus"){
    item.dif <- mirt::DIF(model,
                          which.par = which.par,
                          items2test = item,
                          scheme = scheme, #seq_stat = .05, max_run = 2,
                          Wald = Wald, # can only be TRUE if scheme = "add"
                          return_models = FALSE) # , p.adjust = "BH", ...
    item.dif$type <- "omnibus"
    
  } else{
    
    if(dif.type %in% c("both", "uniform")){
      ## Identifying items with uniform DIF
      d.dif <- mirt::DIF(model,
                         which.par = which.par[grepl("d", which.par)],
                         items2test = item,
                         scheme = scheme, #seq_stat = .05, max_run = 2,
                         Wald = Wald, # can only be TRUE if scheme = "add"
                         return_models = FALSE) # , p.adjust = "BH", ...
      d.dif$type <- "uniform"
    } else{d.dif <- NULL}
    
    if(dif.type %in% c("both", "nonuniform")){
      ## Identifying items with uniform DIF
      a.dif <- mirt::DIF(model,
                         which.par = which.par[grepl("a", which.par)],
                         items2test = item,
                         scheme = scheme, #seq_stat = .05, max_run = 2,
                         Wald = Wald, # can only be TRUE if scheme = "add"
                         return_models = FALSE) # , p.adjust = "BH", ...
      a.dif$type <- "nonuniform"
    } else{a.dif <- NULL}
    
    item.dif <- Reduce(rbind, list(d.dif, a.dif))
  }
  
  item.dif$item <- row.names(item.dif)[[1]]
  item.dif <- item.dif[c(6,5,2,3,4)]
  row.names(item.dif) <- NULL
  
  return(item.dif)
}

# gather item parameters for output
get_irt_params <- function(model){
  temp <- coef(model, IRTpars = TRUE, printSE = TRUE, as.data.frame = TRUE)
  out <- Reduce(rbind, lapply(names(temp),
                              function(x) pars_wide(temp[[x]]) %>%
                                mutate(Group = x))) %>%
    select(Group, everything())
  return(out)
}

# subroutine for get_irt_pars for outputting item parameters
pars_wide <- function(df){
  as.data.frame(df) %>% tibble::rownames_to_column("temp") %>%
    tidyr::separate(temp, into = c("item", "parameter"), sep = "\\.")%>%
    filter(item != "GroupPars") %>%
    select(-SE) %>%
    tidyr::spread(parameter, par)
}


# Compares standardized treatment effects estimated with and without adjustments for DIF.
# 
# irt.output - output from dif_irt function
# data.prep - output from dif_prep function
# irt.scoring - see mirt::fscores
# item.type - For IRT, the type of model to fit for each item. See \code{\link[mirt]{mirt}} for more options and details.
# method - estimation method

dif_impact <- function(irt.output, data.prep,
                       irt.scoring = "EAP", item.type = "graded", method = "MHRM"){
  
  item.data = data.prep$item.data
  dif.groups = data.prep$dif.groups
  dif.items = irt.output$dif.items
  
  # What model to use if anchors were previously specified (i.e., no anchor.test)
  if(is.null(irt.output$anchor.test)){
    
  }
  
  if(is.character(dif.items)){
    warning("No DIF was detected; therefore, scores will not be impacted.")
    # gather models
    dif.models <- list(no.dif.mod = irt.output$anchor.test$s1.mod)
    
    # estimate scores
    irt.all <- mirt::fscores(dif.models$no.dif.mod, method = irt.scoring)
    total.all <- scale_score(item.data, items = names(item.data), type = "mean", min.valid = length(item.data))
    scores <- data.frame(irt.all, total.all)
    score.type <- c("IRT: All Items", "Total: All Items")
    
  } else {
  
  # run model dropping DIF items
  drop.mod <- mirt::multipleGroup(item.data[-dif.items], model = 1, itemtype = item.type,
                                  group = dif.groups,
                                  method = method, SE = F, # don't need SE for scores (at least not EAP)
                                  invariance = c('slopes', 'intercepts',
                                                 "free_means", "free_var"))
  
  # gather models
  dif.models <- list(no.dif.mod = irt.output$anchor.test$s1.mod,
                     drop.mod = drop.mod,
                     adj.mod = irt.output$adj.mod)
  
  # estimate scores
  irt.all <- mirt::fscores(dif.models$no.dif.mod, method = irt.scoring)
  irt.drop <- mirt::fscores(dif.models$drop.mod, method = irt.scoring)
  irt.adj <- mirt::fscores(dif.models$adj.mod, method = irt.scoring)
  total.all <- scale_score(item.data, items = names(item.data), type = "mean", min.valid = length(item.data))
  total.drop <- scale_score(item.data[-dif.items], items = names(item.data)[-dif.items],
                            type = "mean", min.valid = length(item.data[-dif.items]))
  scores <- data.frame(irt.all, irt.drop, irt.adj, total.all, total.drop)
  score.type <- c("IRT: All Items", "IRT: DIF Omitted", "IRT: DIF Adjusted","Total: All Items", "Total: DIF Omitted")
  
  }
  
  # # check if hedges2007 gets same outuput
  # Reduce(rbind, lapply(scores, effectsize::hedges_g, y = dif.groups, pooled_sd = TRUE))
  
  # Compute effect sizes (and 95% ci)
  effects <- data.frame(Reduce(rbind, lapply(scores, DIFreport::hedges2007, groups = dif.groups,
                                             pooled = TRUE)), row.names = NULL)
  effects <- cbind(data.frame(Type = score.type), effects)
  effects$ci.lo <- effects$effect.size - effects$effect.size.se*qnorm(.975)
  effects$ci.hi <- effects$effect.size + effects$effect.size.se*qnorm(.975)
  
  return(list(effects = effects,
              scores = cbind(scores, data.frame(dif.groups = dif.groups)),
              dif.models = dif.models))
}

# effects - output from dif_impacts$effects

impact_plot <- function(effects){
  ggplot(data = effects,
         aes(x = effect.size, y = Type, xmin = ci.lo, xmax = ci.hi)) +
    geom_vline(xintercept = 0, colour = "black" , size = 1 , linetype = 2) +
    geom_pointrange(size = 1) +
    xlab("Hedges's g [95% CI]") +
    ylab("Scoring Method") +
    # ggtitle(main) +
    theme(strip.placement = "outside",
          strip.text.y = element_text(angle = 180, vjust = 1, face = "bold"),
          strip.background = element_blank(),
          panel.spacing = unit(0,"cm"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor=element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size=14))
}

# model - mirt object

item_score_plot <- function(model){
  
  itemnames <- mirt::extract.mirt(model, "itemnames")
  groupNames <- mirt::extract.mirt(model, "groupNames")
  theta <- seq(-6, 6, length.out = 200)
  
  data <- vector("list", length(groupNames))
  for(g in 1:length(groupNames)) {
    item.score <- vector("list", length(itemnames))
    for(i in itemnames){
      icc <- extract.item(model, item = i, group = g)
      item.score[[i]] <- data.frame(Group = groupNames[g],
                                    Item = i,
                                    x = theta,
                                    #pdf = pdf(theta),
                                    Score = expected.item(icc, Theta = theta, min = 0))
    } 
    data[[g]] <- Reduce(rbind, item.score)
  }
  
  item.score.data <- Reduce(rbind, data)
  
  
  plot <- ggplot(item.score.data, aes(x = x, y = Score, group = Group)) +
    geom_line(aes(color = Group), lwd = .8) +
    # geom_ribbon(aes(ymin = prob - 1.96 * se,
    #                 ymax = prob + 1.96 * se,
    #                 fill = dif.groups), alpha = .4) +
    labs(x = "\u03b8 (SD units)",
         y = "Predicted Item Score") + 
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size = 18) +
    facet_wrap(~Item, nrow = 2, scales = "fixed") +
    theme(legend.position = "top",
          legend.title = element_blank())
  
  return(list(data = item.score.data,
              plot = plot))
}


# -----    Create Codebook    -------------------

#### Extracting labels from a factor and/or character or labelled variable ####
## labelled returns both the label and the value, otherwise only the label is returned
Get_Labels <- function(data,type=c("factor","character","labelled","factor_character")){
  
  if(type=="labelled"){
    
    ## Select labelled variables, extract labels, and transform to long format dataframe
    all_labels <- data %>% select_if(haven::is.labelled) %>%
      purrr::map(~attr(.,"labels")) %>% tibble::enframe("Item","Value") %>% 
      mutate(Label = purrr::map(Value,~attr(.,"names"))) %>% tidyr::unnest(cols = c(Value, Label))
    
  } else {
    
    ## Select variables
    if(type=="factor"){
      data <- data %>% select_if(is.factor)
    } else if(type=="character"){
      data <- data %>% select_if(is.character) %>%
        mutate_if(is.character,forcats::as_factor)
    } else if(type=="factor_character"){
      data <- data %>% mutate_if(is.character,forcats::as_factor) %>%
        select_if(is.factor)
    }
    ## Extract labels and transform to long format dataframe
    all_labels <- data %>% select_if(is.factor) %>%
      purrr::map(~levels(.)) %>% tibble::enframe("Item","Label") %>%
      unnest()
    
  }
  
  return(all_labels)
}


#### Creating and Exporting the Codebook ####
## Requires Get_Labels function from above; only tested thus far for type="labelled"
## if export_type = "none" & keep_R_Object = FALSE, the function does not output anything
Create_Codebook <- function(OrigData, export_type = c("excel","csv","none"), export_name="MyData",
                            label_type = "labelled", keep_R_Object = TRUE){
  
  ## Extract Values and Labels
  ValueLabels <- OrigData %>% Get_Labels(type = label_type)
  
  ## Putting in wide format
  VLW <- ValueLabels %>% mutate(VL = paste(Value,Label,sep=" = ")) %>%
    group_by(Item) %>% summarize(Value_Label = paste(VL,collapse="; ")) %>%
    ungroup()
  
  ## Extract Item Stem (i.e. label), then joining values and labels
  Stems <- purrr::map(OrigData,~attr(.,"label")) %>% tibble::enframe("Item","Stem")
  
  ## Joining Stems, Values, and Labels (gets us 90% of the way to the final codebook)
  Codebook <- left_join(Stems, VLW, by = "Item") %>%
    mutate(Stem = as.character(Stem))
  
  
  if(export_type == "excel"){
    
    ## Exporting to excel file
    openxlsx::write.xlsx(x = Codebook, file = paste0(export_name, ".xlsx"), asTable = FALSE)
    
    message("Codebook exported to file ", export_name, ".xlsx")
    
  } else if(export_type == "csv"){
    
    ## Exporting to csv file
    write.csv(Codebook,file = paste0(export_name, ".csv"), row.names = FALSE, na = "")
    
    message("Codebook exported to file ", export_name, ".csv")
    
  } else if(export_type!="none"){
    
    stop("Must specify 'excel', 'csv', or 'none' for export_type argument.")
    
  }
  
  if(keep_R_Object==TRUE){
    return(Codebook)
  }
  
}
