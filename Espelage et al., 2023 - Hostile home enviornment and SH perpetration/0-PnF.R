##########################################
#                                        #
#       Paper 1 - Abuse and SH           #
#      Packages and Functions            #
#                                        #
##########################################

# ------    Packages    --------
library(dplyr)
library(haven)
library(stringr)
library(tidyr)
library(ggplot2)
library(forcats)
library(flextable)
library(lavaan)
library(purrr)
library(semTools)
library(naniar)

# ----    Custom Functions     --------------------------


#### Only needed to confirm long data subsetted correctly ####
# LongWave <- function(data, wave, ...){
#   
#   wave.chr <- paste0("_W", wave)
#   
#   btsubvars <- quos(...)
#   
#   longdat <- data %>%
#     select(!!!btsubvars, ends_with(wave.chr)) %>%
#     rename_at(vars(ends_with(wave.chr)), ~str_remove(., wave.chr)) %>%
#     mutate(Wave = wave)
#   
#   return(longdat)
# }

#################################
#### Frequencies and Scoring ####

#### Item Frequencies (including NAs) for multiple items with similar response options ####
# data = items and grouping variables
# ... = grouping variables
# NAto0 = Convert NAs in frequency table to 0s?
Get_ItemFreqs <- function(data, ..., NAto0 = FALSE){
  
  groups <- enquos(...)
  
  Freqs <- data %>%
    tidyr::gather(Item, Response, -c(!!!groups)) %>%
    group_by(!!!groups, Item, Response)%>%
    summarize(n = n(), .groups = "drop") %>%
    tidyr::spread(Response, n)
  
  if(NAto0 == TRUE){
    
    Freqs <- Freqs %>%
      mutate_if(is.numeric, ~replace_na(., 0))
  }
  return(Freqs)
}

#### Frequency and Proportion for a single variable or crosstab ####
# primarily used for demographics
FreqProp <- function(x, y = NULL, useNA = "no", margin = NULL,
                     varnames = NULL, percent_integer = TRUE){
  if(!is.null(y)){
    datable <- table(x, y, useNA = useNA)
  } else{
    datable <- table(x, useNA = useNA)
  }
  
  prop <- as.data.frame(prop.table(datable, margin = margin), responseName = "Percent")
  
  if(percent_integer == TRUE){
    prop <- prop %>% mutate(Percent = round(Percent*100,0))
  }
  
  joined <- datable %>% as.data.frame(responseName = "n") %>%
    left_join(prop)
  
  if(!is.null(varnames)){
    names(joined) <- c(varnames, "n", "Percent")
  }
  
  return(joined)
}

#### Calculating scale scores ####
# items is a character vector
# min.valid = minimum number of valid responses required to calculate a scale score
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



#### converting a correlation matrix to long format ####
flatten_cor_matrix <- function(matrix){
  ut <- upper.tri(matrix)
  data.frame(
    row = rownames(matrix)[row(matrix)[ut]],
    column = rownames(matrix)[col(matrix)[ut]],
    cor  =(matrix)[ut])
  
}

#### calculating correlation matrix for various output formats ####
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        digits=2,
                        usena="pairwise.complete.obs",
                        graph=FALSE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL,
                        alpha = .05, ...)
{
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
      mutate(CorSig = paste0(cor,ifelse(as.numeric(as.character(p)) < alpha, "*", " ")),
             cor = as.numeric(cor))
    return(cormat)
  }
  
}



#### Shortcut for creating table 1 descriptive statistics ####
skim_table1 <- function(skimdf, recodes){
  table1 <- skimdf %>%
    mutate(missing_rate = 1 - complete_rate,
           top = dplyr::recode(skim_variable, !!!recodes)) %>%
    rename_with(~str_remove(., "numeric.")) %>%
    select(top, missing_rate, mean, sd, skew, kurtosis) %>% # skim_variable; include to double-check top matches skim_variable appropriately
    gather(var, stat, -top) %>%
    mutate(stat = format(round(stat, 2), nsmall = 2),
           var = factor(var, levels = c("missing_rate", "mean", "sd", "skew", "kurtosis"))) %>%
    spread(top, stat) %>%
    arrange(var)
  
}

############################################


########################
####    Plotting    ####

cond_dist_plot <- function(data, condvar){
  
  cv <- enquo(condvar)
  
  data <- data %>%
    filter(!is.na(!!cv)) %>%
    gather(Outcome, Score, matches("del_scale|dep_scale|sh_scale|schb_scale")) %>%
    mutate(!!cv := as_factor(!!cv),
           Outcome = as_factor(Outcome) %>%
             fct_recode(Delinquency = "del_scale", Depression = "dep_scale", SH = "sh_scale", `School Belonging` = "schb_scale"))
  if(rlang::as_name(cv) == "Belong"){
    data <- data %>%
      filter(Outcome != "School Belonging")
  }
  plot <- data %>%
    ggplot(aes(x = Score, group = !!cv, fill = !!cv)) +
    geom_density(alpha = .6) +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size = 16) +
    facet_grid(Outcome ~ Wave, scales = "free_y") +
    theme(strip.background = element_blank())
  return(plot)
}

## initially intended as a spaghetti plot, but now taking group means
make_ghetti <- function(data, group, scale){
  group <- enquo(group)
  scale <- enquo(scale)
  
  gdata <- data %>%
    mutate(!!group := as_factor(!!group)) %>%
    filter(!is.na(!!group)) %>%
    group_by(Wave, !!group) %>%
    summarize(n = sum(!is.na(!!scale)),
              Score = mean(!!scale, na.rm = TRUE),
              SD = sd(!!scale, na.rm = TRUE),
              se = SD / sqrt(n),
              lower.ci = Score - se*1.96,
              upper.ci = Score + se*1.96,.groups = "drop_last")
  
  plot <- gdata %>%
    ggplot(aes(x = Wave, y = Score, colour = !!group)) +
    geom_ribbon(aes(ymin = lower.ci, ymax = upper.ci, fill = !!group), alpha = .5) +
    geom_line(size = 2) +
    scale_y_continuous(limits = c(0, 4), breaks = seq(0, 4, .5)) +
    scale_fill_brewer(palette = "Set2") +
    scale_colour_brewer(palette = "Set2") +
    theme_bw()
  
  return(list(data = gdata,
              plot = plot))
}

# data is expected to be output from predict_growth() function
multigroup_plot <- function(data, ylab = "default", max.y = 4){
  
  plot <- data %>%
    ggplot(aes(x = Wave, y = Estimate, group = Group)) +
    geom_ribbon(aes(ymin = Estimate - S_interim, ymax = Estimate + S_interim), fill = "grey", alpha = .4) +
    geom_line(aes(linetype = Group), size = 1) +
    scale_linetype_manual(values = c(1, 2, 3, 4)) +
    scale_y_continuous(limits = c(0, max.y), breaks = seq(0, max.y, .5)) +
    scale_x_continuous(name = "Year", limits = c(0, 2.5), breaks = seq(0, 2.5, .5)) +
    theme_bw(base_size = 20) +
    theme(legend.title = element_blank())
  
  if(is.null(ylab)|ylab == ""){
    plot <- plot +
      theme(axis.title.y = element_blank())
  } else if (ylab != "default"){
    plot <- plot +
      ylab(ylab)
  }
  return(plot)
  
}

##########################################

###############################
#### Missing Data and ICCs ####

## Summarizes the missingness at each wave after removing attrition cases
get_miss_summary <- function(data, wave){
  surveyvar <- paste0("Took_Survey", wave)
  suffix <- paste0("_W", wave)
  
  data <- data[data[[surveyvar]] == 1, ]
  ms <- data %>%
    select(subjno:Race, ends_with(suffix)) %>%
    miss_summary()
  
  return(ms)
  
}

## Calculates ICC at student and school level
scale_icc <- function(data, scale, logistic = FALSE, lgm = FALSE){
  
  data <- data %>% 
    select(subjno, SCHOOL_ID_W1, matches(paste0(scale,"_scale_W[1-4]$"))) %>%
    gather("Wave", "Value", -subjno, -SCHOOL_ID_W1) %>%
    mutate(SCHOOL_ID_W1 = as.character(SCHOOL_ID_W1))
  
  if(lgm == FALSE){
    if(logistic == FALSE){
      mod <- lme4::lmer(Value ~ 1 + (1|SCHOOL_ID_W1/subjno), data = data)
    } else {
      mod <- lme4::glmer(Value ~ 1 + (1|SCHOOL_ID_W1/subjno), data = data, family = binomial())
    }
    icc <- performance::icc(mod, by_group = TRUE)
    
    return(icc)
  } else {
    if(logistic == FALSE){
      mod <- lme4::lmer(Value ~ 1 + Wave + (1 + Wave|SCHOOL_ID_W1/subjno), data = data)
    } else {
      mod <- lme4::glmer(Value ~ 1 + Wave + (1 + Wave|SCHOOL_ID_W1/subjno), data = data, family = binomial())
    }
    return(mod)
  }
  
}

###############################################

#######################################
####    Latent Variable Models     ####

#### Run CFA separately by wave ####
run_cfa <- function(data, inames, fname, waves, missing, cluster){
  
  mod <- paste(fname, " =~ ", paste(inames, collapse = " + "))
  
  cfas <- map(.x = waves,
              ~data %>%
                filter(Wave == .x) %>%
                cfa(mod = mod, data = ., ordered = inames,
                    missing = missing, cluster = cluster,
                    parameterization = "theta"))
  
  return(cfas)
  
}


#### Extract model information and fit statistics ####
get_lavaan_fits <- function(object, measures = "scaled"){
  
  if(length(measures) > 1){
    
    indices <- measures
    
  } else if(measures == "scaled"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled','srmr')
    
  } else if(measures == "robust"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.robust', 'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr')
    
  } else if(measures == "naive"){
    
    indices <- c('npar', 'chisq', 'df', 'pvalue',
                 'cfi','rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')
  }
  
  
  
  fits <- as.data.frame(t(c(lavaan::fitMeasures(object, fit.measures = indices))))
  
  fits <- fits %>% mutate(ntotal = lavaan::inspect(object, "ntotal")) %>%
    select(ntotal, everything())
  
  if(!is.na(object@loglik$loglik)){
    
    fits <- fits %>%
      mutate(AIC = round(AIC(object), 1),
             BIC = round(BIC(object), 1))
    
  }
  
  return(fits)
}


#### gathering fit of multiple models into a single dataframe ####
fits_wrapper <- function(mod.list, .id = "Wave", measures = "scaled", digits = 2){
  
  fits <- map_dfr(.x = mod.list,
                  ~get_lavaan_fits(.x, measures = measures) %>%
                    round(., digits), .id = .id)
  
  if(measures == "scaled"){
    fits <- fits %>%
      mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.scaled, nsmall = digits),
                             ", ", format(rmsea.ci.upper.scaled, nsmall = digits), "]"))
  names(fits) <- str_remove(names(fits), "\\.scaled")
  } else if(measures == "robust"){
    fits <- fits %>%
      mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.robust, nsmall = digits),
                               ", ", format(rmsea.ci.upper.robust, nsmall = digits), "]"))
    names(fits) <- str_remove(names(fits), "\\.robust")
    names(fits) <- str_remove(names(fits), "\\.scaled")
    
  } else {stop("need to update function")}
  
  fits <- fits %>%
    select(one_of(.id), ntotal, npar, chisq, df, pvalue, cfi, rmsea, rmsea.ci, srmr)
  
  return(fits)
}

#### Estimating model-based reliability ####
get_relis <- function(object, wave){

  relis <- suppressWarnings(semTools::reliability(object))
  
  df <- data.frame(wave = wave,
                   alpha = relis[[2]],
                   omega = relis[[5]])
  return(df)
}


#### Extract standardized or unstandardized parameter estimates ####
extract_lavaan_parameters <- function(object, std = "no", params = "all", ...){
  
  if("all" %in% params){
    
    params = c("=~", "~~", "~*~", "~1", "~", "|", "<~")
    
  }
  
  if(std == "no"){
    
    TheParams <- parameterEstimates(object, standardized = FALSE, ...) %>%
      filter(op %in% params)
    
  } else if(std %in% c("std.all", "std.lv", "std.nox")){
    
    TheParams <- standardizedSolution(object, type = std) %>%
      filter(op %in% params)
    
  } else {
    stop("std argument must be 'no' for unstandardized estimates or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }
  
  
  return(TheParams)
}


#### Write base measurement invariance model ####
# list = see longIndNames argument of semTools::measEq.syntax
write_mi_mod <- function(list, fname, waves){
  
  bywave <- lapply(1:waves, function(x){
    paste0(fname, x," =~ ",
           paste(
             sapply(list, '[', x),  # extracts the item for each wave
             collapse = " + "))     # produces factor model for each wave
  })
  
  # could add if statement here to output each wave without collapsing
  model <- paste(bywave, collapse = "\n") # collapses across waves
}


#### generate measurement invariance syntax and fit model ####
# Other options exist for both syntax (semTools::measEq.syntax) and model (lavaan::lavaan)
mi_wrapper <- function(model, data, items, fnames,
                       ID.fac = "auto.fix.first",
                       ID.cat = "millsap",
                       ID.thr = c(1L, 2L),
                       long.equal = "",
                       auto = "all",
                       group = NULL,
                       group.equal = "",
                       ordered = unlist(items, use.names = FALSE),
                       parameterization = "theta",
                       meanstructure = TRUE,
                       mimic = "Mplus",
                       estimator = "WLSMV",
                       missing = "pairwise",
                       cluster = NULL,
                       produce = "both", ...){ # other options are "syntax", "fit"
  
  lavaan.params <- list(data = data,
                        ordered = ordered,
                        parameterization = parameterization,
                        meanstructure = meanstructure,
                        mimic = mimic,
                        estimator = estimator,
                        missing = missing,
                        cluster = cluster,
                        group = group,
                        ...)
  
  if(produce %in% c("both", "syntax")){
  
  syntax.params <- list(ID.fac = ID.fac,
                        ID.cat = ID.cat,
                        longFacNames = fnames,
                        longIndNames = items,
                        long.equal = long.equal,
                        auto = auto,
                        group.equal = group.equal)
  
  }
  
  
  if(produce == "syntax"){
    
    output <- do.call(measEq.syntax, c(list(configural.model = model), lavaan.params, syntax.params))
    
  } else if(produce == "fit"){
    
    output <- do.call(lavaan, c(list(model = model), lavaan.params))
    
  } else if(produce == "both"){
    
    syntax <- do.call(measEq.syntax, c(list(model), lavaan.params, syntax.params))
    fit <- do.call(lavaan, c(list(as.character(syntax)), lavaan.params))
    
    output <- list(syntax = syntax,
                   fit = fit)
  } else {
    stop("produce argument must be 'both', 'syntax', or 'fit'")
  }
  
  return(output)
  
}


#### Likelihood ratio test comparing measurement invariance models ####
compare_mods <- function(mod0, mod1){
  
  aov01 <- lavTestLRT(mod0, mod1)[2,c(5:7)]
  
  ind0 <- get_lavaan_fits(mod0, measures = c("cfi", "rmsea", "srmr"))
  ind1 <- get_lavaan_fits(mod1, measures = c("cfi", "rmsea", "srmr"))
  ind.diff <- ind1 - ind0
  
  mi.com <- format(round(cbind(aov01, ind.diff[2:4]), 3), nsmall = 3)
  row.names(mi.com) <- NULL
  names(mi.com) <- c("delta.chisq", "delta.df", "delta.pvalue", "delta.cfi", "delta.rmsea", "delta.srmr")
  
  return(mi.com)
}


#### write syntax to estimate covariance between model predictors ####
write_covs <- function(vnames){
  
  syntax <- character(0)
  for (f in seq_along(vnames)) {
    syntax <- c(syntax, paste0(vnames[[f]], " ~~ ", vnames[(f+1):length(vnames)]))
    if (f == length(vnames)-1) break
  }
  syntax <- paste(syntax[!str_detect("NA", syntax)], collapse = "\n")
  syntax
}


#### Add latent growth model syntax to the strict measurement invariance syntax ####
add_lgm <- function(strict.syntax, fname, equal.fo.var = FALSE, quadratic = FALSE){
  
  ## Update strict.syntax
  
  # constrain first order LV covariances to 0
  oldpsi <- strict.syntax@values$`1`$psi
  newpsi <- diag(ncol(oldpsi))
  diag(newpsi) <- NA
  dimnames(newpsi) <- dimnames(oldpsi)
  class(newpsi) <- class(oldpsi)
  
  strict.syntax@values$`1`$psi <- newpsi
  
  # constrain first order LV variances to equality
  if(equal.fo.var == TRUE){
    
    diag(strict.syntax@labels$`1`$psi) <- "psi.1_1"
  }
  
  # constrain first order LV means to 0
  oldalpha <- strict.syntax@values$`1`$alpha
  newalpha <- matrix(0, nrow(oldalpha), ncol = 1)
  dimnames(newalpha) <- dimnames(oldalpha)
  class(newalpha) <- class(oldalpha)
  
  strict.syntax@values$`1`$alpha <- newalpha
  
  ## Add latent growth model syntax
  
  # a) define LGM,
  # b) estimate latent intercept and slope mean, variance, and covariance, (Note: intercept mean must be 0 to identify model with ordinal indicators (Liu and West, 2018))
  # c) constrain covariances of first order and second order (e.g. intercept and slope) LVs to 0
  lgm.syntax <- "\n## LATENT GROWTH\n\nInt =~ 1*xyz1 + 1*xyz2 + 1*xyz3 + 1*xyz4\nSlope =~ 0*xyz1 + .5*xyz2 + 1.5*xyz3 + 2.5*xyz4\n
## MEAN STRUCTURE\n\nInt ~ 0*1\nSlope ~ 1\n
## VARIANCES AND COVARIANCES\n\nInt ~~ Int\nSlope ~~ Slope\nInt ~~ Slope\nInt ~~ 0*xyz1 + 0*xyz2 + 0*xyz3 + 0*xyz4\nSlope ~~ 0*xyz1 + 0*xyz2 + 0*xyz3 + 0*xyz4\n"
  
  if(quadratic == TRUE){
    lgm.syntax <- paste(lgm.syntax,
                        "\n## Quadratic\n\nQuad =~ 0*xyz1 + 1*xyz2 + 4*xyz3 + 9*xyz4
                        Quad ~ 1\nQuad ~~ Quad\nQuad ~~ Int\nQuad ~~ Slope
                        Quad ~~ 0*xyz1 + 0*xyz2 + 0*xyz3 +0*xyz4")
    
  }
  
  # replace placeholder with first order LV name used in strict.syntax
  lgm.syntax <- str_replace_all(lgm.syntax, "xyz", fname)
  
  # add lgm to strict.syntax
  syn <- paste0(as.character(strict.syntax), lgm.syntax)
  
  return(syn)
}

#### add regression syntax to existing syntax ####
# syntax = syntax used in latent growth models
add_reg <- function(syntax, tin.cov = NULL, tv.cov = NULL, fname, waves){
  
  if(!is.null(tin.cov)){
    tin.cov <- paste(tin.cov, collapse = " + ")
    
    tin.syn <- paste0("## TIME-INVARIANT REGRESSIONS\n\nInt ~ ", tin.cov,
                      "\nSlope ~ ", tin.cov)
    
    syntax <- paste(syntax, tin.syn, sep = "\n")
  }
  
  if(!is.null(tv.cov)){
    
    tv.cov <- lapply(waves,
                     function(x) paste(paste0(tv.cov, x),
                                       collapse = " + "))
    
    tv.syn <- paste("\n\n## TIME-VARIANT REGRESSIONS",
                    paste(map2(.x = waves, .y = tv.cov,
                               ~paste0(fname, .x, " ~ ", .y)), collapse = "\n"),
                    sep = "\n\n")
    
    syntax <- paste(syntax, tv.syn, sep = "\n")
  }
  
  return(syntax)
}

#### Converting multigroup latent growth estimates to predicted values ####
predict_growth <- function(object, lavaan = TRUE){
  
  if(lavaan == TRUE){
  object %>%
    filter(op == "~1") %>%
    mutate(interim = est - ci.lower) %>% #check = ci.upper - est
    select(lhs, group, est, interim) %>%
    gather(variable, value, est, interim) %>%
    unite(col = "temp", lhs, variable) %>%
    spread(temp, value) %>%
    mutate(Wave0 = Int_est,
           Wave.5 = Int_est + .5*Slope_est,
           Wave1.5 = Int_est + 1.5*Slope_est,
           Wave2.5 = Int_est + 2.5*Slope_est) %>%
    gather(Wave, Estimate, starts_with("Wave"))
    
  } else{
    object %>%
      filter(paramHeader == "Means") %>%
      mutate(interim = est - low2.5) %>%
      select(Group, param, est, interim) %>%
      gather(variable, value, est, interim) %>%
      unite(col = "temp", param, variable) %>%
      spread(temp, value) %>%
      mutate(Wave0 = I_est,
             Wave.5 = I_est + .5*S_est,
             Wave1.5= I_est + 1.5*S_est,
             Wave2.5 = I_est + 2.5*S_est) %>%
      gather(Wave, Estimate, starts_with("Wave"))
  }
}

###########################################################

#######################################
####    Formatting Functions       ####
#
format_param_table <- function(table, recodes){
  
  table %>%
    unite(col = "parameter", lhs, op, rhs, sep = " ", remove = FALSE) %>%
    mutate(group = as_factor(group) %>% fct_recode(!!!recodes),
           ci = paste0("[", format(round(ci.lower, 2), nsmall = 2),
                       ", ", format(round(ci.upper, 2), nsmall = 2), "]")) %>%
    select(parameter, group, est.std, ci, se, z, pvalue, ci.lower, ci.upper, lhs, rhs) %>%
    arrange(parameter)
}

get_est <- function(estimates){
  est <- estimates %>%
    mutate(parameter = paste(lhs, op, rhs) %>%
             str_replace_all(., " ~~ ", "_") %>%
             str_replace_all(., " ~1 ", ""),
           sig = ifelse(is.na(pvalue), " ", ifelse(pvalue < .05, "*", " "))) %>%
    mutate(across(where(is.numeric), ~format(round(., 2), nsmall = 2))) %>%
    mutate(estimate = paste0(est, " (", se, ")", sig))
  return(est)
  
}

mplus_fit <- function(object, digits = 2, format = TRUE){
  
  fit <- object$summaries %>%
                   select(ntotal = Observations, npar = Parameters, chisq = ChiSqM_Value, df = ChiSqM_DF, pvalue = ChiSqM_PValue,
                          cfi = CFI, rmsea = RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, srmr = SRMR) %>%
    mutate(rmsea.ci =  paste0("[", format(round(RMSEA_90CI_LB, digits), nsmall = digits),
                              ", ", format(round(RMSEA_90CI_UB, digits), nsmall = digits), "]")) %>%
    select(ntotal:rmsea, rmsea.ci, srmr)
  
  if(format == TRUE){
    fit <- fit %>%
      mutate(across(c(chisq, pvalue, cfi, rmsea, srmr), ~format(round(.x, digits), nsmall = digits)))
  }
  return(fit)
}

mplus_est <- function(object, params = "all",
                      std = c("unstandardized", "stdyx.standardized", "stdy.standardized"),
                       combine = TRUE, alpha = .05, digits = 2, ci = FALSE){
  
  if("all" %in% params){
    
    params = c("|", "BY", "ON", "WITH", "Means", "Intercepts", "Variances") # add more as needed
    
  }
  params <- ifelse(str_detect(params, "\\|"), "[|]", params)
  params = paste(params, collapse = "|")
  
  est <- object$parameters[[std]] %>%
    filter(str_detect(paramHeader, params))
  

  if(combine == TRUE){
    est <- est %>%
      mutate(parameter = paste(paramHeader, param, sep = "_"),
             estimate = paste0(format(round(est, digits), nsmall = digits),
                               " (", format(round(se, digits), nsmall = digits), ")"),
             estimate = case_when(pval < .01 ~ paste0(estimate, "**"),
                                  pval < alpha ~ paste0(estimate, "*"),
                                  TRUE ~ estimate))
  }
  
  if(ci == TRUE){
    
    cis <- object$parameters[[paste0("ci.", std)]] %>%
      filter(str_detect(paramHeader, params))
    est <- left_join(est, cis)
    
    if(combine == TRUE){
      est <- est %>%
        mutate(ci.95 = paste0("[", format(round(low2.5, digits), nsmall = digits),
                              ", ", format(round(up2.5, digits), nsmall = digits), "]"))
    }
  }
  return(est)
}


## Extracts random effect and R2 for intercept and slope factor
mplus_rer2 <- function(object){
  rer2 <- mplus_est(object, std = "unstandardized", params = "Residual.Variances",
                    digits = 2, combine = TRUE, ci = FALSE) %>%
    bind_rows(object$parameters$r2 %>%
                mutate(estimate = paste0(format(round(est, 2), nsmall = 2),
                                         " (", format(round(se, 2), nsmall = 2), ")"))) %>%
    filter(param %in% c("I", "S")) %>%
    mutate(predictor = c(rep("Variance", 2), rep("R2", 2)) %>% forcats::as_factor()) %>%
    select(predictor, param, estimate) %>%
    spread(param, estimate)
  return(rer2)
}

mplus_sb2001 <- function(m0, m1, digits = 2){
  
  # https://statmodel.com/chidiff.shtml
  d0 <- m0$summaries$ChiSqM_DF
  c0 <- m0$summaries$ChiSqM_ScalingCorrection
  T0 <- m0$summaries$ChiSqM_Value
  d1 <- m1$summaries$ChiSqM_DF
  c1 <- m1$summaries$ChiSqM_ScalingCorrection
  T1 <- m1$summaries$ChiSqM_Value
  
  cd <- (d0 * c0 - d1 * c1) / (d0 - d1)
  
  Td <- (T0 * c0 - T1 * c1) / cd
  df <- d0 - d1
  p <- pchisq(Td, df, lower.tail = FALSE)
  
  # fit index differences
  cfi <- m0$summaries$CFI - m1$summaries$CFI
  rmsea <- m0$summaries$RMSEA_Estimate - m1$summaries$RMSEA_Estimate
  srmr <- m0$summaries$SRMR - m1$summaries$SRMR
  
  out <- data.frame(delta.x2 = Td,
                    delta.df = df,
                    delta.p = p,
                    delta.cfi = cfi,
                    delta.rmsea = rmsea,
                    delta.srmr = srmr) %>%
    mutate(across(-delta.df, ~format(round(.x, digits), nsmall = digits)))
  return(out)
}


format_mplus <- function(object, recodes, outcomes,
                         std = c("unstandardized", "stdyx.standardized", "stdy.standardized"),
                         reg = TRUE, r2 = TRUE, digits = 2){

  fit <- map_dfr(.x = object,
                 ~mplus_fit(.x, digits = 2, format = TRUE)) %>%
    mutate(Outcome = outcomes) %>%
    select(Outcome, everything())
  
  if(reg == TRUE){
  estimates <- map(.x = object, ~.x$parameters[[std]] %>%
                     filter(str_detect(paramHeader, "\\.ON") | (paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
                     mutate(is = str_remove(paramHeader, ".\\ON") %>% ifelse(. == "Intercepts", param, .),
                            predictor = recode(param, !!!recodes) %>%
                              factor(., levels = recodes[-1]),  # specific to latent growth models where I and S recoded to same value
                            estimate = paste0(format(round(est, digits), nsmall = digits),
                                              " (", format(round(se, digits), nsmall = digits), ")"),
                            estimate = case_when(pval < .01 ~ paste0(estimate, "**"),
                                                 pval < .05 ~ paste0(estimate, "*"),
                                                 TRUE ~ estimate)) %>%
                     select(predictor, is, estimate) %>%
                     spread(is, estimate)) %>%
    set_names(outcomes)
  } else{
    estimates <- map2_dfr(.x = object, .y = outcomes,
                          ~.x$parameters[[std]] %>%
                           filter(paramHeader %in% c("Means", "Variances", "S.WITH")) %>%
                           mutate(Outcome = .y,
                                  param = paste(param, paramHeader, sep = "_"),
                                  estimate = paste0(format(round(est, digits), nsmall = digits),
                                                     " (", format(round(se, digits), nsmall = digits), ")") %>%
                                     ifelse(pval < .05, paste0(., "*"), .)))
    if("Group" %in% names(estimates)){
      estimates <- estimates %>%
        select(Outcome, Group, param, estimate) %>%
        spread(param, estimate)
    } else {
    estimates <- estimates %>%
      select(Outcome, param, estimate) %>%
      spread(param, estimate)
    }
  }
  
  warnerr <- map2_dfr(.x = object, .y = outcomes,
                      ~data.frame(outcome = .y,
                                  errors = length(.x$errors),
                                  warnings = length(.x$warnings)))
  

  if(r2 == TRUE){
    r2 <- map2_dfr(.x = object, .y = outcomes,
                   ~.x$parameters$r2 %>% mutate(outcome = .y)) %>%
      mutate(across(where(is.numeric), ~format(round(.x, digits), nsmall = digits)))

      return(list(Model_Fit = fit,
              Estimates = estimates,
              R2 = r2,
              Warn_Error = warnerr))
  } else{
    
    return(list(Model_Fit = fit,
                Estimates = estimates,
                Warn_Error = warnerr))
  }
}

#### Interaction functions are not at all generalizable; very project specific ####
intx_dat_shortcut <- function(object, std = "unstandardized", grades = TRUE){
  
  # Calculates the mean and Mean +1 SD (High) value of each hostile home environment variable
  # Creates 3 (high, mean, none) x 3 (abuse, famcon, sibagg) dataframe
  acevals <- object %>%
    mplus_est(params = c("Means", "Variances"), std = std, digits = 3) %>%
    filter(param %in% c("FAMCON", "ABUSE", "SIBAGG")) %>%
    select(paramHeader, param, est) %>%
    spread(paramHeader, est) %>%
    mutate(High = Means + sqrt(Variances),
           None = 0) %>%
    select(-Variances) %>%
    gather(Level, value, -param) %>%
    spread(param, value)
  
  # binds parameter estimates of main effects and interaction effects onto acevals
  aceparam <- object %>%
    mplus_est(params = c("ON", "Intercepts"), std = std, digits = 3) %>%
    filter(str_detect(paramHeader, "\\.ON")|(paramHeader == "Intercepts" & param %in% c("I", "S"))) %>%
    select(parameter, est) %>%
    spread(parameter, est) %>%
    bind_rows(replicate(nrow(acevals) - 1, ., simplify = FALSE)) %>%
    bind_cols(acevals, .)
  
  # calculates predicted intercept and slope for each HSB by hostile home environment value
  hsb.vals = c(1,0)
  hsb <- bind_rows(replicate(length(hsb.vals), aceparam, simplify = FALSE)) %>%
    bind_cols(., data.frame(HSB = rep(hsb.vals, each = 3))) %>%
    mutate(Level = recode(Level, Means = "Mean"),
           ABx = ABUSE*HSB,
           FCx = FAMCON*HSB,
           SAx = SIBAGG*HSB,
           I.Abuse = Intercepts_I + I.ON_ABUSE*ABUSE + I.ON_SCHBEL*HSB + I.ON_SBXAB*ABx,
           S.Abuse = Intercepts_S + S.ON_ABUSE*ABUSE + S.ON_SCHBEL*HSB + S.ON_SBXAB*ABx,
           I.Family_Conflict = Intercepts_I + I.ON_FAMCON*FAMCON + I.ON_SCHBEL*HSB + I.ON_SBXFC*FCx,
           S.Family_Conflict = Intercepts_S + S.ON_FAMCON*FAMCON + S.ON_SCHBEL*HSB + S.ON_SBXFC*FCx,
           I.Sibling_Aggression = Intercepts_I + I.ON_SIBAGG*SIBAGG + I.ON_SCHBEL*HSB + I.ON_SBXSA*SAx,
           S.Sibling_Aggression = Intercepts_S + S.ON_SIBAGG*SIBAGG + S.ON_SCHBEL*HSB + S.ON_SBXSA*SAx)
  
  if(grades == TRUE){
  # ag.vals <- object %>%
  #   mplus_est(params = c("Means", "Variances"), std = "unstandardized", digits = 3) %>%
  #   filter(param %in% c("GRADES")) %>%
  #   select(paramHeader, param, est) %>%
  #   spread(paramHeader, est) %>%
  #   mutate(High = 6,
  #          Low = Means - sqrt(Variances)) %>%
  #   select(-Variances) %>%
  #   gather(Grade_Cat, value, -param) %>%
  #   spread(param, value)
  # 
  ag.vals <- data.frame(Grade_Cat = c("Mostly A's (90-100)", "Mostly B's (80-84)", "Mostly C's (70-74)"),
                        GRADES = c(6, 4, 2))
  
  # calculates predicted intercept and slope for each academic grade by hostile home environment value
  ag <- bind_rows(replicate(nrow(ag.vals), aceparam, simplify = FALSE)) %>%
    bind_cols(., uncount(mutate(ag.vals, n = nrow(aceparam)), n)) %>%
    mutate(Level = recode(Level, Means = "Mean"),
           ABx = ABUSE*GRADES,
           FCx = FAMCON*GRADES,
           SAx = SIBAGG*GRADES,
           I.Abuse = Intercepts_I + I.ON_ABUSE*ABUSE + I.ON_GRADES*GRADES + I.ON_AGXAB*ABx,
           S.Abuse = Intercepts_S + S.ON_ABUSE*ABUSE + S.ON_GRADES*GRADES + S.ON_AGXAB*ABx,
           I.Family_Conflict = Intercepts_I + I.ON_FAMCON*FAMCON + I.ON_GRADES*GRADES + I.ON_AGXFC*FCx,
           S.Family_Conflict = Intercepts_S + S.ON_FAMCON*FAMCON + S.ON_GRADES*GRADES + S.ON_AGXFC*FCx,
           I.Sibling_Aggression = Intercepts_I + I.ON_SIBAGG*SIBAGG + I.ON_GRADES*GRADES + I.ON_AGXSA*SAx,
           S.Sibling_Aggression = Intercepts_S + S.ON_SIBAGG*SIBAGG + S.ON_GRADES*GRADES + S.ON_AGXSA*SAx)
  
  return(list(HSB = hsb,
              AG = ag))
  } else {
    return(list(HSB = hsb))
    }
  
}

intx_plot_prep <- function(intxdata, var){
  
  var <- enquo(var)
  
  data <- intxdata %>% select(Level, !!var, I.Abuse:S.Sibling_Aggression) %>%
    gather(temp, value, -Level, -(!!var)) %>%
    separate(temp, into = c("IS", "ACE"), sep = "\\.") %>%
    spread(IS, value) %>%
    mutate(Wave0 = I,
           Wave.5 = I + .5*S,
           Wave1.5= I + 1.5*S,
           Wave2.5 = I + 2.5*S) %>%
    gather(Year, Estimate, starts_with("Wave")) %>%
    mutate(Year = str_remove(Year, "Wave") %>% as.numeric())
  
  return(data)
}


############################################################

# ---- Defining sets of items ------------------------

# Note: variable names based on long format data; for wide format, paste "_W" and the waves of interest

## IDs
ID_items <- c("subjno", "Gender", "Race", "SCHOOL_ID")

## ACEs
fv_items <- paste0("FAM_VIOL", c(1:3))          # Family violence
ab_items <- paste0("ABUSE", c(1:3))             # Abuse
sib_items <- c("SIB_AGG_VICT4","SIB_AGG_VICT5") # Sibling Aggression; Note: only using most severe items

## Outcome - Sexual Harassment
w1sh_items <- c(paste0("COMM_SH_PERP", c(1:12)), paste0("SCH_SH_PERP", c(1:12)))
sh_items <- paste0("SH_PERP", c(1:12))

## Mediators
dep_items <- paste0("DEPRESSION", c(1:9)) # Depression/Anxiety
del_items <- paste0("DELINQ", c(1:7))     # Delinquent/anti-social behaviors
emp_items <- paste0("EMPATHY", c(1:5))    # Empathy
su_items <- c("BEER","WINE","CIGARETTE","DRUNK","LIQUOR","CANNABIS","INHALANT","ILLICIT")

## Moderators
schb_items <- paste0("SCH_BEL", c(1:4)) # School Belonging; Note: item 5 dropped after W1
vict_items <- paste0("LT_SH_VICT", c(1:4, 6,7))
