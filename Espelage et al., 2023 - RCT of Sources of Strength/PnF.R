############################
#                          #
#  Packages and Functions  #
#                          #
############################


#### Packages ####
library(dplyr)
library(haven)
library(stringr)
library(ggplot2)
library(tidyr)
library(forcats)
library(lavaan)
library(purrr)
library(naniar)
library(semTools)
library(lme4)
library(cobalt)
library(gtsummary)
library(flextable)


#---------- Functions ----------------------------------------------

#### Shortcut for converting data from wide to long format by wave ####
# put between subject variables in ...
LongWave <- function(data, wave, ..., prefix = "_W"){
  
  wave.chr <- paste0(prefix, wave)
  
  btsubvars <- quos(...)
  
  longdat <- data %>%
    select(!!!btsubvars, ends_with(wave.chr)) %>%
    rename_at(vars(ends_with(wave.chr)), ~str_remove(., wave.chr)) %>%
    mutate(Wave = wave)
  
  return(longdat)
}


#############################
####     Frequencies     ####

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

Get_DemoFreqs <- function(data, prefix, digits = 1){
  
  demos <-  data %>%
    select(StudentID, Grade, Transgender, Gender, Race, SexOr) %>%
    mutate(across(one_of(DemoVars), as_factor)) %>%
    gather(Demo, Group, -StudentID) %>%
    group_by(Demo, Group) %>%
    summarize(n = n(), .groups = "drop_last") %>%
    mutate(Total = sum(n),
           Percent = format(round(n / Total * 100, digits = digits), nsmall = digits)) %>%
    rename_with(~paste(prefix, ., sep = "_"), c(n, Total, Percent))
  
}

#### Frequency and Proportion for a single variable or crosstab ####
## Could remove dplyr references
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

#### Scale Score Function ####
#' @param data a \code{data.frame}
#' @param items a character vector of column names
#' @param type should the score be the sum or the mean of item responses?
#' @param min.valid the minimum number of valid responses to receive a score.
#'
#' @example 
#' scale_score(myData, items = c("Item1", "Item2", "Item3", "Item4", "Item5"), type = "sum", min.valid = 1)
scale_score <- function(data, items, type = c("sum", "mean"), min.valid = NULL){
  
  
  if(type == "sum"){
    score <- rowSums(data[, items], na.rm = TRUE)
  } else if(type == "mean"){
    score <- rowMeans(data[, items], na.rm = TRUE)
  }
  
  nacount <- rowSums(is.na(data[, items]), na.rm = TRUE)
  
  if(!is.null(min.valid)){
    score <- ifelse((length(items) - nacount) < min.valid, NA, score)
  } else {
    score <- ifelse(nacount == length(items), NA, score)
  }
  
  return(score)
}

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


##############################################

######################
#### Missing Data ####


lollipop_missing <- function(data){
  
  plot <- ggplot(data = data, aes(x = pct_cases, y = n_miss, group = Wave, color = Wave)) +
    geom_point(position = position_dodge(1)) + 
    geom_linerange(aes(xmin = 0, xmax = pct_cases),
                   position = position_dodge(1)) +
    geom_text(aes(label = n_cases, x = pct_cases + 5),
              position = position_dodge(1)) +
    scale_x_continuous(name = "% of Students", breaks = seq(0, 100, 20)) +
    ylab("# of missing items") +
    theme_bw(base_size = 18)
}

# data - data in long format (one row for each student x wave combination)
# scale - character vector of item names. For multiple scales, provide a list of character vectors
# scale.names - For multiple scales, supply the names of each scale. length(scale.names) must equal length(scale).
missing_data_wrapper <- function(data, scale, scale.names){
  
  if(is.list(scale)){
    
    Data <- map2_dfr(.x = scale,
                     .y = scale.names,
                     ~data %>%
                       select(Wave, Tx, one_of(.x)) %>%
                       group_by(Wave, Tx) %>%
                       miss_case_table() %>%
                       mutate(Item = .y))
    # Plot
    Plot <- Data %>%
      mutate(n_miss = factor(n_miss_in_case, levels = c(0:length(scale[[1]]))),
             Wave = as_factor(Wave),
             Tx = recode(Tx, `0` = "WL", `1` = "SoS")) %>%
      lollipop_missing() +
      facet_grid(Tx ~ Item)
    
    
  } else {
    
    Data <- data %>%
      select(Wave, Tx, one_of(scale)) %>%
      group_by(Wave, Tx) %>%
      miss_case_table()
    
    # Plot
    Plot <- Data %>%
      mutate(n_miss = factor(n_miss_in_case, levels = c(0:length(scale))),
             Wave = as_factor(Wave),
             Tx = recode(Tx, `0` = "WL", `1` = "SoS")) %>%
      lollipop_missing() +
      facet_grid(Tx ~ .)
    
  }
  
  output <- list(Data = Data,
                 Plot = Plot)
  
  return(output)
}



#####################################

#' ###################################
#' ####  Functions for EFA_CFA.R  ####
#' 
#' #' Converts EFA results into CFA syntax
#' #'
#' #' Uses the matrix of loadings from an EFA to generate \code{lavaan} compatible
#' #' CFA syntax.
#' #'
#' #' @param loadings matrix of factor loadings
#' #' @param simple logical; Should the simple structure be returned (default)?
#' #' If \code{FALSE}, items can cross load on multiple factors.
#' #' @param threshold numeric between 0 and 1 indicating the minimum (absolute) value
#' #' of the loading for an item on a factor.
#' #' @param single.item character indicating how single-item factors should be treated.
#' #' Use "keep" to keep them in the model when generating the CFA syntax, "drop"
#' #' to remove them, or "" indicating the CFA syntax should not be generated for this model.
#' #'
#' #' @return character. Use \code{cat} to best examine the returned syntax.
#' efa_cfa_syntax <- function(loadings, simple = TRUE, threshold = NA,
#'                            single.item = c("keep","drop", "")){
#'   if(simple == FALSE & is.na(threshold)){
#'     stop("threshold must be supplied when simple = FALSE")
#'   }
#'   # item and factor names
#'   if(is.null(dimnames(loadings))){
#'     inames <- paste0("i", 1:dim(loadings)[[1]]) # item
#'     fnames <- paste0("f", 1:dim(loadings)[[2]]) # factor
#'   } else {
#'     inames <- dimnames(loadings)[[1]] # item
#'     fnames <- dimnames(loadings)[[2]] # factor
#'   }
#'   if(simple == TRUE){
#'     # largest (absolute) loading for each item
#'     maxload <- apply(abs(loadings), 1, max)
#'   } else {
#'     maxload <- c(rep(NA, length(inames)))
#'   }
#'   # obtaining simple structure matrix with NAs elsewhere
#'   loadings.max <- loadings
#'   for(i in 1:length(inames)){
#'     thresh <- max(maxload[[i]], threshold, na.rm = TRUE)
#'     loadings.max[i, ][abs(loadings.max[i, ]) < thresh] <- NA
#'   }
#'   # returns vector with each element being the lavaan syntax identifying the factor
#'   cfa.syntax <- c()
#'   for(fn in 1:length(fnames)){
#'     cfa.syntax <- c(cfa.syntax,
#'                     paste0(fnames[[fn]], " =~ ",
#'                            paste(inames[!is.na(loadings.max[,fn])],
#'                                  collapse = " + ")))
#'   }
#'   # What to do with single item factors?
#'   if(length(single.item) > 1){
#'     single.item <- "keep"
#'   }
#'   if(single.item == "keep"){
#'     # final cfa syntax
#'     cfa.syntax <- paste(cfa.syntax, collapse = "\n")
#'   } else if(single.item == "drop"){
#'     # drops single item factors before collapsing to final syntax
#'     cfa.syntax <- cfa.syntax[nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,
#'                                                             fixed = TRUE)) > 0]
#'     cfa.syntax <- paste(cfa.syntax, collapse = "\n")
#'   } else if(single.item == ""){
#'     # check if they exist
#'     if(all(nchar(cfa.syntax) - nchar(gsub("+", "", cfa.syntax,fixed = TRUE)) > 0) == TRUE){
#'       cfa.syntax <- paste(cfa.syntax, collapse = "\n")
#'     } else {
#'       cfa.syntax <- ""
#'     }
#'   }
#'   return(cfa.syntax)
#' }
#' ## internal function for extracting standardized loadings
#' get_std_loadings <- function(object, type = "std.all"){
#'   
#'   # extracting unrotated standardized results
#'   params <- lavaan::standardizedsolution(object, type = type,
#'                                          se = FALSE, zstat = FALSE, # Not needed so saves
#'                                          pvalue = FALSE, ci = FALSE)# computation time
#'   loaddf <- params[params$op == "=~",]
#'   
#'   # loading matrix dimension names
#'   inames <- unique(loaddf$rhs) # item names
#'   fnames <- unique(loaddf$lhs) # factor names
#'   
#'   # matrix of standardized factor loadings
#'   loadmat <- matrix(loaddf$est.std,
#'                     ncol = length(fnames), nrow = length(inames),
#'                     byrow = FALSE, dimnames = list(inames, fnames))
#'   
#'   return(loadmat)
#' }

efa_cfa_prelim <- function(longdata, items, Wave = 1){
  
  if(is.null(Wave)){
    sample <- longdata
  } else{
    sample <- longdata %>%
      filter(Wave == Wave & rowSums(is.na(.[,items])) != length(items)) %>%
      select(StudentID, School, all_of(items))
  }
  
  ## polychoric correlation matrix
  cor <- lavaan::lavCor(sample[,items],
                        ordered = items,
                        missing = "pairwise",
                        output = "cor",
                        cor.smooth = FALSE)
  
  # optimal number of factors based on 18 criteria
  nf <- parameters::n_factors(x = sample[,items],
                              type = "FA",
                              rotation = "oblimin",
                              package = c("psych", "nFactors"),
                              cor = cor,
                              safe = TRUE)
  
  out <- list(sample = sample,
              cor = cor,
              nf = nf)
  
  return(out)
}


#########################################3


school_effects <- function(model){
  icc <- performance::icc(model)
  fe <- parameters::model_parameters(model)
  re <- parameters::random_parameters(model)
  # prf <- performance::model_performance(model)
  
  fe$Within_Variance = re$Value[[1]]
  fe$Between_Variance = re$Value[[2]]
  fe$ICC = icc[[1]]
  
  return(fe)
}

get_baltab <- function(baltab){
  tibble::rownames_to_column(baltab[[1]], "Scale") %>%
    filter(!str_detect(Scale, "\\:")) %>%
    select(Scale, Waitlist_M = M.0.Un, SoS_M = M.1.Un, Waitlist_SD = SD.0.Un, SoS_SD = SD.1.Un,
           d = Diff.Un, V_Ratio = V.Ratio.Un)
}

#######################################
####    Latent Variable Models     ####


#### Extract model information and fit statistics ####
Get_lavaan_fits <- function(object, measures = "scaled"){
  
  if(length(measures) > 1){
    
    indices <- measures
    
  } else if(measures == "scaled"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.scaled', 'tli.scaled', 'agfi',
                 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled','srmr')
    
  } else if(measures == "robust"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.robust', 'tli.robust', 'agfi',
                 'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr')
    
  } else if(measures == "naive"){
    
    indices <- c('npar', 'chisq', 'df', 'pvalue',
                 'cfi', 'tli', 'agfi',
                 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')
  }
  
  
  
  fits <- as.data.frame(t(c(lavaan::fitMeasures(object, fit.measures = indices))))
  
  fits <- fits %>% mutate(ntotal = lavaan::inspect(object, "ntotal"),
                          ngroups = lavaan::inspect(object, "ngroups")) %>%
    select(ntotal, ngroups, everything())
  
  if(!is.na(object@loglik$loglik)){
    
    fits <- fits %>%
      mutate(AIC = round(AIC(object), 1),
             BIC = round(BIC(object), 1))
    
  }
  
  return(fits)
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

#### Wrapper around above functions, specifically for CFAs ####
## Need to clean up this function for read/useability
cfa_wrapper <- function(data, items, fname,
                        mods = NULL,
                        ordered = items,
                        estimator = "WLSMV",
                        missing = "pairwise",
                        mimic = "lavaan",
                        cluster = NULL,
                        digits = 2,
                        waves = 1:4
                        ){
  if(!is.null(waves)){
    if(is.null(mods)){
      
      model <- paste0(fname, " =~ ",  paste(items, collapse = " + "))
      
      mods <- map(.x = waves, ~cfa(model = model,
                                 data = data[data$Wave == .x, ],
                                 ordered = ordered,
                                 estimator = estimator,
                                 missing = missing,
                                 cluster = cluster,
                                 mimic = mimic))
    }
    
    
    fits <- map_dfr(.x = mods,
                    ~Get_lavaan_fits(.x) %>%
                      round(., digits), .id = "Wave") %>%
      mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.scaled, nsmall = digits),
                               ", ", format(rmsea.ci.upper.scaled, nsmall = digits), "]"))
    names(fits) <- str_remove(names(fits), "\\.scaled")
    
    fits <- fits %>%
      select(Wave, ntotal, npar, chisq, df, pvalue, cfi, tli, rmsea, rmsea.ci, srmr)
    
    relis <- map_dfr(.x = mods,
                     ~semTools::reliability(.x) %>%
                       round(., digits) %>%
                       t() %>% data.frame(),
                     .id = "Wave") %>%
      select(Wave, alpha, omega_h = omega3)
    rownames(relis) <- NULL
    
    loadings <- map_dfr(.x = mods,
                        ~extract_lavaan_parameters(.x, std = "std.all", params = "=~") %>%
                          select(Item = rhs, est.std), .id = "Wave") %>%
      mutate(est.std = format(round(est.std, digits), nsmall = digits),
             Wave = paste0("W", Wave)) %>%
      spread(Wave, est.std)
    
    if(is.null(mods)){
      
      output <- list(model = model,
                     cfas = mods,
                     fits = fits,
                     reliabilities = relis,
                     loadings = loadings)
      
    } else {
      
      output <- list(fits = fits,
                     reliabilities = relis,
                     loadings = loadings)
      
      
    }
  } else { # single wave
    
    model <- paste0(fname, " =~ ",  paste(items, collapse = " + "))
    
    mod <-cfa(model = model,
              data = data,
              ordered = ordered,
              estimator = estimator,
              missing = missing,
              cluster = cluster,
              mimic = mimic)
    
    fits <- Get_lavaan_fits(mod) %>%
      round(., digits) %>%
      mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.scaled, nsmall = digits),
                               ", ", format(rmsea.ci.upper.scaled, nsmall = digits), "]"))
    names(fits) <- str_remove(names(fits), "\\.scaled")
    
    fits <- fits %>%
      select(ntotal, npar, chisq, df, pvalue, cfi, tli, rmsea, rmsea.ci, srmr)
    
    relis <- semTools::reliability(mod) %>%
      round(., digits) %>%
      t() %>% data.frame()%>%
      select(alpha, omega_h = omega3)
    rownames(relis) <- NULL
    
    loadings <- extract_lavaan_parameters(mod, std = "std.all", params = "=~") %>%
      select(Item = rhs, est.std) %>%
      mutate(est.std = format(round(est.std, digits), nsmall = digits))
    
    output <- list(model = model,
                   cfa = mod,
                   fits = fits,
                   reliabilities = relis,
                   loadings = loadings)
    
  }
  
  return(output)
  
}

#### gathering fit of multiple models into a single dataframe ####
fits_wrapper <- function(mod.list, .id = "Wave", measures = "scaled", digits = 2){
  
  fits <- map_dfr(.x = mod.list,
                  ~Get_lavaan_fits(.x, measures = measures) %>%
                    round(., digits), .id = .id)
  
  if("rmsea.ci.lower.scaled" %in% names(fits)){
    fits <- fits %>%
      mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.scaled, nsmall = digits),
                               ", ", format(rmsea.ci.upper.scaled, nsmall = digits), "]"))
    names(fits) <- str_remove(names(fits), "\\.scaled")
  } else if("rmsea.ci.lower.robust" %in% names(fits)){
    fits <- fits %>%
      mutate(rmsea.ci = paste0("[", format(rmsea.ci.lower.robust, nsmall = digits),
                               ", ", format(rmsea.ci.upper.robust, nsmall = digits), "]"))
    names(fits) <- str_remove(names(fits), "\\.robust")
    names(fits) <- str_remove(names(fits), "\\.scaled")
    
  } else {stop("need to update function")}
  
  # fits <- fits %>%
  #   select(one_of(.id), ntotal, npar, chisq, df, pvalue, cfi, rmsea, rmsea.ci, srmr)
  
  return(fits)
}

# list; named list of character vectors where each vector includes the column names of an indicator measured repeatedly
# fname; name of the factor
# waves; number of waves in the data

write_mi_mod <- function(list, fname, waves = 1:4){
  
  bywave <- lapply(waves, function(x){
    paste0(fname, x," =~ ",
           paste(
             sapply(list, '[', x),  # extracts the item for each wave
             collapse = " + "))     # produces factor model for each wave
  })
  
  # could add if statement here to output each wave without collapsing
  model <- paste(bywave, collapse = "\n") # collapses across waves
}

## Add latent growth model syntax to the strict measurement invariance syntax
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
  # b) estimate latent intercept and slope mean, variance, and covariance, (Note: intercept mean must be 0 to identify model with ordinal indicators)
  # c) constrain covariances of first order and second order (e.g. intercept and slope) LVs to 0
  lgm.syntax <- "\n## LATENT GROWTH\n\nInt =~ 1*xyz1 + 1*xyz2 + 1*xyz3 + 1*xyz4\nSlope =~ 0*xyz1 + 1*xyz2 + 2*xyz3 + 3*xyz4\n
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

## add regression syntax to existing syntax
# currently the existing syntax is expected to be the lgm.syntax
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
    
    tv.syn <- paste("## TIME-VARIANT REGRESSIONS",
                    paste(map2(.x = waves, .y = tv.cov,
                               ~paste0(fname, .x, " ~ ", .y)), collapse = "\n"),
                    sep = "\n\n")
    
    syntax <- paste(syntax, tv.syn, sep = "\n")
  }
  
  return(syntax)
}

# cov <- c("a", "b")
# fn = "xyz"
# waves = 2:4
# test <- lapply(waves, function(x) paste(paste0(cov, x), collapse = " + "))
# paste("## TIME-VARIANT REGRESSIONS", paste(map2(waves, test, ~paste0(fn, .x, " ~ ", .y)), collapse = "\n"), sep = "\n\n")
# tv.syn <- paste("## TIME-VARIANT REGRESSIONS",
#                 paste(paste0("xyz", 2:4), " ~ ", "a + b",
#                 collapse = "\n"), sep = "\n\n")


# Other options exist for both syntax (semTools::measEq.syntax) and model (lavaan::lavaan)
# Need to incorporate ... so I don't have to list out all the parameters
# if produce == "fit" \code{model} needs to be the full syntax. if using a
# \code{measureEQ.syntax} object, then \code{model = as.character(object)} 
mi_wrapper <- function(model, data, items, fnames,
                       ID.fac = "auto.fix.first",
                       ID.cat = "millsap",
                       long.equal = "",
                       auto = "all",
                       group = NULL,
                       group.equal = "",
                       parameterization = "theta",
                       meanstructure = TRUE,
                       mimic = "Mplus",
                       estimator = "WLSMV",
                       missing = "pairwise",
                       cluster = NULL,
                       ordered = unlist(items),
                       produce = "both", # other options are "syntax", "fit"
                       ...){
  
  ## Define lavaan parameters, which are used for all output
  lavaan.params <- list(data = data,
                        ordered = ordered,
                        parameterization = parameterization,
                        meanstructure = meanstructure,
                        mimic = mimic,
                        estimator = estimator,
                        missing = missing,
                        cluster = cluster,
                        group = group,
                        group.equal = group.equal)
  
  if(produce %in% c("both", "syntax")){
    
    # arguments are specific to semTools::measEq.syntax
    syntax.params <- list(ID.fac = ID.fac,
                          ID.cat = ID.cat,
                          longFacNames = fnames,
                          longIndNames = items,
                          long.equal = long.equal,
                          auto = auto)
  }
  
  if(produce == "syntax"){
    
    output <- do.call(measEq.syntax,
                      c(list(configural.model = model), lavaan.params, syntax.params)) #, list(...)
    
  } else if(produce == "fit"){
    
    output <- do.call(lavaan, c(list(model = model), lavaan.params, list(...)))
    
  } else if(produce == "both"){
    
    syntax <- do.call(measEq.syntax, c(list(model), lavaan.params, syntax.params)) #, list(...)
    fit <- do.call(lavaan, c(list(as.character(syntax)), lavaan.params))
    
    output <- list(syntax = syntax,
                   fit = fit)
  } else {
    stop("produce argument must be 'both', 'syntax', or 'fit'")
  }
  
  return(output)
  
}

compare_mods <- function(mod0, mod1, digits = 3){
  
  aov01 <- anova(mod0, mod1)[2,c(5:7)]
  
  ind0 <- Get_lavaan_fits(mod0, measures = c("cfi", "tli", "rmsea", "srmr"))
  ind1 <- Get_lavaan_fits(mod1, measures = c("cfi", "tli", "rmsea", "srmr"))
  ind.diff <- ind1 - ind0
  
  mi.com <- format(round(cbind(aov01, ind.diff[3:6]), digits), nsmall = digits)
  row.names(mi.com) <- NULL
  
  return(mi.com)
}

## Calculating factor scores from lavaan object
lavaan_scores <- function(model, ids, fname, ...){
  
  
  ## make ids into a single column data.frame for binding
  id.df <- data.frame(ID = ids)
  
  ## Calculate scores and convert to data.frame
  fs <- do.call(lavPredict, c(list(object = model), list(...)))
  fs.df <- as.data.frame(fs)
  
  ## extract first order factor scores and convert to longer format
  f.scores <-bind_cols(id.df, select(fs.df, matches("[0-9]$"))) %>%
    gather(Wave, Score, matches("[0-9]$")) %>%
    mutate(Wave = str_replace(Wave, fname, "W"))
  
  ## Calculate scores based on latent growth model
  lg.scores <- bind_cols(id.df,select(fs.df, Int, Slope)) %>%
    mutate(W1 = Int,
           W2 = Int + Slope,
           W3 = Int + 2*Slope,
           W4 = Int + 3*Slope) %>%
    gather(Wave, LG, matches("W[0-9]$"))
  
  
  ## join both types of scores
  all.scores <- inner_join(lg.scores, f.scores, by = c("ID", "Wave")) %>%
    rename_with(~paste(fname, ., sep = "_"), c(Int, Slope, LG, Score))
  
  
  return(all.scores)
}

########################################################


# -------  Mplus Functions ------------------------------------

outcome_shortcut <- function(df, remove, recodes, tolower = FALSE){
  if(tolower == TRUE){
    names(recodes) <- tolower(names(recodes))
  }
  df <- df %>%
    mutate(outcome = str_remove(outcome, remove),
           outcome = dplyr::recode(outcome, !!!recodes) %>%
             factor(., levels = recodes)) %>%
    arrange(outcome)
  return(df)
}

# @param object - an mplus.model object returned from MplusAutomation::readModels
# @return a one-row dataframe of fit statistics
mplus_fit <- function(object, digits = 2, format = TRUE, ml = FALSE){
  
  if(ml == TRUE){
    fit <- object$summaries %>%
      select(ntotal = Observations, npar = Parameters, chisq = ChiSqM_Value, df = ChiSqM_DF, pvalue = ChiSqM_PValue,
             cfi = CFI, rmsea = RMSEA_Estimate, srmr.w = SRMR.Within, srmr.b = SRMR.Between)
    
    if(format == TRUE){
      fit <- fit %>%
        mutate(across(c(chisq, pvalue, cfi, rmsea, srmr.w, srmr.b), ~format(round(.x, digits), nsmall = digits)))
    }
  } else{
  
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
  }

  

  return(fit)
}

# @param object - an mplus.model object returned from MplusAutomation::readModels
mplus_est <- function(object, params = "all",
                      std = c("unstandardized", "stdyx.standardized", "stdy.standardized", "r2"),
                      combine = TRUE, alpha = .05, digits = 2, ci = FALSE){
  
  if("r2" %in% std){
    return(object$parameters[[std]])
  }
  
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

# @param object - list containing multiple mplus.model objects returned from MplusAutomation::readModels
# @return 
format_mplus <- function(object, recodes, outcomes,
                         std = c("unstandardized", "stdyx.standardized", "stdy.standardized"),
                         reg = TRUE, r2 = TRUE, digits = 2){
  
  fit <- map_dfr(.x = object, ~.x$summaries %>%
                   select(ntotal = Observations, npar = Parameters, chisq = ChiSqM_Value, df = ChiSqM_DF, pvalue = ChiSqM_PValue,
                          cfi = CFI, rmsea = RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, srmr = SRMR), .id = "Outcome") %>%
    mutate(rmsea.ci =  paste0("[", format(round(RMSEA_90CI_LB, digits), nsmall = digits),
                              ", ", format(round(RMSEA_90CI_UB, digits), nsmall = digits), "]"),
           Outcome = outcomes) %>%
    select(Outcome, ntotal:rmsea, rmsea.ci, srmr) %>%
    mutate(across(c(chisq, pvalue, cfi, rmsea, srmr), ~format(round(.x, digits), nsmall = digits)))
  
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
                                                     " (", format(round(se, digits), nsmall = digits), ")"),
                                   estimate = case_when(pval < .01 ~ paste0(estimate, "**"),
                                                        pval < .05 ~ paste0(estimate, "*"),
                                                        TRUE ~ estimate)))
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





##################################
####    Creating Codebook     ####


#### Extracting labels from a factor and/or character or labelled variable ####
## labelled returns both the label and the value, otherwise only the label is returned
Get_Labels <- function(data,type=c("factor","character","labelled","factor_character")){
  
  if(type=="labelled"){
    
    ## Select labelled variables, extract labels, and transform to long format dataframe
    all_labels <- data %>% select_if(haven::is.labelled) %>%
      purrr::map(~attr(.,"labels")) %>%
      tibble::enframe("Item","Value") %>% 
      mutate(Label = purrr::map(Value,~attr(.,"names"))) %>%
      tidyr::unnest(cols = c(Value, Label))
    
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
      purrr::map(~levels(.)) %>%
      tibble::enframe("Item","Label") %>%
      tidy::unnest(cols = c(Value, Label))
    
  }
  
  return(all_labels)
}

#### Creating and Exporting the Codebook ####
## Requires Get_Labels function from above; only tested thus far for type="labelled"
## if export_type = "none" & keep_R_Object = FALSE, the function does not output anything
Create_Codebook <- function(OrigData, export_type = c("excel","csv","none"), export_name = "MyData",
                            label_type = "labelled"){
  
  ## Extract Values and Labels
  ValueLabels <- OrigData %>% Get_Labels(type = label_type)
  
  ## Putting in wide format
  VLW <- ValueLabels %>% mutate(VL = paste(Value, Label, sep = " = ")) %>%
    group_by(Item) %>% summarize(Value_Label = paste(VL, collapse = "; ")) %>%
    ungroup()
  
  ## Extract Item Stem (i.e. label), then joining values and labels
  Stems <- purrr::map(OrigData, ~attr(.,"label")) %>%
    tibble::enframe("Item","Stem")
  
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
    
  } else if(export_type == "none"){
    
    return(Codebook)
    
  } else {
    
    stop("Must specify 'excel', 'csv', or 'none' for export_type argument.")
    
  }
  
}

###############################################################


# --------------- Sets of Variables -----------------


## Names and orders based on sources_of_strength_w1-w4$SCHOOL_W[1-4]
SchoolNames <- c("Brush", "South Park", "Fort Lupton", "Center", "Delta",
                 "Cheraw", "Rocky Ford", "Manitou Springs", "Cripple Creek-Victor",
                 "Yampah Mountain", "Renaissance", "Patriot", "CEC Denver",
                 "CEC Parker", "Northglenn", "JFK", "Red Canyon",
                 "Vail Ski & Snowboard", "Battle Mountain", "Central (Pueblo)")

DemoVars <- c("Transgender", "Gender", "Race", "SexOr")

#### Item-Scale Shortcuts ####

## Outcomes
NoContactPerpVars <- paste("SEX_VIOL_PERP", 1:4, sep = "_")
ContactPerpVars <- paste("SEX_VIOL_PERP", 5:13, sep = "_")
NoContactVictVars <- paste("SEX_VIOL_VICT", 1:4, sep = "_")
ContactVictVars <- paste("SEX_VIOL_VICT", 5:13, sep = "_")
CyberVars <- paste("CYBER_SEX_PERP", 1:3, sep = "_")
DismisssvVars <- paste("DISMISS_SEX_VIOL", 1:6, sep = "_")
HNCPerpVars <- paste("HOM_PERP", 1:5, sep = "_")
HNCVictVars <- paste("HOM_VICT", 1:5, sep = "_")

OutcomeVars <- list(NoContactPerpVars, NoContactVictVars,
                    ContactPerpVars, ContactVictVars,
                    HNCPerpVars, HNCVictVars,
                    CyberVars, DismisssvVars)
OutcomeNames <- c("No_Contact_Perpetration", "No_Contact_Victimization",
                  "Contact_Perpetration", "Contact_Victimization",
                  "HNC_Perpetration", "HNC_Victimization",
                  "Cybersex_Perpetration", "SV_Dismissiveness")

names(OutcomeVars) <- OutcomeNames

## Protective Factors
WellBeingVars <- paste("GEN_WELL_BEING", 1:8, sep = "_")
HelpAttVars <- paste("HELP_SEEK_GEN_BELIEF", 1:8, sep = "_")
SHHelpAttitudeVars <- paste("SEX_HAR_HELP_ATTITUDE", 1:4, sep = "_")
SHHelpIntentVars <- paste("SEX_HAR_HELP_INTENT", 1:4, sep = "_")
HelpOthersVars <- paste("INTENT_HELP_OTHERS", 1:6, sep = "_")
StaffIntentVars <- paste("STAFF_INT_BULY_SEX_VIOL", 1:8, sep = "_")

ProtectiveVars <- list(WellBeingVars, HelpAttVars,
                       SHHelpAttitudeVars, SHHelpIntentVars,
                       HelpOthersVars, StaffIntentVars)

ProtectiveNames <- c("General_Well.being", "General_Help_Attitudes",
                     "SH_Help_Attitudes", "SH_Help_Seeking",
                     "Intent_Help_Others", "Staff_Help_Intent")

names(ProtectiveVars) <- ProtectiveNames

## Exposure
ExposureVars <- c("EXPOSURE_POSTERS_VIDEOS", "EXPOSURE_SOCIAL_MEDIA", "EXPOSURE_STRENGTHS_RESILIENCE",
                  "EXPOSURE_HELP_SUICIDAL_TEENS", "EXPOSURE_TOLD_YOU_ABOUT_SOURCES", "EXPOSURE_OWN_STRENGTHS",
                  "EXPOSURE_ACTIVITITY_PARTICIPATION", "EXPOSURE_NAMED_TRUSTED_ADULT", "EXPOSURE_STRENGTHS_ANXIETY_DEP",
                  "EXPOSURE_STRENGTHS_GROW", "EXPOSURE_THANKFUL_FOR", "EXPOSURE_HELP_OTHERS", "EXPOSURE_SHARED_STORY_GETTING_HELP",
                  "EXPOSURE_OTHER", "EXPOSURE_LOGO")
ActiveExpVars <- c("EXPOSURE_NAMED_TRUSTED_ADULT", "EXPOSURE_STRENGTHS_ANXIETY_DEP",
                   "EXPOSURE_STRENGTHS_GROW", "EXPOSURE_THANKFUL_FOR", "EXPOSURE_HELP_OTHERS",
                   "EXPOSURE_SHARED_STORY_GETTING_HELP", "EXPOSURE_OTHER","EXPOSURE_ACTIVITITY_PARTICIPATION")
PassiveExpVars <- c("EXPOSURE_POSTERS_VIDEOS", "EXPOSURE_SOCIAL_MEDIA", "EXPOSURE_STRENGTHS_RESILIENCE",
                    "EXPOSURE_HELP_SUICIDAL_TEENS", "EXPOSURE_TOLD_YOU_ABOUT_SOURCES", "EXPOSURE_OWN_STRENGTHS",
                    "EXPOSURE_LOGO")

## Other Student-level scales
AODVars <- paste("AOD_NEXT6_MTHS", 1:4, sep = "_")
BullyPerpVars <- paste("BULLYING_PERP", 1:9, sep = "_")
CyberBullyPerpVars <- paste("CYBERBULLYING_PERP", 1:3, sep = "_")
DepAnxVars <- paste("DEP_ANX", 1:13, sep = "_")
PeerVictVars <- paste("PEER_VICT", 1:4, sep = "_")

OtherScaleVars <- list(AODVars, BullyPerpVars,
                       CyberBullyPerpVars, DepAnxVars,
                       PeerVictVars)
OtherScaleNames <- c("Alcohol_and_Drug", "Bullying",
                     "Cyberbullying", "Depression.Anxiety",
                     "Peer_Victimization")
names(OtherScaleVars) <- OtherScaleNames

## School-level Scales
StudentInterveneVars <- paste("STUDENTS_INTERVENE", 1:7, sep = "_")
StaffInterveneVars <- paste("STAFF_INTERVENE", 1:7, sep = "_")
YouInterveneVars <- paste("YOU_INTERVENE", 1:7, sep = "_")
SchCom2BullyVars <- paste("SCHOOL_COMMITMENT_BULLYING", 1:6, sep = "_")
SchCom2SHVars <- paste("SCHOOL_COMMITMENT_SEX_HARR", 1:6, sep = "_")
SchCom2MHVars <- paste("SCHOOL_COMMITMENT_MT_HEALTH", 1:6, sep = "_")
SchAggressionVars <- paste("AGGRESSION_PROBLEM_AT_SCHOOL", 1:7, sep = "_")
SchClimateVars <- paste("POSITIVE_PERCEP_SCHOOL_CLIMATE", 1:11, sep = "_")
ProgwarenessVars <- paste("PROGRAM_AWARENESS", 1:7, sep = "_")
ProgParticipationVars <- paste("PROGRAM_PARTICIPATION", 1:6, sep = "_")

SchoolScales <- list(StudentInterveneVars, StaffInterveneVars, YouInterveneVars,
                     SchCom2BullyVars, SchCom2SHVars, SchCom2MHVars,
                     SchAggressionVars, SchClimateVars, ProgwarenessVars, ProgParticipationVars)
names(SchoolScales) <- c("Student_Intervene", "Staff_Intervene", "You_Intervene",
                         "School_Commitment_to_Bullying", "School_Commitment_to_SH",
                         "School_Commitment_to_Mental_Health", "School_Aggression_Problems",
                         "Positive_School_Climate", "Program_Awareness", "Program_Participation")


#' @title Plots for checking model assumptions
#'
#' @description 
#' Produces plots via [ggplot2] to examine the fit of single or two-level linear or logistic regression models 
#' 
#' @param model An `lm`, `glm`, or `merMod` object.
#' @param glm.predict Type of predictions to use when model is a `glm` object. Can be "response" or "link", but only "response" is currently used for outputs.
#' @param glm.resid Type of predictions to use when model is a `glm` or glmerMod object. Can be "deviance", "pearson", "working", "response", or "partial".
#' @param smooth_method Method of smoothing, which is passed to [ggplot2::geom_smooth()]. Default is "loess".
#' @param ... Additional arguments passed to [ggplot2::geom_smooth()].
#' 
#' @details 
#' Currently accepts linear models from [stats::lm()] or [lme4::lmer()] or logistic models from [stats::glm()] or [lme4::glmer()].
#' Linear models output a Q-Q plot of raw residuals and scatterplot of fitted and raw residual values to examine normality and
#' homoscedasticity assumptions, respectively. Logistic models output a scatterplot of residuals grouped by binary outcome to examine
#' homoscedasticity. Additionally, a ROC analysis and plot of the ROC curve, along with area under the curve estimate, demonstrates the
#' discrimination of the logistic model. All models produce a plot of Cook's distance for each observation.
#' Two-level models from [lme4] also output a scatterplot of level 1 and level 2 residuals to evaluate their correlation and, for linear models,
#' a Q-Q plot of the level 2 residuals (i.e., random intercept estimates).  
#' 
#' @return a list object.
#' 
#' @export
#' 
#' @examples 
#' ## OLS
#' data(sleepstudy, package = "lme4")
#' lm.mod <- lm(Reaction ~ Days, data = sleepstudy)
#' lm.plots <- ModelCheckPlots(lm.mod, se = FALSE, color = "black")
#' 
#' ## Two-level logistic regression
#' data(grouseticks, package = "lme4")
#' grouseticks$TICKS01 <- ifelse(grouseticks$TICKS > 0, 1, 0)
#' 
#' glmer.mod <- lme4::glmer(TICKS01 ~ YEAR + cHEIGHT + (1|LOCATION), family = binomial(link = "logit"), data = grouseticks)
#' glmer.plots <- ModelCheckPlots(glmer.mod, glm.predict = "response", glm.resid = "deviance")
ModelCheckPlots <- function(model,
                            glm.predict = "response", glm.resid = "deviance",
                            smooth_method = "loess", ...){
  
  library(ggplot2)
  
  ## Collects output list
  MCPlots <- list(QQ = NULL,
                  L2_QQ = NULL,
                  Fitted_and_Residual_Values = NULL,
                  Residual_Correlation = NULL,
                  Cooks_Distance = NULL,
                  ROC_Curve = NULL)
  
  ## Extracting residuals
  if("lm" %in% class(model)){
    
    ## Dependent variable name
    DV <- names(model$model)[[1]]
    
    ## fit information
    aug <- broom::augment(model, type.predict = glm.predict, type.residuals = glm.resid) #glm. arguments ignored for lm objects
    
    
  } else if(grepl("merMod", class(model))){
    
    ## Dependent variable name
    DV <- names(model@frame)[[1]]
    
    ## Level 1 fit information
    aug <- broom.mixed::augment(model, type.predict = glm.predict, type.residuals = glm.resid) #glm. arguments ignored for lmer objects
    
    ## Level 2 information and plots
    clusternm <- names(model@cnms)[[1]]
    L2aug <- broom.mixed::augment(lme4::ranef(model))
    L1L2 <- dplyr::left_join(dplyr::rename(aug, level = all_of(clusternm)), L2aug, by = "level")
    
    ## L1 and L2 Residual Correlation
    L1L2corr <- ggplot(L1L2, aes(x = estimate, y = .resid)) +
      geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
      geom_point(shape = 1) +
      geom_smooth(method = smooth_method, ...) +
      labs(x = "Level 2 Residual", y = "Level 1 Residual") +
      theme_bw()
    
    # Storing plot in output object
    MCPlots[["Residual_Correlation"]] <- L1L2corr
    
  } else {stop("model must be of class 'lm', 'glm', or '(g)lmerMod'")}
  
  ## Adding id column
  aug <- tibble::rowid_to_column(aug, ".id")
  
  ## Cook's d
  cookplot <- ggplot(aug, aes(x = .id, y = .cooksd)) +
    geom_bar(stat = "identity", position = "identity") +
    labs(x = "Observation", y = "Cook's distance") +
    theme_bw()
  
  # Storing plot in output object
  MCPlots[["Cooks_Distance"]] <- cookplot
  
  
  if("glm" %in% class(model) | class(model) == "glmerMod"){
    
    ## Fitted vs. Residuals grouped by DV
    aug[[DV]] <- factor(aug[[DV]]) 
    
    binned <- ggplot(aug, aes(x = .id, y = .resid)) + 
      geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
      geom_point(aes_string(color = DV)) +
      geom_smooth(aes_string(color = DV), ...) +
      labs(x = "Observation", y = paste(tools::toTitleCase(glm.resid), "Residual")) +
      theme_bw()
    
    ## ROC Analysis
    rocit <- pROC::roc(response = aug[[DV]], predictor = aug[[".fitted"]]) # glm.predict muste be "response"
    
    # Extracting ROC Results
    AUC <- round(rocit$auc[[1]], 3)
    senspec <- data.frame(Sensitivity = rocit$sensitivities,
                          Specificity = rocit$specificities)
    
    rocplot <- ggplot(senspec, aes(x = Specificity, y = Sensitivity)) + 
      geom_abline(slope = 1, intercept = 1, colour = "grey", size = 1) +
      geom_line(size = 1.5, color = "black") +
      scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .2)) +
      scale_x_reverse(limits = c(1, 0), breaks = seq(1, 0, -.2)) +
      annotate(geom = "text", x = .3, y = .2, label = paste("AUC =", AUC)) +
      theme_bw()
    
    
    # Storing plots in output object
    MCPlots[["Fitted_and_Residual_Values"]] <- binned
    MCPlots[["ROC_Curve"]] <- rocplot
    
  } else if(class(model) == "lm" | class(model) == "lmerMod"){
    
    ## Q-Q Plot
    qq <- ggplot(aug, aes(sample = .resid)) +  ## standardizing doesn't change the shape of the distribution so it doesn't matter of .resid or .std.resid
      stat_qq(shape = 1) + stat_qq_line() +
      labs(x = "Theoretical Quantiles", y = "Residual") +
      theme_bw()
    
    ## Fitted vs. Residuals Plot
    frplot <- ggplot(aug, aes(x = .fitted, y = .resid)) +
      geom_hline(aes(yintercept = 0), color = "grey", size = 1, linetype = 1) +
      geom_point(shape = 1) +
      geom_smooth(method = smooth_method, ...) +
      labs(x = "Fitted", y = "Residual") +
      theme_bw()
    
    # Storing plots in output object
    MCPlots[["QQ"]] <- qq
    MCPlots[["Fitted_and_Residual_Values"]] <- frplot
    
  }
  
  if(class(model) == "lmerMod"){
    
    ## Q-Q Plot of L2 residuals
    L2qq <- ggplot(L2aug, aes(sample = estimate)) +  
      stat_qq(shape = 1) + stat_qq_line() +
      labs(x = "Theoretical Quantiles", y = "Level 2 Residual") +
      theme_bw()
    
    # Storing plot in output object
    MCPlots[["L2_QQ"]] <- L2qq
    
  }
  
  ## Removing NULL list elements 
  MCPlots <- MCPlots[lengths(MCPlots) != 0]
  
  return(MCPlots)
  
}
