########################################
#                                      #
#       Paper 3 - SV Perp Identities   #
#   Data Preparation and Exploration   #
#                                      #
########################################

#### Loading packages and functions ###
library(dplyr)
library(haven)
library(stringr)
library(ggplot2)
library(tidyr)
library(forcats)
library(lavaan)
library(purrr)
library(flextable)
library(semTools)
library(gtsummary)
library(emmeans)

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
cfa_wrapper <- function(data, items, fname,
                        ordered = items,
                        estimator = "WLSMV",
                        missing = "pairwise",
                        mimic = "lavaan",
                        cluster = NULL,
                        digits = 2){
  
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
  
  return(output)
  
}

get_tab <- function(data, group, outcomes){
  
  tab <- data %>%
    select(all_of(c(group, outcomes))) %>%
    mutate(across(.cols = all_of(group), .fn = as_factor)) %>%
    rename_with(.cols = all_of(outcomes), ~str_replace_all(.x, "_", " ")) %>%
    tbl_summary(by = group, 
                missing="no")
  
  return(tab)
  
}

contrasts.m <- function(x, y, alpha = .05) {
  
  x <- as.character(x)
  
  freq <- as.data.frame(table(x))
  
  names <- as.character(freq[which(freq$Freq>10), "x"]) 
  
  x1 <- ifelse(x %in% names, x, NA)
  
  x1 <- factor(x1)
  
  mod <- glm(y ~ x1, family = "binomial")
  
  # from emmeans documentation (including vignette("models", "emmeans")), I'm not sure what distinguishes mode and type arguments.
  # Despite accepting some of the same parameters (e.g., "linear.predictor", "response"), they don't function the same, or possibly
  # mode is just ignored for glm models. "Score" also does not appear to be a valid parameter for type, so the default ("linear.predictor") is used,
  # which gives the contrast in logits, whereas type = "response" gives the contrasts in odds ratios
  b <- summary(emmeans(mod, pairwise ~ x1, adjust="tukey", mode="linear.predictor", type="Score"))
  
  
  c <-  b$contrasts
  
  c[,2:6] <- round(c[,2:6], 2)
  
  c <- c[which(c$p.value < alpha),]
  
  
  return(c)
  
}
