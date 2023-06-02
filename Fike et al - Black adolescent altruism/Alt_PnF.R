###########################################
#                                         #
#        Black Adolescent Altruism        #
#       Packages & Custom Functions       #
#                                         #
###########################################

#### Packages ####
library(haven)
library(dplyr)
library(purrr)
library(tidyr)
# library(emmeans)
# library(interactions)
# library(reghelper)
# library(Hmisc)
# library(ggplot2)
library(lavaan)
library(kfa)
library(flextable)
library(stringr)
library(semTools)

#### custom functions ####
## calculate mean, sd for each group and run t-test; combine into single table
the_t <- function(data, outcome, by, name, covariates = NULL){
  
  out <- enquo(outcome)
  b <- enquo(by)
  dsout <- deparse(substitute(outcome))
  dsb <- deparse(substitute(by))
  
  if(is.null(covariates)){ # conducts t-test
    
  test <- t.test(data[[dsout]] ~ data[[dsb]]) %>%
    broom::tidy() %>%
    mutate(Variable = name,
           d = effectsize::t_to_d(statistic, parameter)[[1]]) %>%
    select(Variable, t = statistic, df = parameter, p = p.value, d)
  
  } else { # conducts linear regression
    
    mod <- lm(formula(paste(dsout, "~", dsb, "+", covariates)), data = data2)
    test <- parameters::model_parameters(mod) %>%
      filter(Parameter == dsb) %>%
      mutate(Variable = name,
             d = effectsize::t_to_d(t, df_error)[[1]]) %>%
      select(Variable, t, df = df_error, p, d)
  }
  
  together <- data %>%
    filter(!is.na(!!b)) %>%
    group_by(!!b) %>%
    summarize(an = sum(!is.na(!!out)),
              M = mean(!!out, na.rm = TRUE),
              SD = sd(!!out, na.rm = TRUE)) %>%
    gather(x, y, an:SD) %>%
    unite(col = "temp", !!b, x, sep = '_') %>%
    spread(temp, y) %>%
    bind_cols(test) %>%
    select(Variable, everything())
  
  return(together)
  
}

## formatting output tables
two_level_flex <- function(flex, mapping, vert.cols, border, digits, dig.cols){
  
  flex <- flextable::colformat_double(flex, j = dig.cols, digits = digits)
  flex <- flextable::set_header_df(flex, mapping = mapping)
  flex <- flextable::merge_h(flex, part = "header")
  flex <- flextable::merge_v(flex, j = vert.cols, part = "header")
  flex <- flextable::fix_border_issues(flex)
  flex <- flextable::border_inner_h(flex, border = border, part = "header")
  flex <- flextable::hline_top(flex, border = border, part = "all")
  # flex <- flextable::theme_vanilla(flex)
  flex <- flextable::align(flex, j = -1, align = "center", part = "all")
  flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
  flex <- flextable::fontsize(flex, i = NULL, j = NULL, size = 11, part = "all")
  flex <- flextable::italic(flex, i = NULL, italic = TRUE, part = "header")
  flex <- flextable::padding(flex, padding = 1, part = "all")
  flex <- flextable::autofit(flex)
  return(flex)
}
flex.border <- officer::fp_border(width = 2) # manual horizontal flextable border width



flextab_format <- function(df, bold.type = "none", width = NULL, digits = 2){
  
  numericcols <- which(unlist(lapply(df, is.numeric)))
  
  flex <- flextable::flextable(df)
  # flex <- flextable::colformat_double(flex, j = numericcols, digits = digits)
  flex <- flextable::align(flex, i = NULL, j = NULL, align = "center", part = "all")
  flex <- flextable::align(flex, i = NULL, j = c(1), align = "left", part = "all")
  flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
  flex <- flextable::padding(flex, padding = 1, part = "all")
  
  if(bold.type == "fit"){
    flex <- bold(flex, i = ~rmsea == min(rmsea), part =  "body")
  } else if(bold.type == "lambda"){
    flex <- bold(flex, i = ~mean < .3, part = "body")
    # error occurs when .3 is replaced by cut; not sure why
  } else if(bold.type == "p"){
    flex <- bold(flex, i = ~p < .05, part = "body")
  }
  
  if(!is.null(width)){
    flex <- flextable::fit_to_width(flex, max_width = width)
  }
  flex <- flextable::autofit(flex)
  
  return(flex)
}

#### Extract model information and fit statistics ####
get_lavaan_fits <- function(object, measures = "scaled"){
  
  if(object@Fit@converged == 0){
    stop("Model did not converge and fit statistics cannot be produced.")
  }
  
  if(length(measures) > 1){
    
    indices <- measures
    
  } else if(measures == "scaled"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.scaled', 'rmsea.scaled',
                 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled','srmr')
    
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


#### Likelihood ratio test comparing nested models ####
compare_mods <- function(mod0, mod1, method = "default", measures = c("cfi", "rmsea", "srmr"), digits = 3){
  
  aov01 <- lavTestLRT(mod0, mod1, method = method)[2,c(5:7)]
  
  # Uses non-scaled version
  ind0 <- get_lavaan_fits(mod0, measures = measures)
  ind1 <- get_lavaan_fits(mod1, measures = measures)
  ind.diff <- ind1 - ind0
  
  mi.com <- format(round(cbind(aov01, ind.diff), digits), nsmall = digits)
  row.names(mi.com) <- NULL
  # names(mi.com) <- c("delta.chisq", "delta.df", "delta.pvalue", "delta.cfi", "delta.rmsea", "delta.srmr")
  
  return(mi.com)
}

cfa_mod <- function(items, fname){
  paste0(fname, " =~ ",  paste(items, collapse = " + "))
}

extract_lavaan_params <- function(object, std = "no", params = "all", ...){
  
  if("all" %in% params){
    
    params = c("=~", "~~", "~*~", "~1", "~", "|", "<~")
    
  }
  
  if(std == "no"){
    
    TheParams <- parameterEstimates(object, standardized = FALSE, ...) %>%
      filter(op %in% params)
    
  } else if(std %in% c("std.all", "std.lv", "std.nox")){
    
    TheParams <- standardizedSolution(object, type = std, ...) %>%
      filter(op %in% params)
    
  } else {
    stop("std argument must be 'no' for unstandardized estimates
         or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }
  
  
  return(TheParams)
}
