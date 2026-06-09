#############################################################
#    Trends in Anxiety, Depression, and Social Isolation    #
#    among First-Year College Students before and after     #
#          the onset of the Covid-19 Pandemic               #
#            Last Update: January 2025; KN                  #
#############################################################

# -----    Packages   --------
library(ggplot2)
library(dplyr)
library(kfa)
library(gtsummary)
library(tidyr)
library(MplusAutomation)
library(purrr)
library(forcats)
library(stringr)




# -----      Custom Functions    --------

####    Codebook    ####

#' Create codebook for a dataset
#'
#' The function extracts and organizes variable attributes into a dataframe that can be used for other R functions or exported.
#'
#' @param data a \code{data.frame}
#' @param export.type current options are \code{"excel"} and \code{"csv"}
#' @param export.name character string indicating file path and file name for exported codebook. File extention is added by the function and should not be included here.
#' @param return.object should the codebook dataframe be returned?
#'
#' @details
#' If \code{export.type = NULL} & \code{return.object = FALSE}, the function does not output anything.
#'
#' @return data.frame

create_codebook <- function(data,
                            export.type = NULL, export.name = "MyCodebook",
                            return.object = FALSE){
  
  ## Extract Values and Labels
  ValueLabels <- get_labels(data)
  
  ## Putting in wide format
  VLW <- ValueLabels %>%
    mutate(VL = paste(Value, Label, sep = " = ")) %>%
    group_by(Item) %>%
    summarize(Value_Label = paste(VL, collapse = "; ")) %>%
    ungroup()
  
  ## Extract Item Stem (i.e. label), then joining values and labels
  Stems <- lapply(data, function(x) attr(x,"label")) %>%
    tibble::enframe("Item","Stem")
  
  ## Joining Stems, Values, and Labels (gets us 90% of the way to the final codebook)
  Codebook <- left_join(Stems, VLW, by = "Item") %>%
    mutate(Stem = as.character(Stem))
  
  if(!is.null(export.type)){
    if(export.type == "excel"){
      
      ## Exporting to excel file
      openxlsx::write.xlsx(x = Codebook, file = paste0(export.name, ".xlsx"), asTable = FALSE)
      message("Codebook exported to file ", export.name, ".xlsx")
      
    } else if(export.type == "csv"){
      
      ## Exporting to csv file
      write.csv(Codebook,file = paste0(export.name, ".csv"), row.names = FALSE, na = "")
      message("Codebook exported to file ", export.name, ".csv")
      
    }
  }
  
  if(return.object == TRUE){
    return(Codebook)
  }
}

#' Extracting labels from factor and labelled variables
#'
#' @param data a \code{data.frame} with observations in rows and variables in columns
#'
#' @return a \code{data.frame} with a row for each value-label pair

get_labels <- function(data){
  
  if(sum(sapply(data, haven::is.labelled)) > 0){
    
    ## Select labelled variables, extract labels, and transform to long format dataframe
    llbls_temp <- lapply(select_if(data, haven::is.labelled), function(x) attr(x,"labels"))
    
    # if values are a mix of numeric and character
    if(length(unique(sapply(llbls_temp, class))) != 1){
      
      llbls_num <- llbls_temp[sapply(llbls_temp, class) == "numeric"] %>%
        tibble::enframe("Item","Value") %>%
        mutate(Label = lapply(Value, function(x) attr(x,"names"))) %>%
        tidyr::unnest(cols = c(Value, Label)) %>%
        mutate(Value = as.character(Value)) # need to convert to character for combining
      
      llbls <- llbls_temp[sapply(llbls_temp, class) == "character"] %>%
        tibble::enframe("Item","Value") %>%
        mutate(Label = lapply(Value, function(x) attr(x,"names"))) %>%
        tidyr::unnest(cols = c(Value, Label)) %>%
        bind_rows(llbls_num) %>%
        mutate(Item = factor(Item, levels = names(llbls_temp))) %>%
        arrange(Item)
      
    } else {
      
      llbls <- llbls_temp %>%
        tibble::enframe("Item","Value") %>%
        mutate(Label = lapply(Value, function(x) attr(x,"names"))) %>%
        tidyr::unnest(cols = c(Value, Label))
    }
    
  } else {
    llbls <- NULL
  }
  
  if(sum(sapply(data, is.factor)) > 0){
    
    ## Extract labels and transform to long format dataframe
    flbls <- lapply(select_if(data, is.factor), levels) %>%
      tibble::enframe("Item","Label") %>%
      mutate(Value = lapply(Label, function(x) 1:length(x))) %>%
      tidyr::unnest(cols = c(Value, Label))
    
  } else {
    flbls <- NULL
  }
  
  if(!is.null(llbls) & !is.null(flbls)){
    if(class(llbls$Value) == "character"){
      flbls$Value <- as.character(flbls$Value)
    }
    
    all_labels <- bind_rows(llbls, flbls) %>%
      mutate(Item = factor(Item, levels = names(data)) %>%
               forcats::fct_drop()) %>%
      arrange(Item)
    
    return(all_labels)
    
  } else if(!is.null(llbls)){
    return(llbls)
  } else if(!is.null(flbls)){
    return(flbls)
  }
}


####    Scoring and Descriptives   #####

#' Calculate scale score
#'
#' Combine item responses into a scale score
#'
#' @param data a \code{data.frame}
#' @param items character vector of column names in \code{data} to use in the scale
#' @param type should the score be the \code{"sum"} or the \code{"mean"} of item responses?
#' @param min.valid the minimum number of valid responses to receive a score. The default is 1, otherwise NA is returned.
#'

scale_score <- function(data, items = names(data), type = c("sum", "mean"), min.valid = 1){
  
  if(type == "sum"){
    score <- rowSums(data[, items], na.rm = TRUE)
  } else if(type == "mean"){
    score <- rowMeans(data[, items], na.rm = TRUE)
  }
  
  nacount <- rowSums(is.na(data[, items]), na.rm = TRUE)
  score <- ifelse((length(items) - nacount) < min.valid, NA, score)
  
  return(score)
}

## calculate mean, sd for each group and run t-test; combine into single table
the_t <- function(data, outcome, by, name, covariates = NULL){
  
  out <- sym(outcome)
  b <- sym(by)
  
  if(is.null(covariates)){ # conducts t-test
    
    test <- t.test(data[[outcome]] ~ data[[by]]) %>%
      broom::tidy() %>%
      mutate(Variable = name,
             d = effectsize::t_to_d(statistic, parameter)[[1]]) %>%
      select(Variable, t = statistic, df = parameter, p = p.value, d)
    
  } else { # conducts linear regression
    
    mod <- lm(formula(paste(outcome, "~", by, "+", covariates)), data = data2)
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
    tidyr::gather(x, y, an:SD) %>%
    tidyr::unite(col = "temp", !!b, x, sep = '_') %>%
    tidyr::spread(temp, y) %>%
    bind_cols(test) %>%
    select(Variable, everything()) %>%
    rename_with(.cols = ends_with("_an"), ~str_replace(.x, "_an", "_n"))
  
  return(together)
  
}

####    MPlus    ####

# as.numeric(stringr::str_extract(enum.mods$step1_1class.out$output[grepl("Entropy", enum.mods$step1_1class.out$output)], "\\d+\\.*\\d*"))
# 
# enum.mods$step1_1class.out$output[which(grepl("LO-MENDELL-RUBIN ADJUSTED LRT TEST", enum.mods$step1_1class.out$output))]
# lmrt <- which(grepl("LO-MENDELL-RUBIN ADJUSTED LRT TEST", enum.mods$step1_1class.out$output))
# lmrt2 <- as.numeric(stringr::str_extract(enum.mods$step1_1class.out$output[c(lmrt+2, lmrt+3)], "\\d+\\.*\\d*"))
# blrt <- which(grepl("Approximate P-Value", enum.mods$step1_1class.out$output))
# blrt2 <- as.numeric(stringr::str_extract(enum.mods$step1_1class.out$output[blrt], "\\d+\\.*\\d*"))
# 
# test.df <- data.frame(LMRT_value = lmrt2[[1]], LMRT_p = lmrt2[[2]], BLRT_p = blrt2)
# 
# ent <- as.numeric(stringr::str_extract(enum.mods$step1_1class.out$output[grepl("Entropy", enum.mods$step1_1class.out$output)], "\\d+\\.*\\d*"))
# ent <- ent[!is.na(ent)]
# 
# test.df <- test.df %>%
#   mutate(Entropy = ifelse(length(ent) != 1, NA, ent))

# @param object - an mplus.model object returned from MplusAutomation::readModels
# @return a one-row dataframe of fit statistics
mplus_fit <- function(object, digits = 2, format = TRUE, ml = FALSE, lpa = FALSE){
  
  if(lpa == TRUE){
    
    lmrt <- which(grepl("LO-MENDELL-RUBIN ADJUSTED LRT TEST", object$output))
    if(length(lmrt) == 0){
      test.df <- data.frame(LMRT_value = NA, LMRT_p = NA, BLRT_p = NA)
    } else {
    lmrt2 <- as.numeric(stringr::str_extract(object$output[c(lmrt+2, lmrt+3)], "\\d+\\.*\\d*"))
    blrt <- which(grepl("Approximate P-Value", object$output))
    blrt2 <- as.numeric(stringr::str_extract(object$output[blrt], "\\d+\\.*\\d*"))
    
    test.df <- data.frame(LMRT_value = lmrt2[[1]], LMRT_p = lmrt2[[2]], BLRT_p = blrt2)
    }
    
    entropy <- as.numeric(stringr::str_extract(object$output[grepl("Entropy", object$output)], "\\d+\\.*\\d*")) # extracts value
    entropy <- entropy[!is.na(entropy)]
    
    test.df <- test.df %>%
      mutate(Entropy = ifelse(length(entropy) != 1, NA, entropy))
      
    
    fit <- object$summaries %>%
      mutate(CAIC = -2*LL+Parameters*(log(Observations)+1)) %>% # see Nylund-Gibson & Choi, 2018 for formula
      select(Profiles = NLatentClasses, ntotal = Observations, npar = Parameters, LL, AIC, BIC, SABIC = aBIC, AICC, CAIC) %>%
      bind_cols(test.df)
    
    if(format == TRUE){
      fit <- fit %>%
        mutate(across(c(LL:CAIC), ~format(round(.x, digits), nsmall = digits)))
    }
  }
  
  # if(ml == TRUE){
  #   fit <- object$summaries %>%
  #     select(ntotal = Observations, npar = Parameters, chisq = ChiSqM_Value, df = ChiSqM_DF, pvalue = ChiSqM_PValue,
  #            cfi = CFI, rmsea = RMSEA_Estimate, srmr.w = SRMR.Within, srmr.b = SRMR.Between)
  #   
  #   if(format == TRUE){
  #     fit <- fit %>%
  #       mutate(across(c(chisq, pvalue, cfi, rmsea, srmr.w, srmr.b), ~format(round(.x, digits), nsmall = digits)))
  #   }
  # } else{
  #   
  #   fit <- object$summaries %>%
  #     select(ntotal = Observations, npar = Parameters, chisq = ChiSqM_Value, df = ChiSqM_DF, pvalue = ChiSqM_PValue,
  #            cfi = CFI, rmsea = RMSEA_Estimate, RMSEA_90CI_LB, RMSEA_90CI_UB, srmr = SRMR) %>%
  #     mutate(rmsea.ci =  paste0("[", format(round(RMSEA_90CI_LB, digits), nsmall = digits),
  #                               ", ", format(round(RMSEA_90CI_UB, digits), nsmall = digits), "]")) %>%
  #     select(ntotal:rmsea, rmsea.ci, srmr)
  #   
  #   if(format == TRUE){
  #     fit <- fit %>%
  #       mutate(across(c(chisq, pvalue, cfi, rmsea, srmr), ~format(round(.x, digits), nsmall = digits)))
  #   }
  # }
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

LP_OR_Results <- function(comp){
  
  strategies <- data.frame(Strategy = rep(NA, 15),
                           OR = rep(NA, 15),
                           SE = rep(NA, 15),
                           Statistic = rep(NA, 15),
                           P = rep(NA, 15))
  for(c in 1:15){
    d <- c*2
    strategies[c, ] <- data.frame(
      Strategy = comp[[d+1]],
      OR = str_sub(comp[[d+2]], 23,28),
      SE = str_sub(comp[[d+2]], 34,39),
      Statistic = str_sub(comp[[d+2]], 45,50),
      p = str_sub(comp[[d+2]], 56,61))
  }
  
  profiles <- str_extract_all(comp[[1]], "[:digit:]")[[1]]
  out <- bind_cols(data.frame(P1 = profiles[[1]],
                              P2 = profiles[[2]]),
                   strategies)
  return(out)
}
