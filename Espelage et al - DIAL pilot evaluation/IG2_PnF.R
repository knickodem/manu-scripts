#####################################
#                                   #
#        IES Goal 2 Analysis        #
#      Packages and F=unctions      #
#                                   #
#####################################

## Report due at the end of March

#### Functions ####
library(dplyr)
library(purrr)
library(gtsummary)
library(kfa)
library(lavaan)
library(tidyr)
library(forcats)
library(stringr)
library(ggplot2)
library(flextable)

#### Shortcut for converting data from wide to long format by wave ####
# put between subject variables in ... 
LongWave <- function(data, wave, ...){
  
  wave.chr <- paste0("_W", wave)
  
  btsubvars <- quos(...)
  
  longdat <- data %>%
    select(!!!btsubvars, contains(wave.chr)) %>%
    rename_at(vars(contains(wave.chr)), ~stringr::str_remove(., wave.chr)) %>%
    mutate(Wave = wave)
  
  return(longdat)
}


# -------   Item and Score Functions   ----------------

#### Item Frequencies (including NAs) for multiple items with similar response options ####
# data = items and grouping variables
# ... = grouping variables
# NAto0 = Convert NAs in frequency table to 0s?
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



#### Coding check-all-that-apply responses into a single variable #####
#' @param data \code{data.frame}. If \code{time} is specified, data must be in "long" format with rows being one time point per subject
#' @param id column name for the id variable in \code{data}
#' @param approach one of "longer", "all", "multiple", "priority", or "mode". See Details.
#' @param ... <\code{\link[tidyr]{tidyr_tidy_select}}> column names in data indicating the dichotomous check-all-that-apply variables to combine
#' @param endorse value indicating endorsement of the category. Common values are 1 (default), "yes", or 2 (for SPSS data).
#' @param time column name for the time variable in \code{data}
#' @param priority character vector of one or more names supplied to \code{...} indicating the order to prioritize response categories when \code{approach} is "priority" or "mode".
#' @param new.name character; column name for the created variable
#' @param sep separator to use between values when \code{approach = "all"}
#' @return \code{data.frame}
#' 
#' @details 
#' When \code{approach} is "multiple", "priority", or "mode", subjects with missing data for all columns supplied to ... are removed.
#' 
#' \itemize{
#' \item \code{"longer"} Produces a longer \code{data.frame} with a row for each subject by category option (by time, if specified)
#' \item \code{"all"} Produces \code{data.frame} with a row per subject (and time point, if specified) with new variable comprised of all categories endorsed separated by \code{sep}.
#' }
#' The remaining 3 approaches produce one code across all time points. The output is a data.frame with one row for each subject
#' \itemize{
#' \item \code{"multiple"} If subject endorsed multiple categories within or across time, code as "Multiple".
#' \item \code{"priority"} Same as "multiple" unless subject endorsed category specified in priority argument at any point, then code as priority
#' \item \code{"mode"} Coded as category with the mode (i.e., most common) endorsement across all time points. Ties coded as "Multiple"; Up to 2 categories can be prioritized over the mode by specifying priority argument
#' }
#' 
#' @example 
#' catacode(sos.long, id = StudentID, approach = "priority", Black:White, time = Wave, priority = c("Indigenous", "Islander"), new.name = "Race_Ethnicity")


catacode <- function(data, id, approach, ...,
                     endorse = 1, time = NULL, priority = NULL,
                     new.name = "variable", sep = "-"){
  
  if(!approach %in% c("longer", "all", "multiple", "priority", "mode")){
    stop("approach must be one of 'longer', 'by_time', 'multiple', 'priority', or 'mode'")
  }
  
  catacols <- quos(...)
  idv <- enquo(id)
  tv <- enquo(time)
  
  if(length(catacols) != 1 | grepl(":", rlang::quo_text(catacols), fixed = TRUE) | grepl("\\(", rlang::quo_text(catacols), fixed = TRUE)){
    
  ## Pivot data to longer format with one row for each id x category (x time) combination
  data <- data %>%
    dplyr::select(!!idv, !!tv, !!!catacols) %>%
    tidyr::pivot_longer(c(!!!catacols), names_to = "new", values_to = "response") 
  
  if(approach == "longer"){
    
    names(data)[names(data) == "new"] <- new.name
    return(data)
  }
  
  if(approach == "all"){
    
    ## Determines all response categories endorsed
    output <- data %>%
      mutate(response = ifelse(response == endorse, new, NA)) %>%                   # Changing values of 1 to the name it represents
      tidyr::pivot_wider(names_from = new, values_from = response) %>%              # Pivot data to wider format with one row for each id x time combination
      unite(col = "new", -!!idv, -!!tv, remove = TRUE, na.rm = TRUE, sep = sep) %>%     # remove = FALSE allows us to examine if the uniting worked
      mutate(new = ifelse(new == "", NA, new))                            # If subject responded NA to all categories for all waves, code as NA rather than ""
    names(output)[names(output) == "new"] <- new.name                                # Note: Combinations are currently in the order in which the variable appears in the dataset
    
    return(output)
  }
  
  if(approach %in% c("multiple", "priority", "mode")){
    
    ## Summarizes each subject's response pattern
    across.prep <- data %>% 
      filter(response == endorse) %>%                    
      group_by(!!idv, new) %>%
      summarize(n_time = n(), .groups = "drop_last") # number of times a subject identified as a certain category
  }
  } else {
    
    ## for data already in format with one row per category (per time)
    across.prep <- data %>%
      rename(new = rlang::quo_text(catacols[[1]])) %>%
      filter(!is.na(new)) %>%            # remove NAs otherwise too many coded as Multiple
      group_by(!!idv, new) %>%
      summarize(n_time = n(), .groups = "drop_last")
    
    if(inherits(across.prep$new, c("factor", "labelled"))){
      across.prep$new <- as.character(across.prep$new)
    }
  }
  
  
  if(approach == "multiple"){
    
    # Combines any multiple responses as Multiple
    output <- across.prep %>%
      summarize(demo = case_when(n() == 1 ~ new,                           # gave the same response across time
                                 TRUE ~ "Multiple"), .groups = "keep") %>% # If subject > 1 response, they were categorized as Multiple
      unique()                                                         # removes redundant rows; produces unique code for each subject
    names(output)[names(output) == "demo"] <- new.name
    
    return(output)
  }
  
  
  if(approach == "priority"){
    
    ## priority argument check
    if(is.null(priority)){
      
      stop("Must specify priority argument when approach = priority'.")
      
      
    } else if(!all(priority %in% unique(across.prep$new))){
      stop("Values in priority argument must match column names supplied to ...")
    }
    
    if(length(priority) == 1){
      
      
      # Prioritizing 1 response
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority) > 0 ~ priority,   # If subject gave priority at any point, code as priority
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
      
    } else if(length(priority) == 2){
      
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],   # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
      
    } else if(length(priority) == 3){
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],   # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   sum(new == priority[[3]]) > 0 ~ priority[[3]],
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
      
    } else if(length(priority) == 4){
      output <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],   # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   sum(new == priority[[3]]) > 0 ~ priority[[3]],
                                   sum(new == priority[[4]]) > 0 ~ priority[[4]],
                                   n() == 1 ~ new,                        # gave the same response across waves
                                   TRUE ~ "Multiple"), .groups = "keep")  # gave multiple response across waves
    } else {stop("priority argument maximum length is 4.")}
    
    output <- output %>%
      unique()                # removes redundant rows; produces unique code for each subject
    names(output)[names(output) == "demo"] <- new.name
    return(output)
  }
  
  if(approach == "mode"){
    
    ## priority argument check
    if(!is.null(priority)){
      if(!all(priority %in% unique(across.prep$new))){
      stop("Values in priority argument must match column names supplied to ...")
    }}
    
    # The mode response with ties coded as multiple
    if(is.null(priority)){
      
      intermediate <- across.prep %>%
        summarize(demo = case_when(n() == 1 ~ new,                                  # gave the same response across waves
                                   sum(n_time == max(n_time)) > 1 ~ "Multiple",   # ties - gave multiple response with equal frequency
                                   n_time == max(n_time) ~ new,                   # gave multiple response across waves but one at a higher frequency
                                   TRUE ~ "Temp"), .groups = "keep")                # temporary code given to rows that will be removed
      
      
    } else if(length(priority) == 1){
      
      # A priority, then the mode response with ties coded as multiple 
      intermediate <- across.prep %>%
        summarize(demo = case_when(sum(new == priority) > 0 ~ priority,             # If subject gave priority at any point, code as priority
                                   n() == 1 ~ new,                                  # gave the same response across waves
                                   sum(n_time == max(n_time)) > 1 ~ "Multiple",   # ties - gave multiple response with equal frequency
                                   n_time == max(n_time) ~ new,                   # gave multiple response across waves but one at a higher frequency
                                   TRUE ~ "Temp"), .groups = "keep")                # temporary code given to rows that will be removed
      
    } else if(length(priority) == 2){
      
      # A priority, then the mode response with ties coded as multiple 
      intermediate <- across.prep %>%
        summarize(demo = case_when(sum(new == priority[[1]]) > 0 ~ priority[[1]],  # If subject gave priority at any point, code as priority
                                   sum(new == priority[[2]]) > 0 ~ priority[[2]],
                                   n() == 1 ~ new,                                  # gave the same response across waves
                                   sum(n_time == max(n_time)) > 1 ~ "Multiple",   # ties - gave multiple response with equal frequency
                                   n_time == max(n_time) ~ new,                   # gave multiple response across waves but one at a higher frequency
                                   TRUE ~ "Temp"), .groups = "keep")                # temporary code given to rows that will be removed
    } else {stop("When approach = 'mode', the maximum length for the priority argument is 2.")}
    
    output <- intermediate %>%
      filter(demo != "Temp") %>%
      unique()                    # removes redundant rows; produces unique code for each subject
    names(output)[names(output) == "demo"] <- new.name
    return(output)
  }
}


# -------    Confirmatory Factor Analysis Functions   ----------------


#### Extract model information and fit statistics ####
get_lavaan_fits <- function(object, measures = "scaled"){
  
  if(object@Fit@converged == 0){
    stop("Model did not converge and fit statistics cannot be produced.")
  }
  
  if(length(measures) > 1){
    
    indices <- measures
    
  } else if(measures == "scaled"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled', 'rmsea.ci.upper.scaled','srmr')
    
  } else if(measures == "robust"){
    
    indices <- c('npar', 'chisq.scaled', 'df.scaled', 'pvalue.scaled',
                 'cfi.robust',  'rmsea.robust', 'rmsea.ci.lower.robust', 'rmsea.ci.upper.robust', 'srmr')
    
  } else if(measures == "naive"){
    
    indices <- c('npar', 'chisq', 'df', 'pvalue',
                 'cfi', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper', 'srmr')
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

#### wrapper around get_lavaan_fits ####
## gathers fit from multiple models into a single table and readies it for presentation
fits_wrapper <- function(mods.list, type = "scaled", digits = 2){
  
  fit.tab <- purrr::map_dfr(mods.list, ~get_lavaan_fits(.x, measures = type), .id = "Model") %>%
    rename_with(.cols = ends_with(paste0(".",type)), .fn = ~gsub(paste0(".", type), "", .)) %>%  # currently assumes .scaled stats are used
    mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., digits), nsmall = digits))) %>%
    mutate(`90CI` = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
    select(Model, n = ntotal, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, `90CI`, SRMR = srmr)
  
  return(fit.tab)
}

#### Estimating model-based reliability ####
get_relis <- function(object){
  
  relis <- suppressWarnings(semTools::reliability(object))
  
  df <- data.frame(alpha = relis[[2]],
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
      dplyr::filter(op %in% params)
    
  } else if(std %in% c("std.all", "std.lv", "std.nox")){
    
    TheParams <- standardizedSolution(object, type = std) %>%
      dplyr::filter(op %in% params)
    
  } else {
    stop("std argument must be 'no' for unstandardized estimates or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }
  
  
  return(TheParams)
}

## gather factor loadings
get_loadings <- function(model, std, items = NULL){
  
  loads <- extract_lavaan_parameters(model, std = std, params = "=~")
  
  if(std == "no"){
    
    loads <- loads %>%
      select(Item = rhs, est)
    
  } else {
    
    loads <- loads %>% 
      select(Item = rhs, est.std)
  }
  
  if(!is.null(items)){
    
    loads <- loads %>%
      mutate(Item = factor(Item, levels = items)) %>%
      arrange(Item)
  }
  return(loads)
}



# ----- Codebook Functions -------------------


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
                            label_type = "labelled", keep_R_Object = TRUE){
  
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
    
  } else if(export_type=="none"){
    return(Codebook)
    
  } else {
    
    stop("Must specify 'excel', 'csv', or 'none' for export_type argument.")
    
  }

}

dist_plot <- function(scores, means, es, gloc, ncol, scale = NULL){
  
  if(is.null(scale)){
  
  ggplot(data = scores, aes(x = Score)) +
    geom_density(aes(group = Condition, fill = Condition), alpha = .4) +
    geom_vline(data = means, aes(xintercept = Mean, group = Condition, color = Condition), size = 1) +
    geom_text(data = es, aes(x = gloc, y = Inf, vjust = 1.5,
                             label = paste0("g = ", format(round(effect.size, 2), nsmall = 2))), size = 5) +
    ylab("Density") +
    scale_color_discrete(guide = 'none') +
    theme_bw(base_size = 18) +
    facet_wrap(~Scale, scales = "free_y", ncol = ncol) +
    theme(legend.position = "top", legend.title = element_blank())
    
  } else {
    
      ggplot(data = filter(scores, Scale == scale), aes(x = Score)) +
      geom_density(aes(group = Condition, fill = Condition), alpha = .4) +
      geom_vline(data = filter(means, Scale == scale), aes(xintercept = Mean, group = Condition, color = Condition), size = 1) +
      geom_text(data = filter(es, Scale == scale), aes(x = gloc, y = Inf, vjust = 1.5,
                                                                    label = paste0("g = ", format(round(effect.size, 2), nsmall = 2))), size = 5) +
      ylab("Density") +
      scale_color_discrete(guide = 'none') +
      theme_bw(base_size = 18) +
      facet_wrap(~Wave, scales = "free_y", ncol = ncol) +
      theme(legend.position = "top", legend.title = element_blank())
  }
  
}


#' Default flextable format
#'
#' Internal function for formatting flextables
#'
#' @param df a \code{data.frame}
#' @param bold.type character indicating table with a pre-specified bolding pattern. Not currently implemented.
#' @param width numeric; maximum width of table in inches.
#' @param digits integer; number of decimal places to display in the report.
#'
#' @return a \code{flextable} object
#'
#' @import flextable
#'
#' @noRd

flextab_format <- function(df, width = NULL, autofit = TRUE, digits = 2){
  
  numericcols <- which(unlist(lapply(df, is.numeric)))
  
  flex <- flextable::flextable(df)
  flex <- flextable::colformat_double(flex, j = numericcols, digits = digits)
  flex <- flextable::align(flex, i = NULL, j = -1, align = "center", part = "all")
  flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
  flex <- flextable::padding(flex, padding = 0, part = "all")
  
  if(!is.null(width)){
    flex <- flextable::fit_to_width(flex, max_width = width)
  }
  
  if(autofit == TRUE){
  flex <- flextable::autofit(flex)
  }
  
  return(flex)
}


#### Gather parameter estimates from mlm into a presentable table ####
# model - a merMod object
# param.names - character vector of names for parameters
# hl - vector of row numbers where a horizontal line should be added below
get_mlm_params <- function(model, param.names = NULL, logistic = FALSE, flex = FALSE, hl = NULL){
  
  # extract parameters and format
  model.param <- parameters::model_parameters(model) %>%
    mutate(`95% CI` = if_else(is.na(CI_low), NA_character_,
                              paste0("[", format(round(CI_low, 2), nsmall = 2), ", ",
                                     format(round(CI_high, 2), nsmall = 2), "]")),
           p = if_else(p < .001, "< .001", format(round(p, 3), nsmall = 3))) %>%
    select(Variable = Parameter, b = Coefficient, `95% CI`, p)
  
  performance <- performance::model_performance(model)
  
  if(logistic == FALSE){
    model.param <- model.param %>%
      mutate(b = format(round(b, 2), nsmall = 2))
    
    if(class(model) == "lm"){
      model.param <- model.param %>%
        bind_rows(data.frame(Variable = c("n teachers", "R2"),  # add extra rows with info
                             b = c(length(summary(model)$residuals),
                                   format(round(performance$R2_adjusted, 2), nsmall = 2))))
    } else {
      model.param <- model.param %>%
        bind_rows(data.frame(Variable = c("n students", "n teachers", "R2"),  # add extra rows with info
                             b = c(length(summary(model)$residuals),
                                   summary(model)$ngrps[[1]],
                                   format(round(performance$R2_conditional, 2), nsmall = 2))))
                                   # format(round(performance$ICC, 2), nsmall = 2))))
    }
  } else {
    
    model.param <- model.param %>%
      filter(!str_detect(Variable, "\\|")) %>%
      mutate(OR = if_else(!is.na(p), format(round(exp(b), 2), nsmall = 2), NA_character_),
             b = format(round(b, 2), nsmall = 2))
    
    if(class(model) == "lm"){
      
      model.param <- model.param %>%
        bind_rows(data.frame(Variable = c("n teachers", "R2"),  # add extra rows with info
                             b = c(summary(model)$dims$n,
                                   format(round(performance$R2_adjusted, 2), nsmall = 2)))) %>%
        rename(logits = b)
      
    } else {
      
      model.param <- model.param %>%
        bind_rows(data.frame(Variable = c("n students", "n teachers", "R2"),  # add extra rows with info
                             b = c(summary(model)$dims$n,
                                   length(summary(model)$ranef),
                                   format(round(performance$R2_conditional, 2), nsmall = 2)))) %>%
                                   # format(round(performance$ICC, 2), nsmall = 2)))) %>%
        rename(logits = b)
    }
  }
  if(!is.null(param.names)){
    model.param <- model.param %>%
      mutate(Variable = param.names) # rename parameters
  }
  
  if(flex == TRUE){
    # convert to flextable and format
    flex <- flextable(model.param)
    flex <- flextable::align(flex, i = NULL, j = -1, align = "center", part = "all")
    flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
    flex <- flextable::padding(flex, padding = 0, part = "all")
    if(!is.null(hl)){
      flex <- flextable::hline(flex, i = hl, border = officer::fp_border(color="black", width = 2))
    }
    flex <- flextable::autofit(flex)
  } else {
    flex <- model.param
    }
  
  return(flex)
  
}

out_reg <- function(model, digits = 2){
  
  extract_lavaan_parameters(model, params = "~") %>%
    mutate(CI = paste0("[", format(round(ci.lower, digits), nsmall = digits), ", ", format(round(ci.upper, digits), nsmall = digits), "]"),
           b = format(round(est, digits), nsmall = digits),
           b = case_when(pvalue < .01 ~ paste0(b, "**"),
                         pvalue < .05 ~ paste0(b, "*"),
                         pvalue < .10 ~ paste0(b, "^"),
                         TRUE ~ paste0(b, " ")),
           rhs = gsub(".*SCORE_W1$", "Wave 1 Score", rhs)) %>%
    select(lhs, rhs, b, CI) %>%
    gather(temp, value, b, CI) %>%
    unite(col = "new", rhs, temp) %>%
    spread(new, value) %>%
    left_join(
      extract_lavaan_parameters(model, params = "r2", rsquare = TRUE) %>%
        mutate(R2 = format(round(est, digits), nsmall = digits)) %>%
        select(lhs, R2), by = "lhs")
}

#### Running two-wave mediation models using latent change score specification ####
# x, y, z - character strings
# ... passed to lavaan
run_lcs_med <- function(data, x, y, m, pre.y, aux = NULL,
                        ...){
  
  m1 <- paste0("T_", toupper(m), "_SCORE_W1")
  m2 <- paste0("T_", toupper(m), "_SCORE_W2")
  y1 <- paste0(pre.y, toupper(y), "_SCORE_W1")
  y2 <- paste0(pre.y, toupper(y), "_SCORE_W2")
  
  data.temp <- data %>%
    mutate(x = !!sym(x),
           y1 = !!sym(y1),
           y2 = !!sym(y2),
           m1 = !!sym(m1),
           m2 = !!sym(m2))
  
  # LCS specification of two-wave models
  # Code from Valente, Georgeson, and Gonzalez (2021)
  ancova_syntax <- 
    '
  #Defining change in M as a function of M1 and M2
  deltam =~ 1*m2
  deltam ~~ deltam
  deltam ~ 1
  m2 ~ 1*m1
  m2 ~~ 0*m1
  m2 ~~ 0*m2
  m2 ~ 0*1
  m1 ~ 1
  #Defining the change in Y as a function of Y1 and Y2
  deltay =~ 1*y2
  deltay ~~ deltay
  deltay ~ 1
  y2 ~ 1*y1
  y2 ~~ 0*y1
  y2 ~~ 0*y2
  y2 ~ 0*1
  y1 ~ 1
  #Estimating the Pretest correlation between M1 and Y1 and Variance of X
  m1 ~~ sm1y1*y1
  # Estimated covariance between M1 and X and Y1 and X because these covariances may not be equal to zero especially if X is not a randomized experiment
  # without these the model has 2 degrees of freedom (covariances are only constrained to zero) but ANCOVA model should start out as saturated and have 0 degrees of freedom 
  m1 ~~ sm1x*x # these covariances may not be equal to zero especially if X is not a randomized experiment
  x ~~ sy1x*y1 # these covariances may not be equal to zero especially if X is not a randomized experiment
  #Regression of change in M on X and pretest measures
  deltam ~ am2x*x + sm1*m1 + bm2y1*y1
  #Regression of change in Y on X, change in M, and pretest measures
  deltay ~ by2x*x + by2m2*deltam + b*m1 + sy1*y1
  #Making constraints to match estimates to ANCOVA
  #Estimate of effect of M1 on M2 in ANCOVA
  sm := sm1+1 
  #Estimate of effect of Y1 on Y2 in ANCOVA
  sy := sy1+1 
  #Estimate of effect of M1 on Y2 in ANCOVA
  by2m1 := b-by2m2 
  #Estimate of indirect and total effect
  Indirect := am2x*by2m2 
  Total := by2x + (am2x*by2m2)
'
  if(!is.null(aux)){
    
    aux <- aux[aux != y]
    aux.vars <- c(paste0(pre.y, toupper(aux), "_SCORE_W1"), paste0(pre.y, toupper(aux), "_SCORE_W2"))
    ancova <- sem.auxiliary(model = ancova_syntax, aux = aux.vars, data = data.temp, fixed.x = FALSE, ...)
    
  } else {
  ancova <- suppressWarnings(lavaan::sem(model = ancova_syntax, data = data.temp, fixed.x = FALSE, ...))
  }
  
  return(list(model = ancova,
              x = x,
              y = y,
              m = m))
}


#### Gather effect estimates from run_lcs_med ####
out_lcs_med <- function(lcs, recodes, standardize = FALSE, flex = FALSE){
  
  ## extracting model parameters
  params <- parameters::model_parameters(lcs$model, standardize = standardize)
  # R2
  r2 <- extract_lavaan_parameters(lcs$model, params = "r2", rsquare = TRUE) %>%
    filter(lhs == "deltay") %>%
    mutate(R2 = format(round(est, 2), nsmall = 2),
           Effect = "Total") %>%
    select(Effect, R2)
  
  ## formatting for output
  out <- filter(params, Label %in% c(recodes, "Indirect", "Total")) %>%
    mutate(`95% CI` = if_else(is.na(CI_low), NA_character_,
                              paste0("[", format(round(CI_low, 2), nsmall = 2), ", ",
                                     format(round(CI_high, 2), nsmall = 2), "]")),
           p = if_else(p < .001, "< .001", format(round(p, 3), nsmall = 3)),
           b = format(round(Coefficient, 2), nsmall = 2),
           Effect = as_factor(Label) %>% fct_recode(!!!recodes) %>%
             fct_relevel(c(names(recodes), "Indirect", "Total"))) %>%
    arrange(Effect) %>%
    mutate(Effect = as.character(Effect) %>%
             str_replace_all("x", lcs$x) %>%
             str_replace_all("y1", paste(lcs$y, "W1")) %>%
             str_replace_all("y2", paste(lcs$y, "W2")) %>%
             str_replace_all("m1", paste(lcs$m, "W1")) %>%
             str_replace_all("m2", paste(lcs$m, "W2")) %>%
             str_replace_all("deltam", paste(lcs$m, "Change")) %>%
             str_replace_all("deltay", paste(lcs$y, "Change")) %>%
             str_replace_all("_", " ")) %>%
    select(Effect, b, `95% CI`, p) %>%
    left_join(r2, by = "Effect")
  
  if(standardize != FALSE){
    out <- rename(out, b.std = b)
  }
  
  # rsquared
  # lavInspect(model, what = "r2")
  
  if(flex == TRUE){
    # convert to flextable and format
    flex <- flextable(out)
    flex <- flextable::align(flex, i = NULL, j = -1, align = "center", part = "all")
    flex <- flextable::font(flex, fontname = "Times New Roman", part = "all")
    flex <- flextable::padding(flex, padding = 0, part = "all")
    flex <- flextable::autofit(flex) 
  } else {
    flex <- out
  }
  return(flex)
}

