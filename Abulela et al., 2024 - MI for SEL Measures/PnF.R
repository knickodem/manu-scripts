#############################################
#                                           #
#   MI of SEL Measures Across Time Points   #
#           Packages and Functions          #
#                                           #
#############################################

# Packages
library(dplyr)
library(lavaan)
library(flextable)
library(purrr)
library(ggplot2)
library(forcats)

#### Scale item shortcuts ####
## developmental skills
ctl.items <- c(paste0("Y", 18:19), "Y20r", paste0("Y21", letters[1:3]))
pio.l1.items <- c("W57L1a", paste0("Y60", letters[c(2,6:8)]), "W57L1n")
pio.l23.items <- paste0("Y60", letters[c(1,2,6:8,14)])
sc.items <- paste0("Y60", letters[c(3:5,9:11,13,17)])

## developmental supports
emp.items <- c(paste0("Y22", letters[c(1:3)]), paste0("Y60", letters[c(12,15,16)]))
fcs.items <- c(paste0("Y59", letters[c(1:3,5)]))
tss.items <- c(paste0("Y21", letters[c(4:8)]), "Y59d")

l23.scales <- list(`Commitment to Learning` = ctl.items,
                   `Positive Identity and Outlook` = pio.l23.items,
                   `Social Competence` = sc.items,
                   `Empowerment` = emp.items,
                   `Family/Community Support` = fcs.items,
                   `Teacher/School Support` = tss.items)

l1.scales <- l23.scales
l1.scales$`Positive Identity and Outlook` <- pio.l1.items

abb.scales <- c("CTL", "PIO", "SC", "EMP", "FCS", "TSS")
names(abb.scales) <- names(l1.scales)


# -----   Custom Functions ------

#' Calculate scale score
#'
#' Combine item responses into a scale score
#'
#' @param data a \code{data.frame}
#' @param items character vector of column names in \code{data} to use in the scale
#' @param type should the score be the \code{"sum"} or the \code{"mean"} of item responses?
#' @param min.valid the minimum number of valid responses to receive a score. The default is 1, otherwise NA is returned.
#'
#' @return numeric vector
#'
#' @examples
#' set.seed(9876)
#' data <- data.frame(x1 = sample(0:4, 20, replace = TRUE),
#'                  x2 = sample(0:4, 20, replace = TRUE),
#'                  x3 = sample(0:4, 20, replace = TRUE),
#'                  x4 = sample(0:4, 20, replace = TRUE))
#'
#' sum.score <- scale_score(data, paste0("x", 1:4), type = "sum")
#'
#' ## Incorporate missing data
#' dfmiss <- apply(data, 2, function(x){
#'                 x[sample(c(1:nrow(df)), floor(nrow(df)/10))] <- NA
#'                 x})
#' dfmiss <- as.data.frame(dfmiss)
#'
#' # Score NA if missing on more than 1 item
#' mean.score <- scale_score(dfmiss, paste0("x", 1:4), type = "mean", min.valid = 3)
#'
#' @export
scale_score <- function(data, items = names(data) , type = c("sum", "mean"), min.valid = 1){
  
  if(type == "sum"){
    score <- rowSums(data[, items], na.rm = TRUE)
  } else if(type == "mean"){
    score <- rowMeans(data[, items], na.rm = TRUE)
  }
  
  nacount <- rowSums(is.na(data[, items]), na.rm = TRUE)
  score <- ifelse((length(items) - nacount) < min.valid, NA, score)
  
  return(score)
}

#### wrapper to generate syntax for configural and constrained measurement invariance models ####
## This is mostly a way to put ugly code in the function and reduce copy/paste errors in analysis script
## Currently organized for ordinal variables a la Svetina, Rutkowski, & Rutkowski (2020)
## base - lavaan syntax defining the baseline model
## data - data.frame containing variables represented in base and grouping variable
## group - grouping variable
## mi.settings - list of other arguments to pass to measEq.syntax
generate_mi_syntax <- function(base, data, group, mi.settings){
  
  ng <- length(unique(data[[group]]))
  na <- paste(rep(NA, ng-1), collapse = ", ")
  ones <- paste(rep(1, ng), collapse = ", ")
  zeros <- paste(rep(0, ng), collapse = ", ")
  
  thesyntax <- list(Configural = do.call(semTools::measEq.syntax,
                                         args = c(list(configural.model = base, data = data,
                                                       group = group, group.equal = "configural"),
                                                  mi.settings)) %>% as.character(),
                    Threshold = do.call(semTools::measEq.syntax,
                                        args = c(list(configural.model = base, data = data,
                                                      group = group, group.equal = c("thresholds")),
                                                 mi.settings)) %>% as.character(),
                      # gsub(paste0("~\\*~ c\\(1, ", na, "\\)"), paste0("~*~ c(", ones ,")"), .),# keeping the scaling factor constrained to 1 (estimating latent response variance)
                    Loading = do.call(semTools::measEq.syntax,
                                      args = c(list(configural.model = base, data = data,
                                                    group = group, group.equal = c("thresholds", "loadings")),
                                               mi.settings)) %>% as.character())
  thesyntax$Means <-  thesyntax$Loading %>%
    sub(paste0("c\\(", zeros,"\\)\\*1 \\+ c\\(alpha"), paste0("c(0, ", na,")*1 + c(alpha"), .) %>% # freeing latent factor mean in groups 2+
    gsub(paste0("c\\(0, ", na, "\\)\\*1 \\+ c\\(nu"), paste0("c(", zeros, ")*1 + c(nu"), .) # constrain latent response mean to 0 to indentify model
  
  return(thesyntax)
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


#### Gathers fit from multiple models into a single table and readies it for presentation ####
## wrapper around get_lavaan_fits
# mods.list - list of lavaan objects
# type      - "scaled" or "robust"; passed to measures argument in get_lavaan_fits
# digits    - number of digits to round numeric columns
fits_wrapper <- function(mods.list, type = "scaled", digits = 2){
  
  fit.tab <- purrr::map_dfr(mods.list, ~get_lavaan_fits(.x, measures = type), .id = "Model") %>%
    rename_with(.cols = ends_with(paste0(".",type)), .fn = ~gsub(paste0("\\.", type), "", .)) %>%  # currently assumes .scaled stats are used
    mutate(across(.cols = c(chisq, pvalue:srmr), .fn = ~format(round(., digits), nsmall = digits))) %>%
    mutate(`90CI` = paste0("[", rmsea.ci.lower, ", ", rmsea.ci.upper, "]")) %>%
    select(Model, n = ntotal, ngroups, x2 = chisq, df, p = pvalue, CFI = cfi, RMSEA = rmsea, `90CI`, SRMR = srmr)
  
  return(fit.tab)
}

#### Compute RMSEA_D (Savalei et al., 2023) ####
# mod0 - lavaan object for the less constained model
# mod1 - lavaan object for the more constrained model (i.e., mod1 is nested within mod0)
# alpha - Type I error for upper confidence limit. The value will be replicated for the lower limit.
#        The default is .05, which produces the 90% confidence interval. Use .025 for 95% CI.
RMSEAd <- function(mod0, mod1, alpha = .10/2){
  
  ind0 <- get_lavaan_fits(mod0, measures = c("chisq", "df", "chisq.scaling.factor"))
  ind1 <- get_lavaan_fits(mod1, measures = c("chisq", "df", "chisq.scaling.factor"))
  n <- ind1$ntotal # could use ind0 too
  g <- ind1$ngroups

  D <- ind1$chisq - ind0$chisq
  dfD <- ind1$df - ind0$df
  if(D < dfD){
    rmsead <- 0
    lower <- NA
    upper <- NA
    warning("D = ", D, " and dfD = ", dfD, "; when D < dfD, RMSEAd is set to 0.")
    # Warning is based on statement from Savalei et al (2023) on p.3.
  } else{
  
  rmsead <- sqrt((D - dfD) / (dfD*(n - g)/g))
  
  Dlim <- MBESS::conf.limits.nc.chisq(Chi.Square = D, df = dfD, conf.level = NULL, alpha.lower = alpha, alpha.upper = alpha)
  lower <- sqrt((Dlim$Lower.Limit - dfD) / (dfD*(n - g)/g))
  upper <- sqrt((Dlim$Upper.Limit - dfD) / (dfD*(n - g)/g))

  }
  return(data.frame(RMSEAd = rmsead, lower = lower, upper = upper))
}

#' Compute RMSEA_D
#'
#' Function to compute RMSEA_D (Savalei et al., 2023) from nested multigroup models in lavaan (Rosseel, 2012).
#' The function also computes the 90% confidence interval and peforms an equivalence test based on a cutoff value.
#' Incorporates code from Savalei et al., 2023 supplemental materials found here: https://osf.io/2ycd6/
#'
#' @param mod0 lavaan object for the less constained model
#' @param mod1 lavaan object for the more constrained model (i.e., mod1 is nested within mod0)
#' @param cutoff The cutoff value for which to perform an equivalence test using the 90% confidence interval.
#' When the upper bound of the 90% CI is below \code{cutoff} the null hypothesis that RMSEA_D > cutoff is statistically significant at alpha = .05


RMSEA_D <- function(mod0, mod1, cutoff = .10){
  
  # suggested to use default chisqure difference from LRT rather than
  # computing by hand from fit indices: https://groups.google.com/g/lavaan/c/6kiW0a_Y64A/m/0HUnVGaZAwAJ
  comp <- lavaan::lavTestLRT(mod0, mod1)
  D <- comp$`Chisq diff`[[2]]
  dfD <- comp$`Df diff`[[2]]
  n = lavaan::inspect(mod0, "ntotal")
  g = lavaan::inspect(mod0, "ngroups")
  
  
  if(D < dfD){
    RMSEA_D <- 0
    ci <- c(NA, NA, NA)
    warning("D = ", D, " and dfD = ", dfD, "; when D < dfD, RMSEAd is set to 0.")
    # Warning is based on statement from Savalei et al (2023) on p.3.
  } else{
    
    RMSEA_D <- sqrt((D - dfD) / (dfD*(n - g)/g))
    
    ci <- RMSEA.CI(D, dfD, n, g, cutoff)
    
  }
  return(data.frame(RMSEA_D = RMSEA_D, Lower_CI = ci[[1]], Upper_CI = ci[[2]], p = ci[[3]]))
}

# adapted from https://osf.io/ne5ar
RMSEA.CI <- function(T, df, N, G, cutoff = .10){
  
  #functions taken from lavaan (lav_fit_measures.R)
  lower.lambda <- function(lambda) {
    (pchisq(T, df=df, ncp=lambda) - 0.95)
  }
  upper.lambda <- function(lambda) {
    (pchisq(T, df=df, ncp=lambda) - 0.05)
  }
  
  #RMSEA CI
  lambda.l <- try(uniroot(f = lower.lambda, lower = 0, upper = T)$root, silent=TRUE)
  if(inherits(lambda.l, "try-error")) { lambda.l <- NA; RMSEA.CI.l<-NA
  } else { if(lambda.l < 0){
    RMSEA.CI.l = 0
  } else {
    RMSEA.CI.l <- sqrt(lambda.l*G/((N-1)*df))
  }
  }
  
  N.RMSEA <- max(N, T*4)
  lambda.u <- try(uniroot(f=upper.lambda, lower=0,upper=N.RMSEA)$root,silent=TRUE)
  if(inherits(lambda.u, "try-error")) { lambda.u <- NA; RMSEA.CI.u<-NA
  } else { if(lambda.u<0){
    RMSEA.CI.u=0
  } else {
    RMSEA.CI.u<-sqrt(lambda.u*G/((N-1)*df))
  }
  }
  
  # computing  p-value
  eps0<-df*cutoff^2/G
  nonc<-eps0*(N-G)
  pval<-pchisq(T,df=df,ncp=nonc)
  
  RMSEA.CI <- c(RMSEA.CI.l,RMSEA.CI.u, pval)
  return(RMSEA.CI)
}


#### Gathers the multiple ways to compare models into one table ####
# mod0 - lavaan object for the less constained model
# mod1 - lavaan object for the more constrained model (i.e., mod1 is nested within mod0)
# method - passed to lavaan::lavTestLRT
# measures - passed to measures argument of get_lavaan_fits
# RMSEAd - should RMSEAd be calculated?
# digits - number of digits to round numeric columns
compare_mods <- function(mod0, mod1, method = "default",
                         measures = c("cfi", "rmsea", "srmr"),
                         RMSEA_D = TRUE, digits = 3){
  
  aov01 <- lavaan::lavTestLRT(mod0, mod1, method = method)[2,c(5:7)]
  
  # Uses non-scaled version
  ind0 <- get_lavaan_fits(mod0, measures = measures)
  ind1 <- get_lavaan_fits(mod1, measures = measures)
  ind.diff <- ind1 - ind0
  
  mi.com <- cbind(aov01, ind.diff)
  if(RMSEA_D){
    mi.com <- cbind(mi.com, RMSEA_D(mod0, mod1))
  }
  
  mi.com <- format(round(mi.com, digits), nsmall = digits)
  row.names(mi.com) <- NULL
  # names(mi.com) <- c("delta.chisq", "delta.df", "delta.pvalue", "delta.cfi", "delta.rmsea", "delta.srmr")
  
  return(mi.com)
}

#### Extract standardized or unstandardized parameter estimates ####
# object - lavaan object
# std - "no" (default) for unstandardized estimates. Otherwise, one of c("std.all", "std.lv", "std.nox"),
#        passed to type argument in standardizedSolution
# params - "all" (default) or character vector of desired parameters, such as c("=~", "~~", "~*~", "~1", "~", "|", "<~")
# ... - additional arguments passed to lavaan::parameterEstimates
extract_lavaan_parameters <- function(object, std = "no", params = "all", ...){
  
  if("all" %in% params){
    
    params = c("=~", "~~", "~*~", "~1", "~", "|", "<~")
    
  }
  
  if(std == "no"){
    
    TheParams <- lavaan::parameterEstimates(object, standardized = FALSE, ...) %>%
      dplyr::filter(op %in% params)
    
  } else if(std %in% c("std.all", "std.lv", "std.nox")){
    
    TheParams <- lavaan::standardizedSolution(object, type = std) %>%
      dplyr::filter(op %in% params)
    
  } else {
    stop("std argument must be 'no' for unstandardized estimates or one of 'std.all', std.lv', or 'std.nox' for standardized estimates")
  }
  
  return(TheParams)
}

#### Gathers factor loadings from multiple models into a single table and readies it for presentation ####
## wrapper around extract_lavaan_parameters
# mods.list - list of lavaan objects
# items - character vector of items in order to arrange datafraem
loadings_wrapper <- function(mods.list, items){
  loadings <- extract_lavaan_parameters(mods.list$Threshold, std = "no", params = "=~") %>%
    select(group, item = rhs, est) %>%
    tidyr::spread(group, est) %>%
    inner_join(extract_lavaan_parameters(mods.list$Loading, std = "no", params = "=~") %>%
                 select(item = rhs, Constrained = est) %>%
                 mutate(Constrained = round(Constrained, 5)) %>%
                 unique(), by = "item") %>%
    mutate(item = factor(item, levels = items)) %>%
    arrange(item)
  return(loadings)
}

#### compare group means and variances ####
# model - lavaan object to extract means;
#          should be model with constrained thresholds/intecepts and loadings, and free latent factor means (except for group 1)
# factor - name of the latent factor(s)
grp_mvs <- function(model, factor){
  
  ## Equal to Glass's delta when group 1 latent factor(s) mean and variance is 0 and 1
  delta <- extract_lavaan_parameters(model, std = "no", params = "~1") %>%
    dplyr::filter(lhs == factor)
  
  vars <- extract_lavaan_parameters(model, std = "no", params = "~~") %>%
    dplyr::filter(lhs == factor)
  
  ## Not clear yet what kind of standardizing this is
  std.means <- extract_lavaan_parameters(model, std = "std.all", params = c("~1")) %>%
    dplyr::filter(lhs == factor)
  
  ## This part is project specific and needs to be generalized (or removed for other projects)
  grp.lvs <- lavaan::lavInspect(model, what = "group.label")
  names(grp.lvs) <- c(1:length(grp.lvs))
  mean.var <- delta %>% select(Year = group, mean = est, mean_se = se) %>%
    left_join(select(vars, Year = group, var = est, var_se = se), by = "Year") %>%
    left_join(select(std.means, Year = group, std_mean = est.std, std_se = se), by = "Year") %>%
    mutate(Year = recode(Year, !!!grp.lvs))
           #`1` = "2013", `2` = "2016", `3` = "2019", `4` = "2022")
  
  return(mean.var)
}

#### Identify possible parameter constraints to release to achieve partial invariance ####
## extracts modification indices and organizes in order of size
# model - lavaan object
# cutoff - subset results to modificiation indices > cutoff. Default is 4.
partial_params <- function(model, cutoff = 4){
  
  ## identifying the parameters with MI > cutoff
  partials <- lavaan::lavTestScore(model, cumulative = TRUE)$uni %>%
    dplyr::filter(X2 > cutoff) %>%
    rename(plabel = lhs) %>%
    select(plabel, X2)
  
  ## gathering additional info about the parameter into final table
  tab <- dplyr::filter(parTable(model), plabel %in% partials$plabel) %>%
    select(plabel, lhs, op, rhs, label) %>%
    left_join(partials, by = "plabel") %>%
    arrange(desc(X2))
  
  return(tab)
}



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
  flex <- flextable::italic(flex, i = 2, italic = TRUE, part = "header")
  flex <- flextable::padding(flex, padding = 1, part = "all")
  flex <- flextable::autofit(flex)
  return(flex)
}
flex.border <- officer::fp_border(width = 2) # manual horizontal flextable border width

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
