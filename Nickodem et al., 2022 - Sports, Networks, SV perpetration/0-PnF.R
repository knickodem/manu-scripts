#################################
#                               #
#     Paper 2 - Sports & SV     #
#     Packages and Functions    #
#                               #
#################################

#### Packages ####
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



#----- Data Prep and Descriptive Statistics Functions ----------------------


#### Shortcut for converting data from wide to long format by wave ####
# put between subject variables in ...
LongWave <- function(data, wave, ...){
  
  wave.chr <- paste0("_W", wave)
  
  btsubvars <- quos(...)
  
  longdat <- data %>%
    select(!!!btsubvars, ends_with(wave.chr)) %>%
    rename_at(vars(ends_with(wave.chr)), ~str_remove(., wave.chr)) %>%
    mutate(Wave = wave)
  
  return(longdat)
}

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

#### Item Frequencies (including NAs) for multiple items with similar response options ####
Get_ItemFreqs <- function(data, NAto0 = FALSE){
  
  Freqs <- data %>%
    tidyr::gather(Item, Response) %>%
    group_by(Item, Response)%>%
    summarize(n = n(), .groups = "drop")  %>%
    tidyr::spread(Response, n)
  
  if(NAto0 == TRUE){
    
    Freqs <- Freqs %>%
      mutate_if(is.numeric, ~replace_na(., 0))
  }
  return(Freqs)
}

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

# variable, id, and outdegree are character strings for column names
# friend.noms character vector of friend nomination columns
create_friend_variable <- function(data, variable, id, friend.noms, matrix = FALSE, outdegree = NULL){
  
  
  nom.cols <- length(friend.noms)
  n <- nrow(data)
  nom.mat <- matrix(99, n, nom.cols)
  
  for (i in 1:n){ 
    for (j in 1:nom.cols){
      if(is.na(data[i, friend.noms[j]]) == FALSE) {			     # If the friend's id is not missing, then record the following information 
        if(data[i,friend.noms[j]] != data[i, id]){		 # If the friends's id is not their ID, keep going
          nom.mat[i, j] <- data[match(data[i, friend.noms[j]], data[[id]]), variable][[1]] # matches friend id to row number in data and extracts variable value
        }}}}
  
  if(matrix == TRUE){
    return(nom.mat) ## 99 indicates no friend nomination
  } else {
    nom.mat[nom.mat == 99] <- NA
    nom.vec <- rowSums(nom.mat, na.rm = TRUE)
    na.count <- rowSums(is.na(nom.mat))
    nom.vec <- ifelse(na.count == nom.cols, NA, nom.vec)
    
    if(!is.null(outdegree)){
      nom.vec <- nom.vec/outdegree
    }
    return(nom.vec)
  }
}


cond_dist_plot <- function(data, condvar){
  
  cv <- enquo(condvar)
  
  data <- data %>%
    filter(!is.na(!!cv)) %>%
    gather(Outcome, Score, SV_Perp_W4, SH_Perp_W4, SV_Perp_Prior, SH_Perp_Prior) %>%
    mutate(!!cv := as_factor(!!cv),
           Outcome = as_factor(Outcome) %>%
             fct_recode(SV = "SV_Perp_W4", SH = "SH_Perp_W4", `Prior SV` = "SV_Perp_Prior", `Prior SH` = "SH_Perp_Prior"))
  
  plot <- data %>%
    ggplot(aes(x = Score, group = !!cv, fill = !!cv)) +
    geom_density(alpha = .6) +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size = 16) +
    facet_wrap(~Outcome, scales = "free") +
    theme(strip.background = element_blank())
  return(plot)
}

#' Aggregate parameter and stability
#'
#' Internal function for aggregating a parameter over k-folds following Rubin's (1987) rules and calculating the stability of the parameter.
#'
#' @param est vector of parameter estimates
#' @param var vector of variances for \code{est}
#' @param n number of observations per fold
#' @param alpha type I error rate for one-sided test
#' @param output.var include all variance components in the output
#'
#' @details
#' The stability index is calculated as 1 - bv / tv where bv is the between fold variance and tv is the total pooled variance.
#'
#' @return a single row \code{data.frame}

agg_param <- function(est, var, n, alpha = .05, output.var = FALSE){
  
  k <- length(est)
  bar <- mean(est)
  wv <- mean(var)
  bv <- var(est)
  tv <- wv + (1 + k)*bv/k
  stability <- 1 - bv / tv
  
  # degrees of freedom
  rd <- (1 + 1/k) * bv/tv
  dfold <- (k - 1) / rd^2
  dfadj <- (n + 1) / (n + 3) * n * (1 - rd)
  df <- dfold * dfadj / (dfold + dfadj)
  
  # confidence interval
  tcrit <- qt(alpha, df)
  ci.lo <- bar - sqrt(tv) * abs(tcrit)
  ci.hi <- bar + sqrt(tv) * abs(tcrit)
  
  table <- data.frame(parameter = bar, ci.lo = ci.lo, ci.hi = ci.hi, stability = stability)
  
  if(output.var == TRUE){
    table <- cbind(table, data.frame(df = df, var.within = wv, var.between = bv, var.total = tv))
  }
  
  return(table)
}

pool_cluster <- function(clusterobj){
  # code pilfered from miceadds documentation
  
  # extract parameters and covariance matrix
  betas <- lapply(clusterobj, coef)
  vars <- lapply(clusterobj, vcov)
  # conduct statistical inference
  pooling_stats <- pool_mi(qhat = betas, u = vars)
  
  return(list(pooling_stats = pooling_stats,
              glm_stats = summary(pooling_stats)))
}

pool_rsquared <- function(modlist, output.var = FALSE){
  
  n <- nrow(modlist[[1]]$data) # assumes complete data and all datasets have equal sample size; should be a fair assumption when imputation is used
  
  ## calculate for each imputed result and convert to z
  toz <- psych::fisherz(map_dbl(.x = modlist, ~performance::r2(.x)[[1]])) #performance::r2 returns a list rather than a numeric, hence the [[1]]
  
  ## variance formula from: https://bookdown.org/mwheymans/bookmi/pooling-correlation-coefficients-1.html
  poolr <- agg_param(est = toz, var = 1/(n-3), n = n, output.var = output.var) %>%   # pool the z scores
    mutate(r2 = psych::fisherz2r(parameter),
           cil = psych::fisherz2r(ci.lo),
           cih = psych::fisherz2r(ci.hi)) # transform back to r
  
  return(poolr)
  
}

## Shortcut for combining estimates from 4 models - Very project specific
parameter_table <- function(params, r2 = NULL){
  
  params.temp <- map(params, ~.x %>%
                       mutate(across(OR:CI_high, ~format(round(.x, 2), nsmall = 2), .names = "{.col}_temp")) %>%
                       mutate(Parameter = paste0(OR_temp, " [", CI_low_temp, ", ", CI_high_temp, "]", ifelse(p < .05, "*", " "))) %>%
                       select(Variable, Parameter))
  
  if(is.null(r2)){
    
    out <- params.temp$Harassment_RQ %>%
      right_join(params.temp$Harassment_PP,
                 by = "Variable", suffix = c("_SH1", "_SH2")) %>%
      left_join(right_join(params.temp$Violence_RQ, params.temp$Violence_PP,
                           by = "Variable", suffix = c("_SV1", "_SV2")),
                by = "Variable")
    
  } else {
    
    r2.temp <- map(r2, ~.x %>%
                     mutate(across(r2:cih, ~format(round(., 2), nsmall = 2), .names = "{.col}_temp")) %>%
                     mutate(Parameter = paste0(r2_temp, " [", cil_temp, ", ", cih_temp, "] "),
                            Variable = "R2") %>%
                     select(Variable, Parameter))
    
    
    out <- params.temp$Harassment_RQ %>%
      right_join(params.temp$Harassment_PP,
                 by = "Variable", suffix = c("_SH1", "_SH2")) %>%
      left_join(right_join(params.temp$Violence_RQ, params.temp$Violence_PP,
                           by = "Variable", suffix = c("_SV1", "_SV2")),
                by = "Variable") %>%
      bind_rows(right_join(r2.temp$Harassment_RQ, r2.temp$Harassment_PP,
                           by = "Variable", suffix = c("_SH1", "_SH2")) %>%
                  left_join(right_join(r2.temp$Violence_RQ, r2.temp$Violence_PP,
                                       by = "Variable", suffix = c("_SV1", "_SV2")),
                            by = "Variable"))
  }
  
  return(out)
  
}

########################################################

# ----  Latent Variable Model Functions ---------------


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


########################################################


# ------ Variable Information --------------------------

demovars <- c("Transgender", "Gender", "Race", "SexOr",
              "Grade", "Age", "Tx") # Not sure why grade was only measured at W1 (Kelly might have W4 grade)

## Indicators and Scores
snvars <- c("indegree", "outdegree", "betweenness",     # also standardized within school versions (suffix = Std)
            "indegreeStd", "outdegreeStd", "betweennessStd",
            "coreness", "reciprate","egodenout")        # no std option for these (yet)

friendcharvars <- c("ppfrM", "ppfrF", "ppfrT", "ppfrO", "ppfrSameG", # gender
                    "ppfrW", "ppfrH", "ppfrOR", "ppfrSameR",         # race
                    "ppfrS", "ppfrBG", "ppfrOO", "ppfrSameSO",       # sexual orientation
                    "ppSVPer", "ppSVHarPerp", "ppSVConLowPerp",      # SV perpetration (% of friends); ppSVPer not a misspelling
                    "ppSVConHighPerp", "ppfrSameSexPerp",
                    "TotSVPer", "TotSVHarPerp", "TotSVConLowPerp",      # SV perpetration (total # friends)
                    "TotSVConHighPerp",
                    "AveFrDismiss", "AveFrAOD")  # Dismissiveness and substance use attitudes (avg friend score)
                    # "ppSVPer", "ppSVHarVict", "ppSVConLowVict",      # SV Victimization (% of friends)
                    # "ppSVConHighVict",
                    # "TotSVPer", "TotSVHarVict", "TotSVConLowVict",      # SV Victimization (total # friends)
                    # "TotSVConHighVict")

trustedadultvars <- c("TotAdultNoms", "TotAdultsFr", "TotAd1Step")
adulttypevars <- c("TotNumANoms", "TotNumCNoms", "TotNumHNoms",
                      "TotNumPNoms", "TotNumSNoms", "TotNumTNoms") # we will eventual add nominated coaches

## Item-level
nocontactperpvars <- paste("SEX_VIOL_PERP", 1:4, sep = "_")
contactperpvars <- paste("SEX_VIOL_PERP", 5:13, sep = "_") # S_SEX_VIOL_PERP_W4
dismissvars <- paste("DISMISS_SEX_VIOL", 1:6, sep = "_") # S_DISMISS_SEX_VIOL_W4
substanceusevars <- paste("AOD_NEXT6_MTHS", 1:4, sep = "_") #S_AOD_NEXT6_MTHS_W4
# nocontactvictvars <- paste("SEX_VIOL_VICT", 1:4, sep = "_")
# contactvictvars <- paste("SEX_VIOL_VICT", 5:13, sep = "_")


##############################################################################



#### Extracting labels from a factor and/or character or labelled variable ####
## labelled returns both the label and the value, otherwise only the label is returned
Get_Labels <- function(data,type=c("factor","character","labelled","factor_character")){
  
  if(type=="labelled"){
    
    ## Select labelled variables, extract labels, and transform to long format dataframe
    all_labels <- data %>% select_if(is.labelled) %>%
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
    
  } else if(export_type!="none"){
    
    stop("Must specify 'excel', 'csv', or 'none' for export_type argument.")
    
  }
  
  if(keep_R_Object==TRUE){
    return(Codebook)
  }
  
}

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

model_check <- function(model, ...){
  
  perform <- ModelCheckPlots(model, ...)
  
  perform$performance <- cbind(data.frame(obs = length(model$residuals)), model_performance(model))
  perform$collinearity <- check_collinearity(model)
  
  
  return(perform)
}
