## General functions used throughout analysis go here.  Also, load the packages
## that you want
pacman::p_load(lubridate,readxl,haven,pipeline,readr,
               tidyr,stringr,dplyr,ggplot2,forcats,
               purrr,tibble,rlang,broom,MuMIn,QME,
               lavaan,semTools,XLConnect,haven)

################################################
#### Confirmatory Factor Analysis Functions ####

#### Gather fit statistics from a CFA or multiple CFAs that have been saved in a list into a single dataframe ####
Get_Fits <- function(list,fittype="scaled",pooled=FALSE){
  #fittype accepts "naive","scaled","robust"
  #if pooled=TRUE then list must be the vector of fit statistics obtained from semTools::anova(model,test="D2",indices=TRUE,...)
  #Note: ... above could include pool.robust=TRUE,scale.W=FALSE
  
  if(fittype=="scaled"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.scaled', 'rmsea.scaled', 'srmr')
  } else if(fittype=="robust"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.robust', 'rmsea.robust', 'srmr')
  } else if(fittype=="naive"){
    indices <- c('chisq','df','pvalue','cfi', 'rmsea', 'srmr')
  }
  
  if(pooled==TRUE){
    indices[6] <- "srmr_bentler"
  }
  
  InternalFits <- function(mod,ind){
    fit <- as.data.frame(round(t(c(fitMeasures(mod,ind))),3))
    names(fit) <- c("chisq","df","pvalue","CFI","RMSEA","SRMR")
    fit <- fit %>% mutate(n=inspect(mod,"nobs"),
                          X2 = paste0(chisq," (",round(df),")")) %>%
      select(n,X2,CFI,RMSEA,SRMR)
    
    if(!is.na(mod@loglik$loglik)){
      fit <- fit %>%
        mutate(AIC = round(AIC(mod),1),
               BIC = round(BIC(mod),1))
      
    }
    return(fit)
  }
  
  if(!is.list(list)){
    if(pooled==FALSE){
      
      AllFits <- InternalFits(list,indices)
      
    } else {
      
      AllFits <- list %>%
        as.data.frame() %>% rownames_to_column("Index") %>%
        filter(Index %in% indices) %>%
        mutate(Index = c("chisq","df","pvalue","CFI","RMSEA","SRMR"),
               Value = round(`.`,3)) %>%
        select(Index,Value) %>% spread(Index,Value) %>%
        mutate(X2 = paste0(chisq," (",round(df),")")) %>%
        select(X2,CFI,RMSEA,SRMR)
    }
    
  } else {
    
    AllFits <- data.frame()
    for(i in names(list)){
      
      if(pooled==FALSE){
        
        fit <- InternalFits(list[[i]],indices) %>%
          mutate(Model=i) %>%
          select(Model,everything())
        
      } else {
        
        fit <- list[[i]] %>%
          as.data.frame() %>% rownames_to_column("Index") %>%
          filter(Index %in% indices) %>%
          mutate(Model = i,
                 Index = c("chisq","df","pvalue","CFI","RMSEA","SRMR"),
                 Value = round(`.`,3)) %>%
          select(Model,Index,Value) %>%
          spread(Index,Value) %>%
          mutate(X2 = paste0(chisq," (",round(df),")")) %>%
          select(Model,X2,CFI,RMSEA,SRMR)
        
      }
      
      AllFits <- bind_rows(AllFits,fit)
    }
  }
  
  return(AllFits)
}


#### Get alpha, 3 versions of omega, and average variance extracted for the scale for each factor and total ####
# requires semTools
Get_ScaleReliabilities <- function(model,digits=3){
  
  modrel <- suppressWarnings(reliability(model)) %>% 
    data.frame() %>% tibble::rownames_to_column(var="Coefficient") %>%
    rename(Total=total) %>% mutate_if(is.numeric,round,digits=digits) %>%
    mutate(Coefficient=c("alpha","conditional omega","unconditional omega",
                         "hierarchical omega","avg. var. extracted"))
}

#### Gather reliabilities from multiple scales into a single dataframe ####
## Requires loading Get_ScaleReliabilities
Get_Relis <- function(list){
  
  AllRelis <- data.frame()
  
  for(i in names(list)){
    
    ## Fit indices
    
    reli <- list[[i]] %>% Get_ScaleReliabilities() %>% 
      select(Coefficient,Total) %>%
      spread(Coefficient,Total) %>% mutate(Model=i) %>%
      select(Model,alpha,`conditional omega`,`unconditional omega`,
             `hierarchical omega`,`avg. var. extracted`)
    
    AllRelis <- bind_rows(AllRelis,reli)
  }
  return(AllRelis)
  
}

#### Print detailed results of a CFA or SEM model ####
PrintSEM <- function(model,reliabilities=FALSE,fittype="scaled",r2=FALSE,items=NULL){
  ## May require Get_ScaleReliabilities and Get_Fits functions to be loaded.
  # Dataframes can be supplied to reliabilities, fittype (for fit indices), r2, and items arguments.
  # For lavaan.mi models, supplying dataframes to reliabilities and fittype is the only option.
  
  if(class(model)=="lavaan"){
    
    ## Reliabilities
    if(reliabilities==TRUE){
      reli <- Get_ScaleReliabilities(model,digits=3)
    } else if(reliabilities!=FALSE){
      reli <- reliabilities
    }
    
    ## R2
    if(r2==TRUE){
      rsq <- inspect(model,"r2") %>% data.frame() %>%
        tibble::rownames_to_column(var="Item") %>%
        rename(R2 = `.`) %>% mutate(R2 = round(R2,3))
    } else if(r2!=FALSE){
      rsq <- r2 
    }
    
    ## Fit indices
    fitin <- Get_Fits(model,fittype=fittype,pooled=FALSE)
    
    ## Model Parameters
    params.std <- standardizedSolution(model)
    params <- parameterestimates(model)
    
    ## Factor Loadings
    lv.std <- params.std %>% filter(op=="=~") %>% select(Factor=lhs,Item_Code=rhs,Std.Est=est.std)
    lv <- params %>% filter(op=="=~") %>%
      select(Factor=lhs,Item_Code=rhs,Estimate=est,SE=se,z,pvalue) %>%
      left_join(lv.std,by=c("Factor","Item_Code"))
    
    ## Variances and Covariances
    vari <- params %>% filter(op=="~~") %>%
      select(ItemOrFactor1=lhs,ItemOrFactor2=rhs,Estimate=est,SE=se,z,pvalue)
    
    ## Regressions
    reg.std <- params.std %>% filter(op=="~") %>% select(Outcome=lhs,Predictor=rhs,Std.Est=est.std)
    reg <- params %>% filter(op=="~") %>%
      select(Outcome=lhs,Predictor=rhs,Estimate=est,SE=se,z,pvalue) %>%
      left_join(reg.std,by=c("Outcome","Predictor"))
    
  } else if(class(model)=="lavaan.mi"){
    
    ## Reliabilities
    if(reliabilities!=FALSE){
      reli <- reliabilities
    }
    
    ## Fit indices - must have previously run Get_Fits and provided results in fittype argument
    fitin <- fittype
    
    ## Model Parameters
    Allparams <- summary(model,standardized=TRUE,add.attributes=FALSE,rsquare=TRUE)
    
    ## Factor Loadings
    lv <- Allparams %>% filter(op=="=~") %>%
      select(Factor=lhs,Item_Code=rhs,Estimate=est,SE=se,t,pvalue,Std.Est=std.all)
    
    ## Variances and Covariances
    vari <- Allparams %>% filter(op=="~~") %>%
      select(ItemOrFactor1=lhs,ItemOrFactor2=rhs,Estimate=est,SE=se,Std.Est=std.all)
    
    ## Regressions
    reg <- Allparams %>% filter(op=="~") %>%
      select(Outcome=lhs,Predictor=rhs,Estimate=est,SE=se,t,pvalue,Std.Est=std.all)
    
    ## R2
    if(r2==TRUE){
      rsq <- Allparams %>% filter(op=="r2") %>%
        select(Outcome=lhs,R2=est)
    } else if(r2!=FALSE){
      rsq <- r2 
    }
    
  } else {
    stop("Can't do that yet")
  }
  
  
  ## Table formatting
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('keep.trailing.zeros', TRUE)
  panderOptions('round', 3)
  panderOptions('table.split.table',Inf)
  
  ## Printing
  cat("\n\n")
  if(!is.null(items)){
    cat("#### Items\n\n")
    pander(items)
    cat("\n\n")
  }
  if(reliabilities!=FALSE){
    cat("#### Scale Reliabilities\n\n")
    pander(reli)
    cat("\n\n")
  }
  cat("#### Fit Measures\n\n")
  pander(fitin)
  cat("\n\n")
  cat("#### Factor Loadings\n\n")
  pander(lv)
  cat("\n\n")
  cat("#### Variances\n\n")
  pander(vari)
  cat("\n\n")
  if(nrow(reg)>0){
    cat("#### Regression\n\n")
    pander(reg)
    cat("\n\n")
  }
  if(r2!=FALSE){
    cat("R-squared\n\n")
    pander(rsq)
    cat("\n\n")
  }
}

#######################################
#### Correction to the QME package ####
distractor_analysis2 <- function(testQME, ...) {
  raw_test = QME:::getRawTestNoID(testQME)
  num_items = ncol(raw_test)
  
  keyed = QME:::getKeyedTestNoID(testQME)
  
  key = QME:::getKey(testQME)
  
  ## persons by items matrix of corrected scores
  scores = rowSums(keyed)
  delscores = scores - keyed
  
  
  distractor = lapply(1:num_items, function(i) { 
    ## Calculate distractor analysis table
    
    difficulty = prop.table(table(raw_test[i]))
    choices = rownames(difficulty)
    keys = key[match(choices, key$response), 1 + i]
    keys = ifelse(is.na(keys),0,keys)
    names(keys) = choices
    
    ## data frame of responses to item i, item-deleted total score
    new = data.frame(response = raw_test[ , i], 
                     corrected_score = delscores[ , i],
                     keyed = keyed[, i])
    new = na.omit(new)
    
    ## Calculate 0-1 indicators for each distractor Other distractors (e.g. key =
    ## 0) are considered MISSING and EXCLUDED (Attali, 2000)
    if(length(choices) > 1) {
      indicators = model.matrix(corrected_score ~ 0 + response, data = new)
      ## rm "response" prefix 
      colnames(indicators) = sub("response", "", colnames(indicators))
      
      indicators = data.frame(indicators, check.names = FALSE)
    } else {
      ## model.matrix fails when there is no variability
      indicators = data.frame(rep(NA, nrow(new)))
      names(indicators) = choices
    }
    
    stopifnot(choices == names(indicators))
    
    distractors = names(keys)[keys == 0]
    
    ## Exclude other incorrect distractors from calculation
    if(length(distractors) > 0) {
      to_exclude = apply(indicators[, distractors, drop = FALSE], 2, function(x) {
        ## return TRUE if not this distractor AND is incorrect
        x == 0 & new$keyed == 0
      })
      
      indicators[, distractors][to_exclude] = NA
    }
    
    ## Calculate correlations of indicators with corrected score
    distractors_discrim = cor(new$corrected_score, 
                              indicators,
                              use = "pairwise.complete.obs")[1,]
    
    out = data.frame(Choice = choices,
                     Key = keys,
                     Proportions = as.numeric(difficulty),
                     `Response Discrimination` = as.numeric(distractors_discrim),
                     check.names = FALSE)
    
    row.names(out) = NULL
    
    out
  }) 
  
  names(distractor) = names(raw_test)
  
  return(distractor)
}


analyze2 <- function(test, key = NULL, create_key = FALSE, id = TRUE, d = 2, use = "pairwise.complete.obs", na_to_0 = TRUE){
  
  # Preliminaries: score & get keyed test
  q1 = QME::QMEtest(test, key = key, id = id, na_to_0 = na_to_0, create_key = create_key)
  keyed_test = QME:::getKeyedTest(q1)
  keyed_test_no_id = QME:::getKeyedTestNoID(q1)
  
  # Get output of middle manager functions
  test_level = list(
    descriptives = summary(q1),
    reliability = QME:::reliability(q1, use = use)
  )
  item_level = list(
    item_stats = QME:::item_level(q1, use = use),
    missing = QME:::miss(QME:::getRawTestNoID(q1))
  )
  
  ## Add distractor analysis only if test is unkeyed
  
  if(is.null(key) & !create_key)
    item_level$distractor_analysis = NULL
  else
    item_level$distractor_analysis = distractor_analysis2(q1)
  
  scores = QME:::getTotalScores(q1)
  
  # Gather it all up in a list
  oz = list(
    test_name = deparse(substitute(test)),
    number_items = ncol(keyed_test_no_id),
    number_examinees = nrow(keyed_test_no_id),
    total_scores = scores,
    test_level = test_level,
    item_level = item_level,
    test = q1
  )
  class(oz) = "analyze"
  
  return(oz)
}
##########################