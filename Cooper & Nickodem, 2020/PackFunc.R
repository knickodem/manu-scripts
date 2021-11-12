##############################################################################
#                                                                            #
#    Cumulative Lifetime Adversities in first generation Latinx population   #
#                          Late July 2019                                    #
#                                                                            #
##############################################################################


#### Packages ####
pacman::p_load(haven,readr,tidyr,stringr,
               dplyr,tibble,forcats,purrr,
               lavaan,broom,semTools)


##########################
####     Functions    ####

#### Extracting labels from a factor and/or character or labelled variable ####
## labelled returns both the label and the value, otherwise only the label is returned
Get_Labels <- function(data,type=c("factor","character","labelled","factor_character")){
  
  if(type=="labelled"){
    
    ## Select labelled variables, extract labels, and transform to long format dataframe
    all_labels <- data %>% select_if(is.labelled) %>%
      purrr::map(~attr(.,"labels")) %>% tibble::enframe("Item","Value") %>% 
      mutate(Label = purrr::map(Value,~attr(.,"names"))) %>% unnest()
    
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
  Codebook <- left_join(Stems,VLW,by="Item") %>%
    mutate(Stem = as.character(Stem))
  
  
  if(export_type == "excel"){
    
    ## Exporting to excel file
    cb <- loadWorkbook(paste0(export_name, ".xlsx") ,create=TRUE)
    createSheet(cb,export_name)
    writeWorksheet(cb, Codebook, export_name)
    saveWorkbook(cb)
    
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


#### Getting summary descriptives on a continuous variable ####
## unquoted grouping variables should be specified in ...
# requires semTools for skew and kurtosis
Get_Descriptives <- function(data,ContinuousVariable,...,digits=5,AllContinuous=TRUE){
  
  groups <- quos(...)
  CV <- enquo(ContinuousVariable)
  
  data_descrip <- data %>% group_by(!!!groups) %>%
    summarize(n = sum(!is.na(!!CV)),
              Median = median(!!CV, na.rm=TRUE),
              Mean = mean(!!CV, na.rm=TRUE),
              SE = sd(!!CV, na.rm=TRUE)/sqrt(sum(!is.na(!!CV))),
              SD = sd(!!CV, na.rm=TRUE),
              Min = min(!!CV, na.rm=TRUE),
              Max = max(!!CV, na.rm=TRUE),
              Skew = suppressWarnings(skew(!!CV)[[1]]),
              Kurtosis = suppressWarnings(kurtosis(!!CV)[[1]])) %>% ungroup()
  
  if(AllContinuous==FALSE){
    
    data_descrip <- data_descrip %>%
      mutate(SE = ifelse(Min==0 & Max==1,(Mean*(1-Mean))/sqrt(n),SE),
             SD = ifelse(Min==0 & Max==1,NA,SD))
  }
  
  data_descrip <- data_descrip %>%
    mutate_if(is.numeric,~round(.,digits=digits))
  
  return(data_descrip)
}

#### Calculate Cohen's D from two independent or paired groups ####
## SD can be pooled by providing sd1 and sd2
Get_CohensD <- function(m1,m2,sd1,sd2=NULL,n1,n2,sample="ind",proportion=FALSE){
  
  
  # raw mean difference
  md <- (m1 - m2)
  
  # Sigma for continuous variables
  if(proportion==FALSE){
    
    # Use only SD from group 1 or an overall SD
    if(is.null(sd2)){
      
      sigma <- sd1
      
    } else {
      
      # sigma for independent groups
      if(sample=="ind"){
        
        sigmanum <- (n1 - 1) * (sd1^2) + (n2 - 1) * (sd2^2)
        sigmadenom <- (n1 + n2 - 2)
        sigma <- sqrt(sigmanum/sigmadenom)
        
      } else{ 
        
        # sigma for paired groups
        sigma <- sqrt((sd1^2 + sd2^2) / 2)
        
      }
    }
    # Sigma for dichotomous variables NOTE: NEED TO REVIEW THESE FORMULAS
  } else {
    
    # Can provide overall proportion to sd2
    if(!is.null(sd2)){
      
      sigma <- sqrt(sd2 * (1 - sd2)/ n1)
      
    } else if(sample=="ind"){
      
      # for unequal independent groups 
      sigma <- sqrt((m1 * (1 - m1) / n1) + (m2 * (1 - m2) / n2))
      
    } else {
      
      # for paired groups or groups of equal size
      sigma <- sqrt((m1 * (1 - m1) + m2 * (1 - m2)) / 2)
      
    }
  }
  
  # Calculating d
  d <- md/sigma
  
  return(d)
}
#### Variance for Cohen's D from above calculation ####
Get_DVar <- function(d,n1,n2){
  left <- (n1+n2)/(n1*n2)
  right <- (d^2)/(2*((n1+n2)))
  dvar <- left + right
  return(dvar)
}

#### Converts correlations, eta2, or log odds ratio to cohen's d ####
## For more eta calculations see http://www-01.ibm.com/support/docview.wss?uid=swg21476421
## Formulas from: https://www.meta-analysis.com/downloads/Meta-analysis%20Converting%20among%20effect%20sizes.pdf
ConvertToD <- function(x,type=c("r","eta2","logOR","t"),levels,n1,n2){
  if(type=="r"){
    d <- (2*x)/sqrt((1-x^2))
  } else if(type=="eta2"){
    d <- sqrt(x^2/(1-x^2))*sqrt(2*levels)
  } else if(type=="logOR"){
    d <- x*(sqrt(3)/pi)
  } else if(type=="t"){
    d <- (2*x)/sqrt(n1+n2-2)
  }
}



#### Computing of correlation matrix ####
## adapted from http://www.sthda.com/english/wiki/correlation-matrix-an-r-function-to-do-all-you-need
rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        digits=2,
                        usena="complete.obs",
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL,
                        alpha = .05, ...)
{
  library(corrplot)
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
      mutate(CorSig = paste0(cor,ifelse(as.numeric(as.character(p)) < alpha, "*", " ")))
    return(cormat)
  }
  
}


#### CFA/SEM Fit Statistics and Reliability ####
## Getting Reliabilities
ScaleRelis <- function(model, ModName, Sname, TotalOnly = FALSE, digits=3){
  
  modrel <- suppressWarnings(semTools::reliability(model))
  
  reli <- modrel %>%
    data.frame() %>% tibble::rownames_to_column(var="Coefficient") %>%
    mutate_if(is.numeric,round,digits=digits) %>%
    mutate(Model = ModName,
           Software = Sname)
  
  if(TotalOnly == TRUE){
    
    reli <- reli %>%
      select(Model, Software, Coefficient, Total = total) %>%
      spread(Coefficient, Total)
    
  } else {
    
    reli <- reli %>%
      select(Model, Software, everything())
    
  }

  return(reli)
}

## Fit and reliability
Get_FitRels <- function(cfamod, fittype = "scaled", digs = 2, Mn = "Model", Sn = "lavaan", relis = TRUE){
  
  if(fittype=="scaled"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.scaled', 'rmsea.scaled', 'rmsea.ci.lower.scaled','rmsea.ci.upper.scaled','srmr')
  } else if(fittype=="robust"){
    indices <- c('chisq.scaled','df.scaled','pvalue.scaled','cfi.robust', 'rmsea.robust', 'rmsea.ci.lower.robust','rmsea.ci.upper.robust', 'srmr')
  } else if(fittype=="naive"){
    indices <- c('chisq','df','pvalue','cfi', 'rmsea', 'rmsea.ci.lower','rmsea.ci.upper', 'srmr')
  }
  
    fit <- as.data.frame(round(t(c(fitMeasures(cfamod,indices))),digs))
    names(fit) <- c("chisq","df","pvalue","CFI","rmsea","lower","upper","SRMR")
    fit <- fit %>% mutate(n = inspect(cfamod,"ntotal"),
                          X2 = paste0(chisq," (",round(df),")"),
                          RMSEA = paste0(rmsea," [",lower,", ",upper,"]")) %>%
      select(n,X2,pvalue,CFI,RMSEA,SRMR)
    
    if(!is.na(cfamod@loglik$loglik)){
      
      fit <- fit %>%
        mutate(AIC = round(AIC(cfamod),1),
               BIC = round(BIC(cfamod),1))
      
    }
    
    if(relis == TRUE){
      
      AllFits <- fit %>%
        bind_cols(ScaleRelis(cfamod, Mn, Sn, TotalOnly = TRUE, digits = digs))%>%
        select(Model, Software, everything())
      
    } else {
  
    AllFits <- fit %>%
      mutate(Model = Mn,
             Software = Sn) %>%
      select(Model, Software, everything())
    }

  
  return(AllFits)
}


#### Indices for evaluating bifactor model from lavaan object ####
## from Rodriguez, Reise, & Havelin (2016) with corrections
Bifactor_Indices <- function(model, genName, groupName = NULL, omegaOnly = FALSE){
  
  ## Extracting matrices
  params<- lavInspect(model, "est")                 # list of model matrices
  phi <- lavInspect(model, "cor.lv")                # factor correlation matrix; in a bifactor model this will always be an identity matrix
  sigma <- lavInspect(model, "cor.ov")              # model implied item correlation matrix, equivalent to params$lambda %*% phi %*% t(params$lambda) + params$theta

  
  ## Interim calculations
  lambdas <- split(params$lambda, col(params$lambda, as.factor = TRUE))                 # standardized factor loadings for each latent variable
  ks <- colSums(params$lambda != 0)                                                     # number of items for each factor; same as purrr::map_dbl(lambdas,~sum(.x != 0))    
  common <- sum(colSums(params$lambda)^2)                                               # common variance from all factors; same as sum(purrr::map_dbl(lambdas,~sum(.x)^2)) 
  error <- sum(params$theta)                                                            # sum of item error variances
  AllFD <- diag(phi %*% t(params$lambda) %*% solve(sigma) %*% params$lambda %*% phi)^.5 # Factor determinancy for all factors
  
  
  
  if(omegaOnly == TRUE){
    Indices <- common / (common + error)
    return(Indices)
  }
  
  if(is.null(groupName)){
    
  Indices <- data.frame(
    omega = common / (common + error),
    omegaH = sum(lambdas[[genName]])^2 / (common + error),
    ECV = sum(lambdas[[genName]]^2) / sum(colSums(params$lambda^2)),
    PUC = ((ks[[genName]]*(ks[[genName]]-1) / 2) - sum(map_dbl(ks[names(ks)!=genName],~.x*(.x-1)/2))) / (ks[[genName]]*(ks[[genName]]-1) / 2),
    FD = AllFD[[genName]],
    H = 1 / (1 + (1 / sum(lambdas[[genName]]^2 / (1 - lambdas[[genName]]^2))))
  )
  
  } else { 
    
    ## Only includes items belonging to the group, not error variance and general factor loadings from all items
    lambdf <- as.data.frame(params$lambda)             # coercing to dataframe for easier indexing
    groupdf <- lambdf[lambdf[groupName]!=0,]           # Extracting items belonging to group factor
    group <- sum(colSums(groupdf)^2)                   # Squared sum of factor loadings on group and general for items in group 
    gperror <- sum(params$theta[rownames(groupdf),])   # Sum of item error variances
    
    Indices <- data.frame(
      omega = group / (group + gperror),                          # Referred to as OmegaS in some paeprs, but Omega in others
      omegaHS = sum(lambdas[[groupName]])^2 / (group + gperror),
      FD = AllFD[[groupName]],
      H = 1 / (1 + (1 / sum(lambdas[[groupName]]^2 / (1 - lambdas[[groupName]]^2))))
    )
    
  }
  
  return(Indices)
}

#### Calculate Coefficient Alpha for a set of items ####
## Keeps only raw alpha
Get_Alpha <- function(data,scalename="Scale",digits=2){
  
  
  alphadf <- data %>% psych::alpha() %>%
    summary() %>% data.frame() %>% mutate(Scale=scalename) %>%
    select(Scale,raw_alpha) %>% mutate(raw_alpha = round(raw_alpha,digits=digits))
}

#### Shortcut for extracting parameters from lavaan object ####
ExtractParams <- function(object,parameter,name,include.groups=FALSE,standardized=FALSE,digits=2){
  
  name <- quo_name(enquo(name))
  
  if(standardized==FALSE){
    
    TheParams <- parameterestimates(object) %>% filter(op==parameter) %>%
      rename(!!name:=est)
    
  } else{
    
    TheParams <- standardizedSolution(object,type="std.lv") %>% filter(op==parameter) %>%
      rename(!!name:=est.std)
  }
  
  if(include.groups==TRUE){
    TheParams <- TheParams %>%
      select(Outcome=lhs,Predictor=rhs,group,!!name) %>% 
      mutate_if(is.numeric,~round(.,digits)) %>% unique()
    
  } else {
    
    TheParams <- TheParams %>%
      select(Outcome=lhs,Predictor=rhs,!!name) %>% 
      mutate_if(is.numeric,~round(.,digits)) %>% unique()
  }
  return(TheParams)
}


#### Compare Fit of two nested models ####
Delta_Fit <- function(mod1,mod2,fittype="scaled",modnames=NULL,digits=2){
  #fittype accepts "naive","scaled","robust"
  
  #if model names are not provided, this creates them from the model objects
  if(is.null(modnames)){
    modnames <- c(quo_name(enquo(mod1)),quo_name(enquo(mod2)))
  }
  
  # Defines whether the naive, scaled, or robust test and fit statistics are extracted
  if(fittype=="scaled"){
    indices <- c('cfi.scaled', 'rmsea.scaled', 'srmr')
  } else if(fittype=="robust"){
    indices <- c('cfi.robust', 'rmsea.robust', 'srmr')
  } else if(fittype=="naive"){
    indices <- c('cfi', 'rmsea', 'srmr')
  }
  
  lavtable <- lavaan::anova(mod1,mod2,method="satorra.bentler.2001")
  
  CompTable <- data.frame(Model1 = modnames[[1]],
                          Model2 = modnames[[2]],
                          # Delta_adjX2 = adjX2,
                          Delta_X2 = paste0(round(lavtable$`Chisq diff`[[2]],1)," (",round(lavtable$`Df diff`[[2]]),")"),         #X2,
                          # Delta_df = round(lavtable$`Df diff`[[2]]),               #d0 - d1,
                          Delta_pvalue = round(lavtable$`Pr(>Chisq)`[[2]],digits), #round(pchisq(X2,(d0 - d1),lower.tail = FALSE),5),
                          Delta_CFI = round(fitmeasures(mod1,indices[[1]]) - fitmeasures(mod2,indices[[1]]),digits),
                          Delta_RMSEA = round(fitmeasures(mod1,indices[[2]]) - fitmeasures(mod2,indices[[2]]),digits),
                          Delta_SRMR = round(fitmeasures(mod1,indices[[3]]) - fitmeasures(mod2,indices[[3]]),digits))
  
  if(!is.na(mod1@loglik$loglik)){
    CompTable <- CompTable %>%
      mutate(Delta_AIC = round(AIC(mod1) - AIC(mod2),1),
             Delta_BIC = round(BIC(mod1) - BIC(mod2),1))
  }
  
  rownames(CompTable) <- NULL
  
  return(CompTable)
}
