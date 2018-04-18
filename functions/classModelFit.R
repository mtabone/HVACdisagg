## ClassModelFitFuns.r
## Contains a function to fit Mark Dyson's hourly classification model 

## Updated to also fit change-point models by restricting the probability of
## Updated to fit models with out any heating or cooling, and to fit models with 
## both heatinga and cooling.

classModelFit <- function(Data , ChangePoints = matrix(c(50,70),ncol = 2), numIters = 100, convCrit = .005, 
                          dependentColName = 'powerAve', CPColName = 'outTemp', 
                          otherRegressors = 'HOD:dayType', emisShape = 'normal',
                          coolingIntercept = FALSE, heatingIntercept = FALSE, 
                          coolingState = FALSE, heatingState = FALSE,
                          storeall = TRUE, verbose= F, class_model = T,
                          heatSqTmp = FALSE, coolSqTmp = FALSE){
  ## classModelFit inputs a data.frame of hourly energy use (household or AC) and hourly outdoor temperature and 
  # a set of change points (below which the AC is never "ON") and fits a classification which distinguishes between times when 
  # the AC is in Cooling Mode (Clg) ON or OFF. 
  #
  ## Inputs  : 
  #     Data             :  A data frame containing (at least) the column 
  #                         dependentColName,    default to "powerAve"   for average power during the hour, and 
  #                         CPColName,           default to "outTemp"    for average outdoor temperature. 
  #                         otherRegressors      default contains "HOD," and "dayType"  
  #     dataUnit**       : ** GETTING RID OF THIS FROM NOW ONLY ALL DATA IN THE SAME TIMESCALE OR AVERAGED
  #                         Data corresponding tot he units of analysis, which can be at a longer time resolution than those in data
  #                         otherRegressors  should be stored here if unitLength is not 1.
  #     unitLength**     : ** no longer available  The length of the unit of analysis, the number of rows of dataUnit must be the number of rows of Data divided by unitLength
  #
  #     dependentColName : Column name of the dependent variable (in dataUnit or in Data)
  #     CPColName        : Column name of the independent vairable which is affected by the changepoint
  #
  #     coolingIntercept : whether to provide a separate intercept when switching between ON/OFF states of Cooling
  #     heatingIntercept : whether to provide a separate intercept when switching between ON/OFF states of Heating
  #
  #     coolSqTmp        : logical, whether to include squared temperature in cooling model
  #     heatSqTmp        : logical, whether to include squared temperature in cooling model
  #
  #     otherRegressors  : other regressors to include for non-HVAC 
  #
  #     ChangePoints     :  An dataframe of changepoints to try, must be a two column array if fitting heating and cooling. 
  #                      :  columns must be labeled cool and heat. 
  #
  #     numIters         :  The maximum number of iterations allowed (100)
  #     convCrit         :  Convergence criterion,
  #                         the maximum percentage of classifications that may be changed 
  #                         during the iteration preceeding convergence
  #     emisShape        : distribtuion shape for errors given the model 'normal' or 'kernel'
  #     storeall         : Returns all models (even those that are not maximum likelihood), setting 
  #                            to FALSE saves space. 
  #
  #
  #
  ## Output : a list structure containing elements named
  #    'Models'       : linear models produced by "lm" (one for each changepoint, stored in a list)
  #    'ChangePoints' : the changepoints (though these were directly input)
  #    'TotalLLs'     : the total log likelihood (one for each changepoint, stored in an array)  
  #    'CoolingMode'  : the cooling modes assignment for each hour (one for each changepoint, stored in a list)
  #    'NumIt'        : Number of iterations required for each fit. 
  #
  
  ## Initialize Outputs
  output                   <- list()
  output$ToverCP           <- list()
  output$TunderCP          <- list()
  output$prob_heat         <- list()
  output$prob_cool         <- list()
  output$totalLL           <- array()
  output$AIC               <- array()
  output$BIC               <- array()
  output$k                 <- array()
  output$Fcool             <- array()
  output$Fheat             <- array()
  output$PredictedValues   <- list()
  output$inputs            <- list()
  
  ## Also store the inputs in the output 
  output$inputs$otherRegressors   <- otherRegressors   # Other regressors in the regression (Z)
  output$inputs$dependentColName  <- dependentColName  # Dependent column name (Y) 
  output$inputs$CPColName         <- CPColName         # Name of the changepoint colum (T_a) 
  output$inputs$emisShape         <- emisShape         # Shape of emissions ('normal', or 'kernel')
  output$inputs$heatingState      <- heatingState      # Is a heating state included?
  output$inputs$coolingState      <- coolingState      # Is a cooling state included?
  output$inputs$coolingIntercept  <- coolingIntercept  # Is a cooling intercept used?
  output$inputs$heatingIntercept  <- heatingIntercept  # Is a heating intercept used?
  output$inputs$coolSqTmp         <- coolSqTmp         # Is sq temp included in the cooling model?
  output$inputs$heatSqTmp         <- heatSqTmp         # Is sq temp included in the heating model?
  output$inputs$class_model       <- class_model       # Is this a classification model or a change-point model?
  

  
  # Cycle through each row of change points run the "EM" algorithm within each *****************************************
  # clgBreakPoints defined outside of function
  for(cpi in 1:dim(ChangePoints)[1]) {
    ## print out the changepoint
    if (verbose){
      print(paste('Fitting Changepoint(s) ', ChangePoints[cpi,]))
    }
    
    ## Get changepoints
    if(coolingState) CPcool <- ChangePoints$cool[cpi]
    if(heatingState) CPheat <- ChangePoints$heat[cpi]
    
    ## THE "OFF" State is included in all models, begin with it
    dataUse       <- Data                      # Initialize data.frame of all variables in regressions
    N             <- dim(Data)[1]              # Get number of observations
    fmlaString    <- paste( dependentColName, '~ 0 + ', otherRegressors ) # Initialize formula
    gamma_off     <- matrix( rep(1, N) , ncol = 1)   # Initialize gamma matrix for expected value of states
    
    ## Initialize heating information 
    if ( heatingState ){
      dataUse$TunderCP  <- CPheat - Data[ , CPColName ]   
      interceptStr      <- rep('+ HtMode', heatingIntercept*1)
      dataUse$TunderCPSq<- dataUse$TunderCP^2  
      squareStr         <- rep('+ HtMode:TunderCPSq', coolSqTmp*1)
      fmlaString        <- paste( fmlaString, interceptStr, ' + HtMode:TunderCP', squareStr )
      UnderCP           <- dataUse$TunderCP > 0
      gamma_heat        <- UnderCP & ( dataUse[, dependentColName] > quantile(dataUse[, dependentColName], probs = .05, na.rm = T))
      gamma_off         <- gamma_off - gamma_heat
      Fheat             <- sum(gamma_heat, na.rm = T) / sum(UnderCP, na.rm = T)
      dataUse$HtMode    <- rep(0,N)  # Initialize heating in the ``dataUse'' frame as as 0 for now. 
    }
    
    ## Initialize cooling information
    if ( coolingState ){
      dataUse$ToverCP   <- Data[ , CPColName ] - CPcool
      interceptStr      <- rep('+ ClgMode', coolingIntercept*1)
      dataUse$ToverCPSq <- dataUse$ToverCP^2
      squareStr         <- rep('+ ClgMode:ToverCPSq', coolSqTmp*1)
      fmlaString        <- paste( fmlaString, interceptStr, ' + ClgMode:ToverCP', squareStr)
      OverCP          <- dataUse$ToverCP > 0
      gamma_cool      <- OverCP & ( dataUse[, dependentColName] > quantile(dataUse[, dependentColName], probs = .1, na.rm = T))
      gamma_off       <- gamma_off - gamma_cool
      Fcool           <- sum(gamma_cool, na.rm = T) / sum(OverCP, na.rm = T)
      dataUse$ClgMode <- rep(0,N)  # Initialize heating in the ``dataUse'' frame as as 0 for now. 
    }
    
    # Define datasets given each state
    # (1) all-cooling times and (2) all-noncooling times
    # use them to predict resids given each state
    
    ## Create data given neither heating or cooling
    dataUse.none           <- dataUse
    dataUse.none$Prob      <- gamma_off*1
    dataUse.none$weights    <- gamma_off*1
    
    if (coolingState){
      # Create Cooling On Data
      dataUse.allClg           <- dataUse
      dataUse.allClg$ClgMode   <- rep(1,N)
      dataUse.allClg$Prob      <- gamma_cool*1
      dataUse.allClg$weights   <- gamma_cool*1
    }
    
    if (heatingState){
      # Create heating On Data
      dataUse.allHt           <- dataUse
      dataUse.allHt$HtMode    <- rep(1,N)
      dataUse.allHt$Prob      <- gamma_heat*1
      dataUse.allHt$weights   <- gamma_heat*1  # for now, assume all variances equal
    }
    
    # Flag for whether "EM" has converged
    converged     = FALSE
    badRegression = FALSE
    
    ## Function to find kernel densities of emissions 
    kernelLik <- function(emiss, probs) {
      induse   <- !is.na(emiss) & !is.na(probs)
      weights  <- probs[induse] / sum(probs[induse])
      theKernel <- density(emiss[induse], bw = bw.nrd(emiss[induse]), 
                           weights = weights, na.rm = T)
      return(theKernel)
    }
    
    ## Initialize total log likelihood vector. 
    TotalLogLik <- rep(NA, numIters)
    
    ## BEGIN FOR LOOP FOR EM ***********************************************************************************************
    for(i in 1:numIters) {
      
      ### CHECKS TO SEE IF REGRESSION HAS GONE "BAD" or "GOOD," EM is like purgatory I supposen ***********************************************
      # ALSO FORCE AT LEAST 4 ITERATIONS. THIS IS PRIMAILY FOR THE CHANGE-POINT ONLY MODELS, 
      # I WOULD LIKE TO THE ITERATE A COUPLE OF TIMES FOR THE SIGMAS TO CONVERGE.
      if(converged & i > 4) {break}
      
      # **************************  UPDATE BETA AND THETA *******************************
      # *************************  weighted regression    *******************************
      regModel=tryCatch({
        
        # Regression is essentially weighted regression, with one "observation" for each possible state, each obs' weight is the probability of being in that state. 
        
        # Compile data frame for regression
        regData <- dataUse.none
        if (coolingState) regData <- rbind(regData, dataUse.allClg)  
        if (heatingState) regData <- rbind(regData, dataUse.allHt)  
        
        regModel = lm(as.formula(fmlaString), data= regData, weights = weights, na.action = na.omit)
        
      }, error =function(e){
        
        # Print to screen if regression is "bombed"
        print('regression bombed, skipping')
        regModel=NULL
      })
      
      ## Another check for bad regressions, (1) if regression was bombed, (2) if there is no standard error. 
      if(is.null(regModel)){
        badRegression=T
        break
      }
      
      ###  ****  UPDATE f_m(e), distribution of disturbances given state ************************************
      ###  **** In the process find the likelihood of each observation and each state. **********************
      # Predict each point each state
      predW_none         = predict ( regModel , dataUse.none )
      if (coolingState) predW_Clg         = predict ( regModel , dataUse.allClg )
      if (heatingState) predW_Ht          = predict ( regModel , dataUse.allHt )
      
      # Get emissions given each state
      offEmiss   <- dataUse[,dependentColName] - predW_none
      if (coolingState) coolEmiss  <- dataUse[,dependentColName] - predW_Clg
      if (heatingState) heatEmiss  <- dataUse[,dependentColName] - predW_Ht
      
      # find standard deviations given each state
      sigma_off  <- sqrt( sum( (offEmiss   * gamma_off )^2 , na.rm = T) / sum( gamma_off , na.rm = T) )
      if (coolingState)  sigma_cool <- sqrt( sum( (coolEmiss  * gamma_cool)^2 , na.rm = T) / sum( gamma_cool, na.rm = T) )
      if (heatingState)  sigma_heat <- sqrt( sum( (heatEmiss  * gamma_heat)^2 , na.rm = T) / sum( gamma_heat, na.rm = T) )
      
      ## Find the probability of each observation given each model 
      # Alllow for an option where emissions are assumed to be (1) normal or (2) based on a kernel
      if (emisShape == 'normal') {
        # NORMAL EMISSION SHAPE
        
        # Initialize probability of heating and cooling being off as 1
        p_off = rep(1,N)
        
        if (coolingState) {
          ## Find likelihod of cooling and emission , p(e, cool)
          lik_cool <- dnorm  ( coolEmiss  , mean = 0, sd = sigma_cool) * OverCP  * Fcool
          ## update probability that both heating and cooling are off. 
          p_off    <- p_off - OverCP  * Fcool
        }
        
        if (heatingState) {
          ## Find likelihod of heating and emission , p(e, heat)
          lik_heat <- dnorm  ( heatEmiss  , mean = 0, sd = sigma_heat) * UnderCP * Fheat
          ## update probability that both heating and cooling are off. 
          p_off    <- p_off - UnderCP * Fheat
        }
        
        # Find probability of emission and model is off
        lik_off <-  dnorm  ( offEmiss   , mean = 0, sd = sigma_off)  * p_off
        
      } else if (emisShape == 'kernel') {
        # Kernel Emission Shape
        ## Find the likelihood of emissions given the "off" model
        offKDE     <- kernelLik(offEmiss, gamma_off)
        likGiv_off <- approx( x = offKDE$x, y = offKDE$y, xout = offEmiss  )$y
        #Find initial probability give off
        p_off      <- 1
        
        if(coolingState) {
          # Find likelihood of emissions given that cooling is on: p(e | cool)
          coolKDE     <- kernelLik(coolEmiss, gamma_cool)
          likGiv_cool <- approx( x = coolKDE$x, y = coolKDE$y, xout = coolEmiss  )$y
          
          # find likelihood of emissions and cooling is on            : p(e , cool)
          lik_cool    <- likGiv_cool * OverCP  * Fcool
          
          # Update prior probability that cooling and heating are off. 
          p_off       <- p_off -  OverCP  * Fcool
        }
        
        if(heatingState) {
          # Find likelihood of emissions given that heating is on: p(e | heat)
          heatKDE     <- kernelLik(heatEmiss, gamma_heat)
          likGiv_heat <- approx( x = heatKDE$x, y = heatKDE$y, xout = heatEmiss  )$y
          
          # Find likelihood of emissions given and heating : p(e , heat)
          lik_heat    <- likGiv_heat * UnderCP * Fheat
          
          # Update prior probability that cooling and heating are off.
          p_off       <- p_off -  UnderCP  * Fheat
        }
        
        # Find probability of emissions and that cooling and heating are off.
        lik_off    <- likGiv_off * p_off 
        
      }
      
      ## ****** Update Gamma *****************************************
      ## ****** ****  Find total likelihood of each observation, and total log likelihod of all observtions ****
      lik_total           <-  lik_off
      if(coolingState) lik_total <- lik_total + lik_cool
      if(heatingState) lik_total <- lik_total + lik_heat
      TotalLogLik[i]         <- sum( log( lik_total ) , na.rm = T)
      
      
      ## Update state probabilities (gamma)
      # Store old gamma_off dvector
      gamma_off_0 <- gamma_off
      if (class_model){
        ## Only do this if we are actually solviing a classification problem, there is a switch in here
        
        ## FIND GAMMA
        gamma_off <- lik_off / lik_total
        if(coolingState) gamma_cool <- lik_cool / lik_total
        if(heatingState) gamma_heat <- lik_heat / lik_total
        
      } else {
        ## If a change-point only model, these are defined by the changepoint alone
        gamma_off            <- 1
        if(coolingState){
          gamma_cool         <- OverCP
          gamma_off          <- gamma_off - OverCP
        }
        if(heatingState){
          gamma_heat         <- UnderCP
          gamma_off          <- gamma_off - UnderCP
        }
      }
      
      ## Exit loop if a state crashes (not sure why this happens)
      gammacrash <- F
      if(!any(gamma_off>0, na.rm = T)) gammacrash <- T
      if(coolingState) if(!any(gamma_off>0, na.rm = T)) gammacrash <- T
      if(heatingState) if(!any(gamma_off>0, na.rm = T)) gammacrash <- T
      
      if (gammacrash){
        print('A State Crashed');
        badRegression = TRUE; 
        break
      }
      
      ## ********   Update prior probabilities of heating or cooling states given the correct side of CP **************
      if(coolingState) Fcool      <- sum(gamma_cool, na.rm = T) / sum(OverCP, na.rm = T)
      if(heatingState) Fheat      <- sum(gamma_heat, na.rm = T) / sum(UnderCP, na.rm = T)
      
      
      # Find weights for regressions as probability of being in the state divided by the variance
      # **** Think this through more thoroughly while writing   
      dataUse.none$weights     <- gamma_off  / sigma_off^2
      if(coolingState) dataUse.allClg$weights   <- gamma_cool / sigma_cool^2
      if(heatingState) dataUse.allHt$weights    <- gamma_heat / sigma_heat^2
      
      # Convergance is based on the percentage of assignments that change
      # We measure this solely with the "off" state since heating and cooling don't compete. 
      # Though actually this isn't so bad a measure
      pctDiff <- sum( abs( gamma_off_0 - gamma_off ), na.rm = T ) / sum(!is.na(gamma_off))
      if(pctDiff < convCrit | i==numIters) {
        converged=T
        ##print(c(i,pctDiffAssignments))
      }
      
      
    } # END of "EM" iteration loop *****************************************************************************************
    
    
    ## Extra validation for comparison between changepoints ****************************************************************
    if(badRegression) {
      next 
    }
    
    ## Find a couple of extra things
    if (cpi == 1 ){
      ## If this is the first cp tried... store the coefficients. 
      output$coefficients      <- matrix(ncol = dim(ChangePoints)[1], nrow = length(regModel$coefficients))
    }
    
    ## Disaggregate cooling and other using exected variances 
    # Find variance of cooling
    ## Find matrixes of covariates and coefficients
    options(na.action = 'na.pass')
    dataUse$HtMode <- rep(1,N)
    dataUse$ClgMode <- rep(1,N)
    X         <- model.matrix( as.formula(fmlaString) , data = dataUse, na.action = 'na.pass'  )
    options(na.action = 'na.omit')
    b         <- as.matrix(regModel$coefficients)
    
    ## Isolate covariates that are associated with cooling
    coolcol  <-  grepl('ClgMode', colnames(X))
    heatcol  <-  grepl('HtMode', colnames(X))

    bcool    <-  as.matrix(b[coolcol,])
    bheat    <-  as.matrix(b[heatcol,])
    boff     <-  as.matrix(b[!(coolcol | heatcol),])
    Xcool    <-  as.matrix(X[,coolcol])
    Xheat    <-  as.matrix(X[,heatcol])
    Xoff     <-  as.matrix(X[,!(coolcol | heatcol)])
    
    if(!coolingState)  gamma_cool <- rep(0, length(gamma_off))
    if(!heatingState)  gamma_heat <- rep(0, length(gamma_off))
    
    ## Estimate each value, disaggregating the residuals
    Cool      <- rep(0, N)
    Heat      <- rep(0, N)
    if(coolingState) Cool      <- gamma_cool* (  Xcool %*% bcool + coolEmiss * (sigma_cool - sigma_off) / sigma_cool )  
    if(heatingState) Heat      <- gamma_heat* (  Xheat %*% bheat + heatEmiss * (sigma_heat - sigma_off) / sigma_heat )
    Other     <- Data[, dependentColName] - Cool - Heat
    CoolNeed  <- Xcool %*% bcool
    HeatNeed  <- Xheat %*% bheat
    OtherBar  <- Xoff %*% boff
    PowerPred <- OtherBar + gamma_cool * CoolNeed + gamma_heat * HeatNeed

    #Create data.frame of predicted values
    PredVal <- data.frame(ProbOff = gamma_off, ProbCool = rep(0,N), ProbHeat = rep(0,N), Heat = Heat, Cool = Cool, Other = Other, CoolNeed = CoolNeed, HeatNeed = HeatNeed)
    if(coolingState) PredVal$ProbCool <- gamma_cool
    if(heatingState) PredVal$ProbHeat <- gamma_heat
    PredVal$HeatNeed  <- HeatNeed
    PredVal$CoolNeed  <- CoolNeed
    PredVal$OtherBar  <- OtherBar
    PredVal$PowerPred <- PowerPred
    
    ## STORAGE OF CONVERGED REGRESSION  ************************************************************************************
    # Save this result in case it turns out to be the best CP (remember, we run EM on a number of change points)
    # refModels are the outputs of LM, dataUses include the cooling mode assignments.  
    if(coolingState) {
      output$ToverCP[[cpi]]              <- dataUse$ToverCP
      output$prob_cool[[cpi]]            <- gamma_cool
      output$Fcool[cpi]                  <- Fcool
      output$sigma_cool[cpi]             <- sigma_cool
    } else {
      output$prob_cool[[cpi]]            <- rep(0, N)
    }

    if(heatingState) {
      output$TunderCP[[cpi]]              <- dataUse$ToverCP
      output$prob_heat[[cpi]]             <- gamma_heat
      output$Fheat[cpi]                   <- Fheat
      output$sigma_heat[cpi]              <- sigma_heat
    } else {
      output$prob_heat[[cpi]]            <- rep(0, N)
    }
    
    N = dim(Data)[1] 
    k = length(b) + 1 + 2*heatingState + 2*coolingState
    
    output$totalLL[cpi]           <- TotalLogLik[i-1]
    output$AIC[cpi]               <- -2 * TotalLogLik[i-1] + k * log(N)
    output$BIC[cpi]               <- -2 * TotalLogLik[i-1] + k * 2
    output$k[cpi]                 <- k
    output$totalLL_trace[[cpi]]   <- TotalLogLik[!is.na(TotalLogLik)]
    output$coefficients[,cpi]     <- regModel$coefficients
    output$PredictedValues[[cpi]] <- PredVal
    output$sigma_off[cpi]              <- sigma_off
    rownames(output$PredictedValues[[cpi]]) <- c()
    # Store the number of iterations
    output$NumIt[cpi]     <- i
    
    # Store a standard deviation or a kernel 
    if (emisShape == 'kernel'){
      output$kernels[[cpi]]         <- list(offKDE = offKDE) 
      if(coolingState) output$kernels[[cpi]]$coolKDE <- coolKDE
      if(heatingState) output$kernels[[cpi]]$heatKDE <- heatKDE
    }
    
    
  } #end of changepoint loop
  
  ## Store the changepoints 
  output$CPs   <- ChangePoints
  
  
  # Name the output coefficients dataframe
  rownames(output$coefficients) <- names(regModel$coefficients)
  
  return(output)
  
}


validate_fit <-function( fitout, data, coolcol = 'Ave_Power_Cool' ){
  ## OK THIS OUTPUTS A LOT OF STATISTICS
  # 'Mean_cool_hour'     is the actual average hourly cooling measured by sub-metering
  # 'Mean_cool_day'      is the actual average daily cooling measured at the submeter
  # 'Cool_hour_rmse'     is the RMSE of disagregated cooling from actual cooling
  # 'Cool_hour_me'       is the ME of disaggregated cooling from the actual cooling
  # 'Cool_bar_hour_rmse' 'Cool_bar' defines statistics from mean cooling without disagregation of residuals. 
  # 'Cool_bar_hour_me'
  # 'Cool_day_rmse'      rmse of daily cooling predictions, accounting for disaggregation... many more follow fromhere
  
  ## Suffixes
  # '_9'                 Statistic using only ours where estimated probability of cooling is above 0.90
  # '_act'               Statistic accounting only for hours when actual cooling is above 2% of the hourly maximum. 
  
  ## Precision and recall
  # 'Cool_hour_recall'        Recall (total number of true positives identified) Using probabilities of cooling identified by model
  # 'Cool_hour_precision'     Precision (total number of identiftied positives that are true) Using probabilities of cooling identified by model
  # 'Cool_hour_recall_9'      Recall (total number of true positives identified) Using a p=0.9 cutoff for classification
  # 'Cool_hour_precision_9'   Precision (total number of identiftied positives that are true) Using a p=0.9 cutoff for classification
  
  ## Initialize an output df
  df <- as.data.frame(matrix(nrow = 1, ncol= 0))
  
  ## Collect actual versus no coling 
  tll <- fitout$totalLL
  tll[tll==Inf]<- NA
  m <- which.max( tll )                  ## Best Model in Set
  Prob_est      <- fitout$prob_cool[[m]]               ## Estiamted Probability of cooling
  Prob_est_9    <- fitout$prob_cool[[m]]>.9            ## Indicator if estiamted probability of cooling is above .9
  Cool_act      <- data[,coolcol]               ## Actual Cooling Use
  Cool_est      <- fitout$PredictedValues[[m]]$Cool ## Estimated Cooling Use (including disag step)
  Cool_bar_est  <- (fitout$PredictedValues[[m]]$CoolNeed) * Prob_est ## Estimated Cooling Use no disagg (mean only)
  
  ## Make the actual probabiity when cooling power is less than 5% of maximum
  Prob_act     <- Cool_act > (.02* max(Cool_act, na.rm = T)) ## Actual probability of cooling
  Cool_act_mean   <- mean(Cool_act, na.rm = T)
  Cool_act_bar_9 <- mean(Cool_act[ Prob_est_9 ], na.rm = T)
  
  ## Find daily values
  t <- as.Date(data$DateTimeCST)
  norm <- 1
  
  # replace infinite likelihood with
  
  # Actual cooling energy used daily (use mean to fill in NA values)
  Cool_act_day <- apply( as.matrix(unique(t)), MARGIN = 1, FUN = function(x) mean( Cool_act[t == x], na.rm = T ) * 24 /norm)
  # Estimated amount of cooling energy daily
  Cool_est_day <- apply( as.matrix(unique(t)), MARGIN = 1, FUN = function(x) mean( Cool_est[t == x], na.rm = T ) *24 /norm)
  # Estimated amount of cooling energy daily (mean only)
  Cool_bar_est_day <- apply( as.matrix(unique(t)), MARGIN = 1, FUN = function(x) mean( Cool_bar_est[t == x], na.rm = T ) *24 /norm)
  
  # Expected number of cooling hours in a day
  Prob_est_coolhour      <- apply( as.matrix(unique(t)), MARGIN = 1, FUN = function(x) mean( Prob_est[t == x], na.rm = T ) *24 /norm)
  # Number of hours with over 90% prob of cooling in a day
  Prob_est_coolhour_9   <- apply( as.matrix(unique(t)), MARGIN = 1, FUN = function(x) mean( Prob_est_9[t == x], na.rm = T ) *24 /norm)
  # Actual number of cooling hours in a day
  Prob_act_coolhour     <- apply( as.matrix(unique(t)), MARGIN = 1, FUN = function(x) mean( Prob_act[t == x], na.rm = T ) *24 /norm)
  
  ## Statistics
  ## Actual mean cooling, for baseline
  df$Mean_cool_hour       <- mean( Cool_act, na.rm = T )
  df$Mean_cool_hour_9    <- mean(Cool_act[Prob_est_9], na.rm = T)  
  df$Mean_cool_hour_act    <- mean(Cool_act[Prob_act], na.rm = T)  
  
  df$Mean_cool_day       <- mean( Cool_act, na.rm = T ) * 24 /norm
  df$Mean_cool_day_act    <- mean(Cool_act_day[Prob_act_coolhour > 5  ], na.rm = T)  
  
  ## Hourly RMSE of Cooling
  df$Cool_hour_rmse         <- sqrt( mean( ( Cool_est - Cool_act )^2, na.rm = T ) )
  df$Cool_hour_rmse_9       <- sqrt( mean( ( Cool_est[Prob_est_9] - Cool_act[Prob_est_9] )^2, na.rm = T ) )
  df$Cool_hour_rmse_act     <- sqrt( mean( ( Cool_est[Prob_act] - Cool_act[Prob_act] )^2, na.rm = T ) )
  
  ## Hourly ME of Cooling
  df$Cool_hour_me             <-  mean( ( Cool_est - Cool_act ), na.rm = T ) 
  df$Cool_hour_me_9           <-  mean( ( Cool_est[Prob_est_9] - Cool_act[Prob_est_9] ), na.rm = T ) 
  df$Cool_hour_me_act         <-  mean( ( Cool_est[Prob_act] - Cool_act[Prob_act] ), na.rm = T ) 
  
  ## Hourly ME and RMSE of Cooling (without disaggregation of residuals)
  df$Cool_bar_hour_rmse         <- sqrt( mean( ( Cool_bar_est - Cool_act )^2, na.rm = T ) )
  df$Cool_bar_hour_rmse_9       <- sqrt( mean( ( Cool_bar_est[Prob_est_9]  - Cool_act[Prob_est_9]  )^2, na.rm = T ) )
  df$Cool_bar_hour_rmse_act     <- sqrt( mean( ( Cool_bar_est[Prob_act]  - Cool_act[Prob_act]  )^2, na.rm = T ) )
  
  df$Cool_bar_hour_me         <- ( mean( ( Cool_bar_est - Cool_act ), na.rm = T ) )
  df$Cool_bar_hour_me_9       <- ( mean( ( Cool_bar_est[Prob_est_9]  - Cool_act[Prob_est_9]  ), na.rm = T ) )
  df$Cool_bar_hour_me_act     <- ( mean( ( Cool_bar_est[Prob_act]  - Cool_act[Prob_act]  ), na.rm = T ) )
  
  ## Hourly Precision and recall metrics
  df$Cool_hour_recall       <- sum( Prob_est * Prob_act , na.rm = T) / sum(Prob_act, na.rm = T)
  df$Cool_hour_precision    <- sum( Prob_est * Prob_act , na.rm = T) / sum(Prob_est, na.rm = T)
  df$Cool_hour_recall_9     <- sum( Prob_est_9 * Prob_act , na.rm = T) / sum(Prob_act, na.rm = T)
  df$Cool_hour_precision_9  <- sum( Prob_est_9 * Prob_act , na.rm = T) / sum(Prob_est_9, na.rm = T)
  
  ## Daily RMSE and ME of Cooling 
  df$Cool_day_rmse       <- sqrt( mean( ( Cool_est_day - Cool_act_day )^2, na.rm = T ) )
  df$Cool_day_rmse_act   <- sqrt( mean( ( Cool_est_day[Prob_act_coolhour>5] - Cool_act_day[Prob_act_coolhour>5] )^2, na.rm = T ) )
  df$Cool_day_me         <- ( mean( ( Cool_est_day - Cool_act_day ), na.rm = T ) )
  df$Cool_day_me_act     <- ( mean( ( Cool_est_day[Prob_act_coolhour>5] - Cool_act_day[Prob_act_coolhour>5] ), na.rm = T ) )
  
  df$Cool_bar_day_rmse       <- sqrt( mean( ( Cool_bar_est_day - Cool_act_day )^2, na.rm = T ) )
  df$Cool_bar_day_rmse_act   <- sqrt( mean( ( Cool_bar_est_day[Prob_act_coolhour>5] - Cool_act_day[Prob_act_coolhour>5] )^2, na.rm = T ) )
  df$Cool_bar_day_me         <- ( mean( ( Cool_bar_est_day - Cool_act_day ), na.rm = T ) )
  df$Cool_bar_day_me_act     <- ( mean( ( Cool_bar_est_day[Prob_act_coolhour>5] - Cool_act_day[Prob_act_coolhour>5] ), na.rm = T ) )
  
  return(df)
  
  
}

class_model_store <- function(class_model_fit, times, idrow){
  ## Function to store a classification model to the database of fits to consert data. 
  # Relies on a nicely specified table
  
  
  ## Initialize a dataframe with basic data
  df <- as.data.frame(idrow[,1:4])
  
  ## Get model inputs
  df$coolingState  <- class_model_fit$inputs$coolingState
  df$heatingState  <- class_model_fit$inputs$heatingState
  df$heatingIntercept <- class_model_fit$inputs$heatingIntercept * df$heatingState
  df$coolingIntercept <- class_model_fit$inputs$coolingIntercept * df$coolingState
  df$heatSqTmp <- class_model_fit$inputs$heatSqTmp * df$heatingState
  df$coolSqTmp <- class_model_fit$inputs$coolSqTmp * df$coolingState
  
  df$otherRegressors  <- class_model_fit$inputs$otherRegressors
  df$emisShape        <- class_model_fit$inputs$emisShape
  df$dependentColName <- class_model_fit$inputs$dependentColName
  df$CPColName        <- class_model_fit$inputs$CPColName
  df$class_model      <- class_model_fit$inputs$class_model
  
  ## find if data are hourly or daily
  d <- diff(as.numeric(times[1:2]))
  if (d > 5000) datatime <- 'daily'
  if (d < 5000) datatime <- 'hourly'
  
  df$dailyData <- d > 5000
  
  ## make a model name  
  cm_str <- if (df$class_model) { 'class' } else {'cp'}
  
  df$modelname <- paste( cm_str, datatime, df$emisShape, rep('cool', df$coolingState*1), rep('cint',df$coolingIntercept*1) ,rep('cTsq',df$coolSqTmp*1) ,
                         rep('heat', df$heatingState*1), rep('hint', df$heatingIntercept*1),rep('hTsq',df$heatSqTmp*1) , sep = '_') 
  
  df$shortname <- paste( if(df$class_model){"C"}else{"P"},
                         if(df$dailyData){"D"}else{"H"}, 
                         if(df$emisShape=='normal'){"N"}else{"K"}, 
                         if(df$coolingState){paste('_C', rep('1',df$coolingIntercept*1), rep('S',df$coolSqTmp*1), sep = '')} else {''} ,
                         if(df$heatingState){paste('_H', rep('1',df$heatingIntercept*1), rep('S',df$heatSqTmp*1), sep = '')} else {''} , sep = '') 
  
  
  
  ## Get correct CP
  tll <-class_model_fit$totalLL
  tll[tll==Inf] <- NA
  k <- which.max( tll )
  
  ## 
  df$totalLL <- class_model_fit$totalLL[k]
  df$AIC     <- class_model_fit$AIC[k]
  df$BIC     <- class_model_fit$BIC[k]
  
  
  if(df$coolingState) df$CPcool  <- class_model_fit$CPs$cool[k]
  if(df$heatingState) df$CPheat  <- class_model_fit$CPs$heat[k]
  
  
  ## Align coefficients
  coeff <- as.data.frame( t( class_model_fit$coefficients[,k]  ) )
  
  ## Sigmas data frame
  df$sigma_off <- class_model_fit$sigma_off[k]
  if (df$coolingState) df$sigma_cool <- class_model_fit$sigma_cool[k]
  if (df$heatingState) df$sigma_heat <- class_model_fit$sigma_heat[k]

  ## validation data.frame 
  validdf <- class_model_fit$validation
  
  # Create fit description
  
  df <- cbind(df, coeff)
  if ( !is.null(validdf) ) df <- cbind(df, validdf)
  
  colnames(df) <- ( gsub(':','_',colnames(df)) )
  
  ## Create predicted values data frame
  attr(times, 'tzone') <- 'UTC'
  valuesdf <- data.frame( VID = df$VID, modelname = df$modelname, dateTime = times)
  valuesdf <- cbind( valuesdf, class_model_fit$PredictedValues[[k]])
  valuesdf$ProbCool <- class_model_fit$prob_cool[[k]]*1
  valuesdf$ProbHeat <- class_model_fit$prob_heat[[k]]*1
  
  
  ## Place in database
  con <- dbcon_bgrid()
  
  examp <- dbGetQuery(con, 'SELECT column_name FROM information_schema.columns WHERE table_schema = \'michaelangelo\' AND table_name   = \'consert_machine_fits_ms\'')
  nm    <- examp$column_name
  nmneed <- nm[!is.element(nm,colnames(df))]
  df[,nmneed] <-NA
  df <- df[,nm]
  
  dbGetQuery(con, 'set time zone \"UTC\"')
  
  tryCatch(dbWriteTable(con, name = c('michaelangelo','consert_machine_fits_ms'), value = df, append = T, row.names = F),
           error = function(e) print(e))
  
  tryCatch(dbWriteTable(con, name = c('michaelangelo','consert_machine_fits_ms_tsout'), value = valuesdf, append = T, row.names = F),
           error = function(e) print(e))    
  
  dbDisconnect(con)
  

}

