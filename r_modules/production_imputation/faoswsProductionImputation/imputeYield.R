##' Function to impute yield
##'
##' This function imputes the yield through linear mixed model
##'
##' @param yieldValue The column name of the yield variable.
##' @param yieldObservationFlag The observation flag of yield.
##' @param yieldMethodFlag The method flag of yield.
##' @param yearValue The column name corresponding to year.
##' @param imputationFlag Flag value for new imputation values.
##' @param newMethodFlag The new method flag to be assigned to imputed
##' value.
##' @param maxdf The maximum degree of freedom for the spline.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data
##' @param weights The weights for the observation
##' @param byKey The unique key identifier.
##'
##' @export
##' 


imputeYield = function(yieldValue, yieldObservationFlag, yieldMethodFlag,
    yearValue, imputationFlag, newMethodFlag, maxdf = 5, 
    flagTable = faoswsFlagTable, data, weights = NULL, byKey,
    yieldFormula){

    setnames(x = data, old = c(yieldValue, yieldObservationFlag,
                           yieldMethodFlag, yearValue),
             new = c("yieldValue", "yieldObservationFlag",
                 "yieldMethodFlag", "yearValue"))
    
    yieldMissingIndex = is.na(data[, yieldValue])
    
    if(missing(yieldFormula)){
        yieldFormula =
            as.formula(paste0("yieldValue ~ -1 + (1 + yearValue|",
            byKey, ")"))
        ## print(yieldFormula)
        model = try(
            lmer(formula = yieldFormula, data = data,
                 ## weights = data[, productionValue],
                 weights = weights,
                 REML = FALSE)
            )
                    
        predictError = function(x, y, newdata){
            yhat = predict(x, newdata = newdata)
            amse = sum((yhat - y)^2,na.rm = TRUE)/
                length(na.omit(y))
            amse
        }

        benchmarkError = bootMer(model,
            FUN = function(x){
                predictError(x = x, y = data$yieldValue,
                             newdata = data)
            }, nsim = 100)
        
        if(!inherits(model, "try-error")){
            for(i in 2:maxdf){
                ## cat("proposing df:", i, "\n")
                newYieldFormula =
                    as.formula(paste0("yieldValue ~ -1 + (1 + bs(yearValue, df = ",
                                      i, ", degree = 1)|", byKey, ")"))
                ## print(newYieldFormula)
                newModel = try(
                    lmer(formula = newYieldFormula,
                         data = data,
                         ## weights = data[, productionValue],
                         weights = weights,
                         REML = FALSE)
                    )
                if(!inherits(newModel, "try-error")){

                    newModelError = bootMer(newModel,
                        FUN = function(x){
                            predictError(x = x, y = data$yieldValue,
                                         newdata = data)
                        }, nsim = 100)
                    ## cat("old:", mean(benchmarkError$t), "\n")
                    ## cat("new:", mean(newModelError$t), "\n")
                    if(mean(benchmarkError$t) > mean(newModelError$t)){
                        yieldFormula = newYieldFormula
                        model = newModel
                        benchmarkError = newModelError
                    } else {
                        cat("Model with", i - 1,
                            "degree of freedom is selected\n")
                        break
                    }                   
                    
                    ## m = bootMer(model, FUN = function(x)
                    ##     as.numeric(logLik(x)), nsim = 100)
                    ## nm = bootMer(newModel, FUN = function(x)
                    ##     as.numeric(logLik(x)), nsim = 100)
                    ## if(quantile(-2 * m$t + 2 * nm$t, prob = 0.05) < 0){
                    ##     break
                    ## } else {
                    ##     yieldFormula = newYieldFormula
                    ##     model = newModel
                    ## }
                }
            }
        }
    } else {
        model = try(
            lmer(formula = yieldFormula, data = data,
                 ## weights = data[, productionValue],
                 weights = weights,
                 REML = FALSE)
            )
    }
                
    if(!inherits(model, "try-error")){
        
        ## Impute the data with lme.
        data[yieldMissingIndex,
             yieldValue := predict(model, newdata = .SD,
                            allow.new.levels = TRUE)]
        
        ## Remove negative value from data.
        data[yieldValue <= 0, yieldValue := as.numeric(NA)]
        

    }
    ## Reimpute with naive imputation for those values that were
    ## negative, or if the model failed.
    data[, yieldValue := defaultNaive(yieldValue), by = byKey]
    data[yieldMissingIndex & !is.na(yieldValue),
         c("yieldObservationFlag", "yieldMethodFlag") :=
         list(imputationFlag, newMethodFlag)]

    setnames(x = data,
             old = c("yieldValue", "yieldObservationFlag",
                 "yieldMethodFlag", "yearValue"),
             new = c(yieldValue, yieldObservationFlag,
                 yieldMethodFlag, yearValue))
}
