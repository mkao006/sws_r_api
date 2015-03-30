##' Impute Seed Domain
##' 
##' This function is a wrapper function which calls all the individual
##' functions of this package.  First, values where the area sown is 0 and the
##' area harvested is non-zero (or vice-versa) are removed.  Next, area sown is
##' imputed, and then country specific and general seed rates are appended to
##' the data.table.  Lastly, the seed is imputed.
##' 
##' @param data The data.table object containing the seed data.
##' @param imputationParameters See
##' ?faoswsImputation::defaultImputationParameters.  This is a list of
##' arguments specifying how imputation should be done, and it pertains to
##' imputation of the area sown variable.  If imputation should be done by a
##' simple mean, set imputationParameters to NULL.
##' @param byKey In the case where imputationParameters is NULL, imputation is
##' done by computing a mean within each group defined by byKey and using
##' that mean for each missing value.  byKey = NULL, the default, uses a global
##' mean for each commodity.  If imputationParameters is not NULL, this is
##' ignored.
##' 
##' @return No value is returned, but the data.table object is modifed: seed is
##' imputed and additional columns are returned.
##' 
##' @export
##' 

imputeSeedDomain = function(data, imputationParameters, byKey = NULL){
    
    ## Add columns for later imputations (needed for checks)
    data[, Value_areaSownRatio := NA]
    data[, flagObservationStatus_areaSownRatio := "M"]
    data[, flagMethod_areaSownRatio := "u"]
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    if(!is.null(imputationParameters))
        faoswsImputation::ensureImputationInputs(data = data,
                imputationParameters = imputationParameters)
    if(!is.null(byKey))
        stopifnot(byKey %in% colnames(data))
    requiredColumns = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears")
    for(code in c("5212", "5312", "5525"))
        requiredColumns = c(requiredColumns, paste0(c("Value_measuredElement_",
                                   "flagObservationStatus_measuredElement_",
                                   "flagMethod_measuredElement_"), code))
    missingColumns = requiredColumns[!requiredColumns %in% colnames(data)]
    if(length(missingColumns) > 0)
        stop("Data is missing required columns:\n",
             paste0(missingColumns, collapse = "\n"))
    
    ## Run the imputation
    for(item in data[, unique(measuredItemCPC)]){
        subset = data[measuredItemCPC == item, ]
        removeZeroConflict(data = subset, value1 = "Value_measuredElement_5212", 
                value2 = "Value_measuredElement_5312",
                observationFlag1 = "flagObservationStatus_measuredElement_5212",
                observationFlag2 = "flagObservationStatus_measuredElement_5312",
                methodFlag1 = "flagMethod_measuredElement_5212",
                methodFlag2 = "flagMethod_measuredElement_5312")
        imputeAreaSown(data = subset,
                       imputationParameters = imputationParameters,
                       byKey = byKey)
        fillCountrySpecificSeedRate(data = subset)
        fillGeneralSeedRate(data = subset)
        imputeSeed(subset)
        ## Reassign processed values back into data.  We do this by filtering
        ## data.table to the desired rows and then grabbing all columns and
        ## assigning to them the subsetted data.
        data[measuredItemCPC == item, (colnames(data)) := subset]
    }
}