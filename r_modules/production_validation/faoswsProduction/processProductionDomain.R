##' This is a wrapper for all the data manipulation step before the
##' preparation of the imputation.
##'
##' @param data The data
##' @param processingParameters A list of the parameters for the production
##' processing algorithms.  See defaultProductionParameters() for a starting
##' point.
##' 
##' @export
##' 

processProductionDomain = function(data, processingParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredProductionData") || !ensuredProductionData)
        ensureProductionInputs(data = data,
                               processingParameters = processingParameters)
    
    ### processingParameters will be referenced alot, so rename to p
    p = processingParameters
    
    ### Remove prior imputations
    if(processingParameters$removePriorImputation){
        faoswsUtil::removeImputation(data = data,
                    value = p$areaHarvestedValue,
                    observationFlag = p$areaHarvestedObservationFlag,
                    methodFlag = p$areaHarvestedMethodFlag,
                    missingObservationFlag = p$naFlag,
                    imputedFlag = p$imputedFlag)
        faoswsUtil::removeImputation(data = data,
                    value = p$yieldValue,
                    observationFlag = p$yieldObservationFlag,
                    methodFlag = p$yieldMethodFlag,
                    missingObservationFlag = p$naFlag,
                    imputedFlag = p$imputedFlag)
        faoswsUtil::removeImputation(data = data,
                    value = p$productionValue,
                    observationFlag = p$productionObservationFlag,
                    methodFlag = p$productionMethodFlag,
                    missingObservationFlag = p$naFlag,
                    imputedFlag = p$imputedFlag)
    }

    ### Assign NA's when the flag is missing
    faoswsUtil::remove0M(data = data,
             value = p$areaHarvestedValue,
             flag = p$areaHarvestedObservationFlag,
             naFlag = p$naFlag)
    faoswsUtil::remove0M(data = data,
             value = p$yieldValue,
             flag = p$yieldObservationFlag,
             naFlag = p$naFlag)
    faoswsUtil::remove0M(data = data,
             value = p$productionValue,
             flag = p$productionObservationFlag,
             naFlag = p$naFlag)
    
    ### Remove conflicting/illogical zeros
    if(p$removeConflictValues){
        faoswsUtil::removeZeroConflict(data = data,
                           value1 = p$areaHarvestedValue,
                           value2 = p$productionValue,
                           observationFlag1 = p$areaHarvestedObservationValue,
                           observationFlag2 = p$productionObservationValue,
                           methodFlag1 = p$areaHarvestedMethodValue,
                           methodFlag2 = p$productionMethodValue,
                           missingObservationFlag = p$naFlag)
    }

    ### Remove byKey groups that have no data
#     faoswsUtil::removeNoInfo(data = data,
#                  value = p$yieldValue,
#                  observationFlag = p$yieldObservationFlag,
#                  byKey = p$byKey)
    # removeNoInfo assigns the new data.table to the variable "data" in the
    # environment of this function.  Thus, to ensure "data" is returned to the
    # caller of this function, assign the data.table to the calling environment.
    # This should be removed/fixed once row deletion by reference is
    # implemented for data.table, see
    # http://stackoverflow.com/questions/10790204/how-to-delete-a-row-by-reference-in-r-data-table
    dataTableName = as.character(match.call()$data)
    assign(x = dataTableName, value = data, envir = parent.frame(1))
}
