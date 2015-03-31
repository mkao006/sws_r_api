##' Ensure Production Inputs
##'
##' This function is designed to ensure that the provided dataset is valid.  In
##' particular, it coerces column types: all values are coerced to numeric (
##' instead of integer, which can cause problems) and all flags are coerced to
##' character (instead of logical, which occurs if the flag is set to NA).
##' Also, it ensures data is a data.table.
##' 
##' @param data A data.table containing the data.
##' @param processingParameters A list containing the parameters to be used in
##' the processing algorithms.  See ?defaultProcessingParameters for a starting
##' point.
##'
##' @export
##' 

ensureProductionInputs = function(data, processingParameters){
    
    ### Basic checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(processingParameters, "list"))
    
    ### Check for parameters
    p = processingParameters
    stopifnot(is.character(c(p$productionValue, p$productionObservationFlag,
                             p$productionMethodFlag, p$yieldValue,
                             p$yieldObservationFlag, p$yieldMethodFlag,
                             p$areaHarvestedValue,
                             p$areaHarvestedObservationFlag,
                             p$areaHarvestedMethodFlag, p$yearValue, p$byKey)))
    stopifnot(is.logical(c(p$removePriorImputation, p$removeConflictValues)))
    stopifnot(is.character(c(p$imputedFlag, p$naFlag,
                             p$newMethodFlag)))
    
    ### Make sure all column name variables exist in data
    columnNames = c(processingParameters$productionValue,
                    processingParameters$productionObservationFlag,
                    processingParameters$productionMethodFlag,
                    processingParameters$yieldValue,
                    processingParameters$yieldObservationFlag,
                    processingParameters$yieldMethodFlag,
                    processingParameters$areaHarvestedValue,
                    processingParameters$areaHarvestedObservationFlag,
                    processingParameters$areaHarvestedMethodFlag,
                    processingParameters$yearValue,
                    processingParameters$byKey)
    missingColumns = ! columnNames %in% colnames(data)
    if( any(missingColumns) )
        stop("The following columns do not exist in data but should (or the",
             "parameters in the global environment should be corrected):\n\t",
             paste(columnNames[missingColumns], collapse="\n\t"))
    
    ### Coerce columns to appropriate type:
    for(name in c(processingParameters$productionValue,
                  processingParameters$areaHarvestedValue,
                  processingParameters$yieldValue)){ 
        warning("This coercion is no longer necessary upon resolution of SWS-797")
        data[, c(name) := as.numeric(get(name))]
    }
    for(name in c(processingParameters$productionObservationFlag,
                  processingParameters$areaHarvestedObservationFlag,
                  processingParameters$yieldObservationFlag,
                  processingParameters$productionMethodFlag,
                  processingParameters$areaHarvestedMethodFlag,
                  processingParameters$yieldMethodFlag)){
        warning("This coercion is no longer necessary upon resolution of SWS-797")
        data[, c(name) := as.character(get(name))]
    }
    
    ### Globally assign ensuredData so data will not need to be ensured again
    ensuredProductionData <<- TRUE
}
