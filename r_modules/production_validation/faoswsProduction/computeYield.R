##' Function to compute and update yield
##'
##' @param data The data.table object containing the data.
##' @param newMethodFlag The flag to be used to update the yield method flag
##' when imputation occurs.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param unitConversion Yield is computed as (production) / (area) and
##' multiplied by unitConversion.  This parameter defaults to 1.
##' @param processingParameters A list of the parameters for the production
##' processing algorithms.  See defaultProductionParameters() for a starting
##' point.
##'
##' @export
##' 

computeYield = function(data, newMethodFlag, flagTable = faoswsFlagTable,
                        unitConversion = 1, processingParameters){

    if(!exists("ensuredProductionData") || !ensuredProductionData)
        ensureProductionInputs(data = data,
                               processingParameters = processingParameters)
    stopifnot(faoswsUtil::checkMethodFlag(newMethodFlag))
    stopifnot(faoswsUtil::checkObservationFlag(
        flagTable$flagObservationStatus))

    ## Abbreviate processingParameters since it is used alot
    pp = processingParameters
    
    ## Balance yield values only when they're missing
    missingYield = is.na(data[[pp$yieldValue]]) |
        data[[pp$yieldObservationFlag]] == "M"
    data[missingYield, c(pp$yieldValue) :=
         faoswsUtil::computeRatio(get(pp$productionValue),
                                  get(pp$areaHarvestedValue)) * unitConversion]
    data[missingYield, c(pp$yieldObservationFlag) :=
         aggregateObservationFlag(get(pp$productionObservationFlag),
                                  get(pp$areaHarvestedObservationFlag),
                                  flagTable = flagTable)]
    data[missingYield, c(pp$yieldMethodFlag) := newMethodFlag]
    ## If yieldValue is still NA, make sure observation flag is "M".  Note:
    ## this can happen by taking 0 production / 0 area.
    data[is.na(get(pp$yieldValue)), c(pp$yieldObservationFlag) := "M"]

}
