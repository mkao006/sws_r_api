##' Function to compute area harvested when new production and yield
##' are given.
##'
##'
##' @param productionValue The column name corresponding to production
##' value.
##' @param productionObservationFlag The column name corresponding to the
##' observation flag of production.
##' @param areaHarvestedValue The column name corresponding to area
##' harvested value.
##' @param areaHarvestedObservationFlag The column name corresponding
##' to the observation flag of area harvested.
##' @param yieldValue The columne name corresponding to yield value.
##' @param yieldObservationFlag The column name corresponding to the
##' observation flag of yield.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##'
##' @export
##' 


balanceAreaHarvested = function(productionValue,
    productionObservationFlag, 
    areaHarvestedValue, areaHarvestedObservationFlag,
    areaHarvestedMethodFlag, yieldValue, yieldObservationFlag,
    newMethodFlag, flagTable = faoswsFlagTable, data){
    
    origName = c(productionValue, productionObservationFlag,
        areaHarvestedValue,
        areaHarvestedObservationFlag, areaHarvestedMethodFlag,
        yieldValue, yieldObservationFlag)
    tmpName = c("pValue", "pObsFlag", "aValue", "aObsFlag",
        "aMetFlag", "yValue", "yObsFlag")
    setnames(data, old = origName, new = tmpName)
    
    data[!is.na(pValue) & is.na(aValue) & !is.na(yValue),
         c("aValue", "aObsFlag", "aMetFlag") :=
         list(computeRatio(pValue, yValue),
              aggregateObservationFlag(pObsFlag, yObsFlag,
                                       flagTable = flagTable),
              newMethodFlag)
         ]
    setnames(data, old = tmpName, new = origName)
}
