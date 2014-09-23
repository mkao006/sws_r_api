##' Function to compute and update yield
##'
##' @param productionValue The column name corresponding to production
##' value.
##' @param productionObservationFlag The column name corresponding to
##' the observation flag of production.
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


computeYield = function(productionValue, productionObservationFlag,
    areaHarvestedValue, areaHarvestedObservationFlag, yieldValue,
    yieldObservationFlag, yieldMethodFlag, newMethodFlag,
    flagTable = faoswsFlagTable, data, unitConversion = 1){

    if(!yieldValue %in% colnames(data))
        data[, c(yieldValue) := NA]
    if(!yieldObservationFlag %in% colnames(data))
        data[, c(yieldObservationFlag) := NA]
    if(!yieldMethodFlag %in% colnames(data))
        data[, c(yieldMethodFlag) := NA]

    setnames(x = data,
             old = c(productionValue, productionObservationFlag,
                     areaHarvestedValue, areaHarvestedObservationFlag,
                     yieldValue, yieldObservationFlag, yieldMethodFlag),
             new = c("productionValue", "productionObservationFlag",
                 "areaHarvestedValue", "areaHarvestedObservationFlag",
                 "yieldValue", "yieldObservationFlag", "yieldMethodFlag"))

    data[, yieldValue :=
         computeRatio(productionValue, areaHarvestedValue) * unitConversion]
    data[, yieldObservationFlag :=
         aggregateObservationFlag(productionObservationFlag,
                                  areaHarvestedObservationFlag,
                                  flagTable = flagTable)]
    data[, yieldMethodFlag := newMethodFlag]

    setnames(x = data,
             old = c("productionValue", "productionObservationFlag",
                 "areaHarvestedValue", "areaHarvestedObservationFlag",
                 "yieldValue", "yieldObservationFlag", "yieldMethodFlag"),
             new = c(productionValue, productionObservationFlag,
                 areaHarvestedValue, areaHarvestedObservationFlag,
                 yieldValue, yieldObservationFlag, yieldMethodFlag))
}
