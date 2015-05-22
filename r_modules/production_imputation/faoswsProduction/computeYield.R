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
##' @param yieldMethodFlag The column name corresponding to the method flag for
##' yield.
##' @param newMethodFlag The character value that should be placed in the
##' yieldMethodFlag column when yield is computed.
##' @param flagTable see \code{\link[faoswsFlag]{faoswsFlagTable}}.
##' @param data The data.table object containing the data.
##' @param unitConversion yield is computed as (production)/(area harvested)*
##' (unit conversion).
##'
##' @export
##' 

computeYield = function(productionValue, productionObservationFlag,
    areaHarvestedValue, areaHarvestedObservationFlag, yieldValue,
    yieldObservationFlag, yieldMethodFlag, newMethodFlag,
    flagTable = faoswsFlag::faoswsFlagTable, data, unitConversion = 1){

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

    ## Don't update all values, only if the yield is "updateable"
    updateable = data[, is.na(yieldValue) & !is.na(productionValue) &
                          !is.na(areaHarvestedValue)]
    data[updateable, yieldValue :=
         computeRatio(productionValue, areaHarvestedValue) * unitConversion]
    data[updateable, yieldObservationFlag :=
         aggregateObservationFlag(productionObservationFlag,
                                  areaHarvestedObservationFlag,
                                  flagTable = flagTable)]
    data[updateable, yieldMethodFlag := newMethodFlag]

    setnames(x = data,
             old = c("productionValue", "productionObservationFlag",
                 "areaHarvestedValue", "areaHarvestedObservationFlag",
                 "yieldValue", "yieldObservationFlag", "yieldMethodFlag"),
             new = c(productionValue, productionObservationFlag,
                 areaHarvestedValue, areaHarvestedObservationFlag,
                 yieldValue, yieldObservationFlag, yieldMethodFlag))
}
