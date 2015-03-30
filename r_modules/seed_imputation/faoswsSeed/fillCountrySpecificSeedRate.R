##' Fill country specific seed rates
##' 
##' @param data The seed data.table, typically as produced by a call to
##' getAreaData.
##' @param countryVariable The column name of data that specifies the country
##' code variable.  This is needed to join with the table from the database.
##' @param commodityVariable The column name of data that specifies the
##' commodity code variable.  This is needed to join with the table from the
##' database.
##' @param countrySpecificData A data.table with data describing seed rates for
##' each country.
##' 
##' @return No value is returned.  Instead, seed rates (from
##' countrySpecificData) are appended onto data.
##' 
##' @export
##' 

fillCountrySpecificSeedRate = function(data,
    countryVariable = "geographicAreaM49",
    commodityVariable = "measuredItemCPC",
    countrySpecificData = getCountrySpecificSeedRate()){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(countrySpecificData, "data.table"))
    
    ## Create the new columns
    data[, Value_seedRate := NA]
    data[, flagObservationStatus_seedRate := NA]
    
    ## Fill in the country Specific rates
    okey = key(data)
    setkeyv(data, cols = c(countryVariable, commodityVariable))
    data[countrySpecificData,
         `:=`(c("Value_seedRate", "flagObservationStatus_seedRate"),
              list(i.Value_seedRate, i.flagObservationStatus_seedRate)),
         allow.cartesian = TRUE]
    setkeyv(data, okey)
}