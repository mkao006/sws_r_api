##' Function to load the country specific rate data
##' 
##' This function grabs data from the specific_seed_rate table in the ess
##' schema in the database and applies a few simple operations to it (renames
##' columns, converters dimensions to characters, and sets all flags that
##' aren't "E" to "").  These rates are provided at the commodity-country level
##' as opposed to commodity level in getCountrySpecificSeedRate().
##' 
##' The seed rates that are pulled from the database are assumed to be official
##' values.  That is, the observation status flags of all of these observations
##' are set to "".
##' 
##' @return The data.table object with specific seed rate data.
##' 
##' @export
##' 

getCountrySpecificSeedRate = function(){
    countrySpecificRate.dt =
        GetTableData(schemaName = "ess", tableName = "specific_seed_rate")
    
    ## NOTE (Michael): We assume all other data collection method are official
    countrySpecificRate.dt[flag != "E", flag := ""]
    
    setnames(countrySpecificRate.dt,
             old = c("area", "item", "value", "flag"),
             new = c("geographicAreaM49", "measuredItemCPC", "Value_seedRate",
                     "flagObservationStatus_seedRate"))
    countrySpecificRate.dt[, measuredItemCPC := as.character(measuredItemCPC)]
    countrySpecificRate.dt[, geographicAreaM49 := as.character(geographicAreaM49)]
    setkeyv(countrySpecificRate.dt, c("geographicAreaM49", "measuredItemCPC"))
    countrySpecificRate.dt
}