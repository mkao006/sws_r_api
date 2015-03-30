##' Get Country General Seed Rates
##' 
##' This function grabs data from the default_seed_rate table in the ess
##' schema in the database and applies a few simple operations to it (renames
##' columns, sets keys).  These default seed rates are provided at the
##' commodity level, as opposed to the commodity-country level in
##' getCountrySpecificSeedRate().
##' 
##' The seed rates that are pulled from the database are assumed to be official
##' values.  That is, the observation status flags of all of these observations
##' are set to "".
##' 
##' @return The data.table object with specific seed rate data.
##' 
##' @export
##' 

getCountryGeneralSeedRate = function(){
    seedGeneral.dt =
        GetTableData(schemaName = "ess", tableName = "default_seed_rate")
    ## NOTE (Michael): We again assume the rates are official here
    seedGeneral.dt[, flagObservationStatus_seedRate := ""]
    setnames(seedGeneral.dt,
             old = c("item", "value"),
             new = c("measuredItemCPC", "Value_seedRate"))
    setkeyv(seedGeneral.dt, "measuredItemCPC")
    seedGeneral.dt
}
