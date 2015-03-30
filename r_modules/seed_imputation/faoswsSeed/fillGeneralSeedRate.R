##' Fill general seed rate
##' 
##' This function updates missing values of the Value_seedRate column in data.
##' If the value is missing, it is replaced with the commodity value.
##' 
##' @param data The data.table object containing the seed data, typically as
##' produced via getAreaData.
##' @param generalSeedData A data.table containing seeding data specific to
##' each commodity, typically as produced by getCountryGeneralSeedRate.
##' 
##' @return No object is returned.  Instead, the underlying data object is
##' modified.
##' 
##' @export
##' 

fillGeneralSeedRate = function(data,
                               generalSeedData = getCountryGeneralSeedRate()){

    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(generalSeedData, "data.table"))
    
    ## fill in the general rates
    okey = key(data)
    setkeyv(data, key(generalSeedData))
    data[generalSeedData, `:=`(c("generalSeedRate", "generalSeedFlag"),
            list(i.Value_seedRate, i.flagObservationStatus_seedRate)),
        allow.cartesian = TRUE]
    ## If no data exists for general case, add columns of NA
    if(!"generalSeedRate" %in% colnames(data))
        data[, c("generalSeedRate", "generalSeedFlag") := list(NA, NA)]
    data[is.na(Value_seedRate),
         `:=`(c("Value_seedRate", "flagObservationStatus_seedRate"),
              list(generalSeedRate, generalSeedFlag))]
    data[, `:=`(c("generalSeedRate", "generalSeedFlag"), list(NULL, NULL))]
    setkeyv(data, okey)
}
