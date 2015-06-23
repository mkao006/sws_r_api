##' Remove Carry Forward
##' 
##' This function removes values from a data.table if that variable has zero
##' variance.  One observation, however, will always be kept.  WHY???
##' 
##' @param data A data.table object containing the seed data which should have 
##'   "carry forward" observations removed.
##' @param variable The column name of data which should have it's "carry 
##'   forward" values deleted.
##'   
##' @return A data.table object, the same as what is passed in, but with "carry 
##'   forward" observations removed.
##'   

removeCarryForward = function(data, variable){
    data[, variance := var(get(variable), na.rm = TRUE),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data[, duplicateValue := duplicated(get(variable)),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data = data[!(variance == 0 & duplicateValue), ]
    data[, `:=`(c("variance", "duplicateValue"), NULL)]
    data         
}
