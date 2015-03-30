##' Remove Carry Forward
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

removeCarryForward = function(data, variable){
    data[, variance := var(.SD[[variable]], na.rm = TRUE),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data[, duplicateValue := duplicated(.SD[[variable]]),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data = data[!(variance == 0 & duplicateValue), ]
    data[, `:=`(c("variance", "duplicateValue"), NULL)]
    data         
}
