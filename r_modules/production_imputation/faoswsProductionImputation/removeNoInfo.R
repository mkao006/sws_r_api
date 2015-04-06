##' This function removes data with no information
##'
##' The information contained within a set of index is calculated
##' based on whether a non-zero value exist. If the data contains only
##' zero and missing value then it is considered to contain no
##' imformation for imputation.
##'
##' @param data The data
##' @param value The value of the production.
##' @param flag The flag/symbol of production.
##' @param byKey The unique keys which identify groups.
##'
##' @export
##' 

removeNoInfo = function (data, value, flag, byKey){
    info = data[, rep(containInfo(get(value), get(flag)),
        NROW(.SD)), by = byKey]$V1
    data = data[info, ]
}
