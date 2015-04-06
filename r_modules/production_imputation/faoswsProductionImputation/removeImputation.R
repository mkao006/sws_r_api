##' Function to remove prior imputation.
##'
##' @param data The data.table object containing the values.
##' @param value The value of the observation.
##' @param flag The flag of the observation.
##' @param imputedFlag The value of the flag which denotes the value
##' was imputed.
##' @param naFlag The value of the flag which denotes missing value.
##'
##' @export

removeImputation = function(data, value, flag, imputedFlag = "T",
    naFlag = "M"){
    imputedIndex = which(data[[flag]] %in% imputedFlag)
    invisible(data[imputedIndex, `:=`(c(value, flag), list(NA, naFlag))])
}
