##' The function converts missing values to zero.
##'
##' @param data The data.table object.
##' @param valueColumns The column names which corresponds to columns
##' which are of type numeric and the missing values can be assumed to
##' be zero.
##'
##' @return A data.table with all the specified columns converted from
##' missing value to zero.
##' 

missingValueToZero = function(data, valueColumns){
    if(missing(valueColumns))
        valueColumns = grep("Value", colnames(data), value = TRUE)
    dataCopy = copy(data)    
    dataCopy[, `:=`(c(valueColumns),
                    lapply(valueColumns,
                           FUN = function(x){
                               tmp = .SD[[x]]
                               tmp[is.na(tmp)] = 0
                               tmp
                           }))]
    dataCopy
}
