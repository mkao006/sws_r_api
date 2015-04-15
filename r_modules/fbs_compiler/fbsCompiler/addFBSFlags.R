##' Function to append flags and order the columns for saving.
##'
##' @param data The contingecy table to be saved back.
##' @param valueColumns The numeric columns which needs to have flags.
##' @param valuePrefix The prefixe name of the value columns.
##' @param flagObsStatusPrefix The prefix name of observation status flag.
##' @param flagMethodPrefix The prefix name of method flag.
##'
##' @return A data.table with all the flags added to the corresponding
##' value columns in the right order for saving.

addFBSFlags = function(data, valueColumns, valuePrefix,
    flagObsStatusPrefix, flagMethodPrefix){

    dataCopy = copy(data)
    flagObsColumns = gsub(valuePrefix, flagObsStatusPrefix, valueColumns)
    flagMethodColumns = gsub(valuePrefix, flagMethodPrefix, valueColumns)
    
    dataCopy[, `:=`(c(flagObsColumns), "E")]
    dataCopy[, `:=`(c(flagMethodColumns), "e")]
    
    
    keyColumns = colnames(dataCopy)[!colnames(dataCopy) %in%
        c(valueColumns, flagObsColumns, flagMethodColumns)]

    valueTupleOrder = c(t(cbind(valueColumns, flagObsColumns, flagMethodColumns)))
    setcolorder(dataCopy, neworder = c(keyColumns, valueTupleOrder))
    dataCopy
}
