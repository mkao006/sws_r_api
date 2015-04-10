addBalancingFlags = function(data, valueColumns, valuePrefix,
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
