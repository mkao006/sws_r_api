aggregateTradeFlow = function(mirroredData, key, aggregateFlag = "c"){
    setnames(mirroredData, reportingCountryVar, standardCountryVar)
    if(missing(key))
        key = c(standardCountryVar, itemVar, yearVar)

    valueColumns = grep(valuePrefix, colnames(mirroredData), value = TRUE)
    flagColumns = grep(flagPrefix, colnames(mirroredData), value = TRUE)
    aggregatedData =
        mirroredData[, lapply(valueColumns,
                              FUN = function(x) sum(as.numeric(.SD[[x]]))),
                     by = key]
    setnames(aggregatedData,
             old = paste0("V", 1:length(valueColumns)),
             new = valueColumns)
    aggregatedData[, `:=`(c(flagColumns), aggregateFlag)]
    setkeyv(aggregatedData, key)
    aggregatedData
}
