saveAggregatedUnbalancedData = function(aggregatedData){
    if(is.null(key(aggregatedData)))
        setkeyv(x = aggregatedData,
                cols = c("geographicAreaM49", "measuredItemHS", "timePointYears"))

    valueColumns = grep(valuePrefix, colnames(aggregatedData), value = TRUE)
    flagColumns = grep(flagPrefix, colnames(aggregatedData), value = TRUE)
    pairColumns = vector("character", length = length(valueColumns) * 2)
    pairColumns[as.logical(1:length(pairColumns) %% 2)] = valueColumns
    pairColumns[!as.logical(1:length(pairColumns) %% 2)] = flagColumns
    setcolorder(aggregatedData, c(key(aggregatedData), pairColumns))
    aggregatedData[, timePointYears := as.character(timePointYears)]
    if(NROW(aggregatedData) > 0)
        SaveData(domain = "trade", dataset = "total_trade",
                 data = aggregatedData, normalized = FALSE)
}
