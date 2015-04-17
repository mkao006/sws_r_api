tableToInput = function(table, meanVariables, sdVariables, feedDemandVariables){
    dataMatrix =
        data.matrix(table[, c(meanVariables),
                          with = FALSE])
    rownames(dataMatrix) = table$measuredItemSuaFbs

    ## Rounding is required to avoid numerical error which results in
    ## negative values.
    rowTotalMatrix = round(rowSums(dataMatrix, na.rm = TRUE), 5)

    sdMatrix = 
        data.matrix(table[, c(sdVariables), with = FALSE])
    rownames(sdMatrix) = table$measuredItemSuaFbs

    feedDemandDf =
        unique(data.frame(table[, c(feedDemandVariables), with = FALSE]))
    list(data = dataMatrix, row_Tot = rowTotalMatrix, sd = sdMatrix,
         feed = feedDemandDf)
}
