getTradeStandardDeviationCaput = function(valueOnly = TRUE){
    tradeStdCaputKey = DatasetKey(
        domain = "trade",
        dataset = "stddev_caloriescap",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "measuredItemSuaFbs",
                      keys = standardizedItem$code),
            Dimension(name = "measuredElementTrade",
                      keys = c("SD5600", "SD5900")),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradeStdCaputPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),        
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeStdCaputQuery = GetData(
        key = tradeStdCaputKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradeStdCaputPivot
    )

    setkeyv(tradeStdCaputQuery, cols = c("geographicAreaM49", "timePointYears"))
    ## Convert time to numeric
    tradeStdCaputQuery[, timePointYears := as.numeric(timePointYears)]
    if(valueOnly)
        tradeStdCaputQuery =
            tradeStdCaputQuery[, c("geographicAreaM49", "measuredItemSuaFbs",
                                   "timePointYears",
                                   grep("Value", colnames(tradeStdCaputQuery),
                                        value = TRUE)),
                               with = FALSE]
    tradeStdCaputQuery    
}
