##' The function extracts data on trade quantities.
##'
##' The Quantities are converted from Kilograms to Tonnes.
##'
##' @return A data.table containing the data on imports and exports in tonnes

getTradeData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    tradeKey = DatasetKey(
        domain = "trade",
        dataset = "total_trade_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementTrade",
                      keys = c("5600", "5900")),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeQuery = GetData(
        key = tradeKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradePivot
    )
    
    ## NOTE (Michael): The unit for trade is in kg while for other
    ##                 elements are tonnes, so we divide the trade by
    ##                 1000 to match the unit.
    if("Value_measuredElementTrade_5600" %in% colnames(tradeQuery))
        tradeQuery[, Value_measuredElementTrade_5600 :=
                       computeRatio(Value_measuredElementTrade_5600, 1000)]
    
    if("Value_measuredElementTrade_5900" %in% colnames(tradeQuery))
    tradeQuery[, Value_measuredElementTrade_5900 :=
                   computeRatio(Value_measuredElementTrade_5900, 1000)]

    ## Convert time to numeric
    tradeQuery[, timePointYears := as.numeric(timePointYears)]
    tradeQuery
}
