##' Function to extract the standard deviation of trade.
##'
##' The standard deviation is calculated as part of the trade
##' balancing process.
##'
##' @return A data.table containing the standard deviation of import
##' and export quantities.

getTradeStandardDeviation = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    tradeSDKey = DatasetKey(
        domain = "trade",
        dataset = "stddev_quantity",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementTrade",
                      keys = c("SD5600", "SD5900")),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradeSDPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeSDQuery = GetData(
        key = tradeSDKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradeSDPivot
    )

    ## NOTE (Michael): Maybe the standard deviation also need to be
    ## divided by 1000 to convert in tonnes.
    

    ## Convert time to numeric
    tradeSDQuery[, timePointYears := as.numeric(timePointYears)]
    tradeSDQuery
}
