##' Function to extract data on loss.
##'
##' @return A data.table containing data on losses.



getLossData = function(){
    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    lossKey = DatasetKey(
        domain = "lossWaste",
        dataset = "loss",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementSuaFbs",
                      keys = "5120"),
            Dimension(name = "measuredItemSuaFbs",
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    lossPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementSuaFbs", ascending = TRUE)
    )

    ## Query the data
    lossQuery = GetData(
        key = lossKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = lossPivot
    )

    ## Convert name to the standard measured element
    setnames(lossQuery, old = "measuredItemSuaFbs", new = "measuredItemCPC")
    setnames(lossQuery,
             old = grep("measuredElementSuaFbs", colnames(lossQuery), value = TRUE),
             new = gsub("measuredElementSuaFbs", "measuredElement",
                 grep("measuredElementSuaFbs", colnames(lossQuery), value = TRUE)))
    

    ## Convert time to numeric
    lossQuery[, timePointYears := as.numeric(timePointYears)]
    lossQuery
}
