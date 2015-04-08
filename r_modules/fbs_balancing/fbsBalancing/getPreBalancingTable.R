getPreBalancingTable = function(){
    preBalanceKey = DatasetKey(
        domain = "suafbs",
        dataset = "fbs_prebalance",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "measuredElement",
                      keys = c("250", "251", "252", "55252", "51202",
                          "51502", "51422", "55202", "50712")),
            Dimension(name = "measuredItemSuaFbs",
                      keys = standardizedItem$code),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    preBalancePivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
    )

    ## Query the data
    preBalanceQuery = GetData(
        key = preBalanceKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = preBalancePivot
    )


    ## Convert time to numeric
    preBalanceQuery[, timePointYears := as.numeric(timePointYears)]
    preBalanceQuery
}
