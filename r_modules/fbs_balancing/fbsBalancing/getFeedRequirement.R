getFeedRequirementData = function(){
    feedRequirementKey = DatasetKey(
        domain = "feed",
        dataset = "total_feed",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "nutrientType",
                      keys = "1"),
            Dimension(name = "estimator",
                      keys = as.character(2:3)),
            Dimension(name = "feedBaseUnit",
                      keys = "2"),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )
    
    ## Pivot to vectorize yield computation
    feedRequirementPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "nutrientType", ascending = TRUE),
        Pivoting(code = "feedBaseUnit", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),
        Pivoting(code = "estimator", ascending = FALSE)
    )

    ## Query the data
    feedRequirementQuery = GetData(
        key = feedRequirementKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = feedRequirementPivot
    )

    setkeyv(feedRequirementQuery, cols = c("geographicAreaM49", "timePointYears"))
    ## Convert time to numeric
    feedRequirementQuery[, timePointYears := as.numeric(timePointYears)]
    feedRequirementQuery[, list(geographicAreaM49, timePointYears,
                                EDemand_lb = Value_estimator_2,
                                EDemand_ub = Value_estimator_3)]
}
