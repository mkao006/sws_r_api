##' Function to obtain total annual feed requirement for a whole
##' country in kilo calorie.
##'
##' @return A data.table object containing the point estimate of the
##' total energy feed requirement.

getFeedRequirementData = function(){
    feedRequirementKey = DatasetKey(
        domain = "feed",
        dataset = "total_feed",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "nutrientType",
                      keys = "1"),
            Dimension(name = "estimator",
                      keys = "1"),
            Dimension(name = "feedBaseUnit",
                      keys = "1"),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    feedRequirementPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "nutrientType", ascending = TRUE),
        Pivoting(code = "feedBaseUnit", ascending = TRUE),
        Pivoting(code = yearVar, ascending = TRUE),
        Pivoting(code = "estimator", ascending = FALSE)
    )

    ## Query the data
    feedRequirementQuery = GetData(
        key = feedRequirementKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = feedRequirementPivot
    )

    ## Convert GJ to Kcal
    valueColumns = grep("Value", colnames(feedRequirementQuery), value = TRUE)
    feedRequirementQuery[, `:=`(c(valueColumns),
                                lapply(valueColumns,
                                       FUN = function(x) gj2kcal(get(x))))]

    setkeyv(feedRequirementQuery, cols = c("geographicAreaM49", "timePointYears"))
    ## Convert time to numeric
    feedRequirementQuery[, timePointYears := as.numeric(timePointYears)]
    feedRequirementQuery
}
