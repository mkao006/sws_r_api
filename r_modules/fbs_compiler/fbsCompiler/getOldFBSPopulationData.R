##' Obtain the old food balance sheet population data
##'
##' Currently the food is taken from the old Food Balance Sheet, thus
##' in order to obtain the total calorie balance we need to multiply
##' by the population. However, the population used in the past is
##' specific to the Food Balance Sheet and thus we need to take the
##' old Food Balance Sheet population to obtain the total balance.
##'
##' @return A data.table which contains the old Food Balance Sheet
##' Population.


getOldFBSPopulationData = function(){
    populationKey = DatasetKey(
        domain = "population",
        dataset = "population",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            ## NOTE (Michael): This is the old fbs population
            Dimension(name = "measuredElementPopulation",
                      keys = "21"),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    populationPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),
        Pivoting(code = "measuredElementPopulation", ascending = FALSE)
    )

    ## Query the data
    populationQuery = GetData(
        key = populationKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = populationPivot
    )

    ## Convert population in thousands to count
    populationQuery[, Value_measuredElementPopulation_21:=
                        Value_measuredElementPopulation_21 * 1000]
    
    ## Convert time to numeric
    populationQuery[, timePointYears := as.numeric(timePointYears)]
    populationQuery
}
