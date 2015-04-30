##' Function to obtain the population data
##'
##' @return A data.table containing the population by country and
##' year.
##' 

getPopulationData = function(){
    populationKey = DatasetKey(
        domain = "population",
        dataset = "population",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "measuredElementPopulation",
                      keys = "11"),
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
    populationQuery[, Value_measuredElementPopulation_11:=
                        Value_measuredElementPopulation_11 * 1000]
    
    ## Convert time to numeric
    populationQuery[, timePointYears := as.numeric(timePointYears)]
    populationQuery
}
