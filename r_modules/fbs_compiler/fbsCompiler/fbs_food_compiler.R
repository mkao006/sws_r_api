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

getTotalFoodCalorie = function(fbsCodes){

    foodKey = DatasetKey(
        domain = "suafbs",
        dataset = "fbs",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "measuredElementSuaFbs",
                      keys = "664"),
            Dimension(name = "measuredItemSuaFbs",
                      keys = fbsCodes),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    foodPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElementSuaFbs", ascending = TRUE)
    )

    ## Query the data
    foodQuery = GetData(
        key = foodKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = foodPivot
    )
        

    ## Convert time to numeric
    foodQuery[, timePointYears := as.numeric(timePointYears)]

    population = getOldFBSPopulationData()

    ## Calculate total calorie available for food from per caput
    ## multiply by population and 365 days.
    foodPopulationQuery =
        merge(foodQuery, population,
              by = c("geographicAreaM49", "timePointYears"), all.x = TRUE)
    foodPopulationQuery[, Value_measuredElementCalorie_FoodTotal :=
                            Value_measuredElementSuaFbs_664 *
                                Value_measuredElementPopulation_21 * 365]

    setnames(foodPopulationQuery,
             old = "measuredItemSuaFbs",
             new = "cpc_standardized_code")
    setkeyv(foodPopulationQuery,
            cols = c("geographicAreaM49", "cpc_standardized_code",
                "timePointYears"))
    
    foodPopulationQuery[, list(geographicAreaM49, cpc_standardized_code,
                               timePointYears,
                               Value_measuredElementCalorie_FoodTotal)]
}
## Need function to get food data




