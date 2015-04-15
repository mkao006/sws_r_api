##' This function extracts the total food calorie available in the old
##' Food Balance Sheet.
##'
##' @param fbsCode The list of codes corresponding to the Food Balance
##' Sheet Items in the new system. Generally they are the same as the
##' old Food Balance Sheet with a prefix "S". (.e.g. Wheat is S2011).
##'
##' @return A data.table containing the Total Food in Kilo Calorie.

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
