##' Function to extract the calorie conversion factors.
##'
##' @param items The list of items in CPC code.
##'
##' @return A data.table with the calorie conversion factors.

getCalorieConversionFactor = function(items){
    nutrientKey =
        DatasetKey(domain = "suafbs",
                   dataset = "nutrient_factors_cpc",
                   dimensions = list(
                       ## NOTE (Michael): We take energy in Kcal here
                       Dimension(name = "measuredElementNutritive",
                                 keys = "904"),
                       Dimension(name = "measuredItemCPC",
                                 keys = items),
                       Dimension(name = "timePointFake",
                                 keys = "1")
                   )
                   )

    nutrientPivot = c(
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointFake", ascending = TRUE),
        Pivoting(code = "measuredElementNutritive", ascending = TRUE)
    )

    nutrientQuery =
        GetData(key = nutrientKey,
                flags = FALSE,
                normalized = FALSE,
                pivoting = nutrientPivot)
    nutrientQuery[, timePointFake := NULL]
    nutrientQuery
}
