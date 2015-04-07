getReliabilityIndex = function(dataContext){
    countries =
        GetCodeList(domain = "trade",
                    dataset = "reliability_index",
                    dimension = "geographicAreaM49")[type == "country", code]
    
    dimensions =
        list(Dimension(name = "geographicAreaM49",
                       keys = countries),
             Dimension(name = "measuredElement",
                       keys = "RELIDX"),
             Dimension(name = "timePointYears",
                       keys = dataContext@dimensions$timePointYears@keys))

    newKey =
        DatasetKey(domain = "trade",
                   dataset = "reliability_index",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
    )

    reliabilityData = GetData(key = newKey, pivoting = newPivot)
    reliabilityData
}
