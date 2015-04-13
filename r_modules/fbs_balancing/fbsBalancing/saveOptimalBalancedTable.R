saveOptimalBalancedTable = function(optimalTable, selectedCountry, selectedYear){

    ## optimalDataTable =
    ##     data.table(cbind(optimalTable@bestTab,
    ##                      Production = rowSums(optimalTable@bestTab)))
    optimalDataTable = data.table(optimalTable@bestTab)
    optimalDataTable[, Imports := Imports * -1]
    optimalDataTable[, measuredItemSuaFbs := rownames(optimalTable@bestTab)]
    optimalDataTable[, geographicAreaM49 := selectedCountry]
    optimalDataTable[, timePointYears := selectedYear]
    
    setnames(optimalDataTable,
             new = c("Value_measuredElement_250",
                 "Value_measuredElement_251", 
                 "Value_measuredElement_252",
                 "Value_measuredElement_55252", 
                 "Value_measuredElement_51202", 
                 "Value_measuredElement_51502", 
                 "Value_measuredElement_51422", 
                 "Value_measuredElement_55202", 
                 "Value_measuredElement_50712"),
             old = c("Production", "Imports", "Exports", "Seed", "Losses",
                 "Industrial", "Food", "Feed", "Stock"))


    finalSaveTable =
        addBalancingFlags(optimalDataTable,
                          valueColumns = grep("Value", colnames(optimalDataTable),
                              value = TRUE),
                          valuePrefix = "Value",
                          flagObsStatusPrefix = "flagObservationStatus",
                          flagMethodPrefix = "flagMethod")
    SaveData(domain = "suafbs", dataset = "fbs_balanced", data = finalSaveTable,
             normalized = FALSE)
}    
