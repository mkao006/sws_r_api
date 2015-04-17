sampleBalancedTable = function(contingencyTableList, selectedCountry,
    selectedYear, sanityCheck = FALSE, maxErr = 10, ...){
    
    balancingFunction =
        balanceFBS(FBS = contingencyTableList,
                   sanityCheck = sanityCheck, maxErr = maxErr)

    sampledTables =
        balancingFunction(Country = selectedCountry, year = selectedYear, ...)

    if(is.null(sampledTables))
        sampledTables =
            new("conTa",
                bestTab =
                    contingencyTableList[[selectedCountry]][[selectedYear]]$data)
    sampledTables
}
