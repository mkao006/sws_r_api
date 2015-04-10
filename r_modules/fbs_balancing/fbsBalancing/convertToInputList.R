convertToInputList = function(data, areaVar = "geographicAreaM49",
    yearVar = "timePointYears",
    meanVariables = c("Imports", "Exports", "Seed", "Losses", "Industrial",
        "Food", "Feed", "Stock"),
    sdVariables = c("Imports.sd", "Exports.sd"),
    feedDemandVariables = c("EDemand_lb", "EDemand_ub")){


    splitYearTable = function(countryData){
        splitedList = split(countryData, countryData[[yearVar]])
        lapply(splitedList, FUN = function(x)
            tableToInput(x,
                         meanVariables = meanVariables,
                         sdVariables = sdVariables,
                         feedDemandVariables = feedDemandVariables)
               )
    }
    
    splitByCountry = split(data, data[[areaVar]])
    lapply(splitByCountry, splitYearTable)
}
