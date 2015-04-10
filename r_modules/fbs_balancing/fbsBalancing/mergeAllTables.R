
mergeAllTables = function(meanTable, stdTable, feedRequirementTable, keys){

    meanStdTable = merge(meanTable, stdTable, by = keys, all.x = TRUE)
    finalTable =
        merge(meanStdTable, feedRequirement,
              by = intersect(colnames(meanStdTable), colnames(feedRequirement)),
              all.x = TRUE)
    
    stdColumns = colnames(stdTable)[!colnames(stdTable)%in% keys]
    
    finalTable[, `:=`(c(stdColumns),
                      lapply(stdColumns,
                             FUN = function(x){
                                 values = get(stdColumns)
                                 values[is.na(values)] = 0
                                 values
                             }))]

    setnames(finalTable,
             old = c("Value_measuredElement_250",
                 "Value_measuredElement_251", 
                 "Value_measuredElement_252",
                 "Value_measuredElement_55252", 
                 "Value_measuredElement_51202", 
                 "Value_measuredElement_51502", 
                 "Value_measuredElement_51422", 
                 "Value_measuredElement_55202", 
                 "Value_measuredElement_50712",
                 "Value_measuredElementTrade_SD5600",
                 "Value_measuredElementTrade_SD5900"),
             new = c("Production", "Imports", "Exports", "Seed", "Losses",
                 "Industrial", "Food", "Feed", "Stock", "Imports.sd",
                 "Exports.sd"))

    ## TODO (Michael): Need to add in recover when feed requirement is not available.
    finalTable
}
