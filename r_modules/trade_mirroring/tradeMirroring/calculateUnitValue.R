calculateUnitValue = function(data, importUnitValue, importUnitValueFlag,
    importTradeValue, importTradeValueFlag, importTradeQuantity,
    importTradeQuantityFlag, exportUnitValue, exportUnitValueFlag,
    exportTradeValue, exportTradeValueFlag, exportTradeQuantity,
    exportTradeQuantityFlag, flagTable = faoswsTradeFlagTable){

    missingCol = setdiff(c(importUnitValue, importTradeValue, importTradeQuantity,
        exportUnitValue, exportUnitValue, exportTradeQuantity), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    
    data[!is.na(data[[importTradeValue]]) & !is.na(data[[importTradeQuantity]]),
         `:=`(c(importUnitValue, importUnitValueFlag),
                list(computeRatio(.SD[[importTradeValue]],
                                  .SD[[importTradeQuantity]]),
                     aggregateObservationFlag(.SD[[importTradeValueFlag]],
                                              .SD[[importTradeQuantityFlag]],
                                              flagTable = flagTable)))]

    data[!is.na(data[[exportTradeValue]]) & !is.na(data[[exportTradeQuantity]]),
         `:=`(c(exportUnitValue, exportUnitValueFlag),
                list(computeRatio(.SD[[exportTradeValue]],
                                  .SD[[exportTradeQuantity]]),
                     aggregateObservationFlag(.SD[[exportTradeValueFlag]],
                                              .SD[[exportTradeQuantityFlag]],
                                              flagTable = flagTable)))]
    
    data
}
