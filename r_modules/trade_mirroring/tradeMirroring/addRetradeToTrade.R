addRetradeToTrade = function(data, importQuantity, reimportQuantity,
    exportQuantity, reexportQuantity, importValue, reimportValue,
    exportValue, reexportValue){
    missingCol = # Detect columns absent in dataset
        setdiff(c(importQuantity, reimportQuantity, exportQuantity,
                  reexportQuantity, importValue, reimportValue, exportValue,
                  reexportValue),
                colnames(data))
    
    if(length(missingCol) > 0) # Creating absent columns filled with numeric NA
        data[, `:=`(c(missingCol), as.numeric(NA))]
    
    data[, `:=`(c(importQuantity, exportQuantity, importValue, exportValue),
                list(
                  # Condition for the first element of vector (importQuantity)
                  ifelse(
                    # Are all values NA?
                    apply(is.na(.SD[, c(importQuantity, reimportQuantity),
                                    with = FALSE]), 
                          MARGIN = 1, 
                          FUN = all),
                    NA,
                    # If there are !NA values than return sum of import and reimp
                    rowSums(.SD[, c(importQuantity, reimportQuantity),
                                with = FALSE], 
                            na.rm = TRUE)),
                  ifelse(apply(is.na(.SD[, c(exportQuantity, reexportQuantity),
                                         with = FALSE]), 1, all), NA,
                         rowSums(.SD[, c(exportQuantity, reexportQuantity),
                                     with = FALSE], na.rm = TRUE)),
                  ifelse(apply(is.na(.SD[, c(importValue, reimportValue),
                                         with = FALSE]), 1, all), NA,
                         rowSums(.SD[, c(importValue, reimportValue),
                                     with = FALSE], na.rm = TRUE)),
                  ifelse(apply(is.na(.SD[, c(exportValue, reexportValue),
                                         with = FALSE]), 1, all), NA,
                         rowSums(.SD[, c(exportValue, reexportValue),
                                     with = FALSE], na.rm = TRUE))))]
    
    ## NOTE (Michael): Make sure the type are numeric
    valueColumns = grep(valuePrefix, colnames(data), value = TRUE)
    data[, `:=`(c(valueColumns),
                lapply(valueColumns, FUN = function(x) as.numeric(.SD[[x]])))]
    
    # Fill re-stuff with NULL value
    #
    # There is data.table::'[' warning about 
    # Adding new column 'flagTrade_measuredElementTrade_(56|59)(12|23)' then assigning NULL (deleting it).
    data[, `:=`(c(reimportQuantity, reexportQuantity,
                  reimportValue, reexportValue,
                  gsub(valuePrefix, flagPrefix, reimportQuantity),
                  gsub(valuePrefix, flagPrefix, reexportQuantity),
                  gsub(valuePrefix, flagPrefix, reimportValue),
                  gsub(valuePrefix, flagPrefix, reexportValue)), NULL)]
    
    ## NOTE (Michael): What do we do with the retrades in the data base?
    
    data
}
