##' Merge Reverse Trade
##' 
##' @param data The mirrored Comtrade data, typically as produced via a call to
##' getComtradeMirroredData.
##' @param elementTable A data.frame with columns type, import, reimport,
##' export, and reexport.  The table should have type descriptions in the type
##' column (such as quantity, value, and unit_value) and element codes in the
##' other four columns.  Thus, this object specifies which element codes
##' correspond to which trade flow, and is used to reverse the trades.
##' @param reportingCountryVar The column name of data corresponding to the
##' reporting country.
##' @param partnerCountryVar The column name of data corresponding to the
##' partner country.
##' @param valuePrefix The column name of data which contains the trade value.
##' @param flagPrefix The column name of data which contains the trade flag.
##' 
##' @return A data.table object similar to data except that the reversed values
##' are appended as two new columns (one for value, one for flag).
##' 

mergeReverseTrade = function(data, elementTable,
                             reportingCountryVar = "reportingCountryM49",
                             partnerCountryVar = "partnerCountryM49",
                             valuePrefix = "Value", flagPrefix = "flagTrade"){
    origin = copy(data)
    reverse = copy(data)
    
    setnames(reverse,
             old = c(reportingCountryVar, partnerCountryVar, valuePrefix,
                 flagPrefix),
             new = c(partnerCountryVar, reportingCountryVar,
                 paste0("reverse_", valuePrefix), paste0("reverse_", flagPrefix)))

    ## Create reverse mapping
    map1 = elementTable[, c("import", "export")]
    colnames(map1) = c("origin", "to")
    map2 = elementTable[, c("export", "import")]
    colnames(map2) = c("origin", "to")
    reversionTable = rbind(map1, map2)

    # Changing elementVar (measuredElementTrade) to reverse element
    reverse[, `:=`(c(elementVar),
                   reversionTable[match(measuredElementTrade,
                                        reversionTable$origin), "to"])]
    
    originWithReverse =
        merge(origin, reverse,
              by = c(reportingCountryVar, partnerCountryVar, elementVar,
                  itemVar, yearVar))
    originWithReverse
}
