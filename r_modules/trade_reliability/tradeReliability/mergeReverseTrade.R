mergeReverseTrade = function(data){
    origin = copy(data)
    reverse = copy(data)
    setkeyv(origin, c(reportingCountryVar, partnerCountryVar, elementVar,
                      itemVar, yearVar))
    setkeyv(reverse, c(reportingCountryVar, partnerCountryVar, elementVar,
                      itemVar, yearVar))
    
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

    reverse[, `:=`(c(elementVar),
                   reversionTable[match(measuredElementTrade,
                                        reversionTable$origin), "to"])]
    
    originWithReverse =
        merge(origin, reverse,
              by = c(reportingCountryVar, partnerCountryVar, elementVar,
                  itemVar, yearVar))
    originWithReverse
}
