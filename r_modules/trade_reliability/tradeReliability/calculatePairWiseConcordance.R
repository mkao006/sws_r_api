##' Calculate Pairwise Concordance
##' 
##' @param data A data.table object containing the trade data and the reversed
##' values.  This object should come from the mergeReverseTrade function to
##' ensure it has the right column structure.
##' @param reportingCountryVar The column name of data corresponding to the
##' reporting country.
##' @param partnerCountryVar The column name of data corresponding to the
##' partner country.
##' @param yearVar The column name of data corresponding to the year variable.
##' @param mirroredFlag The flag which indicates that an observation of data
##' is mirrored.
##' @param valuePrefix The column name of data which contains the trade value.
##' @param flagPrefix The column name of data which contains the trade flag.
##' 
##' @return Data.table with coefficients of concordance for given reporters, 
##' partners and years.
##' 

calculatePairWiseConcordance = function(data,
                                    reportingCountryVar = "reportingCountryM49",
                                    partnerCountryVar = "partnerCountryM49",
                                    yearVar = "timePointYears",
                                    mirroredFlag = "m",
                                    valuePrefix = "Value",
                                    flagPrefix = "flagTrade"){

    tmp = data[!(data[[flagPrefix]] %in% mirroredFlag) &
               !(data[[paste0("reverse_", flagPrefix)]] %in% mirroredFlag), ]
    concordance =
        tmp[,list(concordance =
                       sum(abs(computeRatio(.SD[[valuePrefix]] -
                                            .SD[[paste0("reverse_", valuePrefix)]],
                                            .SD[[valuePrefix]])) <= 0)/.N),
            by = c(reportingCountryVar, partnerCountryVar, yearVar)]
    concordance
}
