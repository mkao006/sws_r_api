## calculateReliability = function(data, mirroredFlag = "m", tolerance = 0.05){
##     tmp = data[!(data[[flagPrefix]] %in% mirroredFlag) &
##                !(data[[paste0("reverse_", flagPrefix)]] %in% mirroredFlag), ]
    
##     reliability =
##         tmp[,sum((.SD[[valuePrefix]] - .SD[[paste0("reverse_", valuePrefix)]])/
##                  .SD[[valuePrefix]] <= tolerance)/.N,
##              by = c(reportingCountryVar, yearVar)]
##     setnames(reliability, old = c(reportingCountryVar, "V1"),
##              new = c(areaVar, "Value_measuredElement_RELIDX"))
##     reliability[, `:=`(c("flagObservationStatus_measuredElement_RELIDX",
##                          "flagMethod_measuredElement_RELIDX"),
##                        list("E", "e"))]
##     reliability
## }



calculateReliability = function(data, reportingCountry, partnerCountry, yearVar,
    concordance, plot = FALSE){


    yearData = split(data, data[[yearVar]])
    
    
    calculateEigenReliability = function(data){
        singleYearGraph =
            graph.data.frame(data[, c(reportingCountry, partnerCountry,
                                      concordance), with = FALSE],
                             directed = FALSE)
        reliability =
            evcent(singleYearGraph, weights = data[[concordance]])$vector
        reliabilityTable =
            data.table(geographicAreaM49 = names(reliability),
                       timePointYears = unique(data[[yearVar]]),
                       reliability = reliability)
        reliabilityTable
    }

    do.call("rbind", lapply(yearData, FUN = calculateEigenReliability))
}
