balanceTrade = function(data){
    balanceData = copy(data)

    balanceData[, reliableValue :=
                    ifelse(reportingReliability >= partnerReliability,
                           get(valuePrefix),
                           get(paste0("reverse_", valuePrefix)))]
    balanceData[, reliabilityFlag :=
                    ifelse(reportingReliability >= partnerReliability,
                           get(flagPrefix), "b")]

    stdData =
        balanceData[, sqrt(sum((.SD[[valuePrefix]] - reliableValue)^2)/.N),
                 by = c(reportingCountryVar, elementVar, itemVar, yearVar)]
    setnames(stdData,
             old = c(reportingCountryVar, "V1"),
             new = c(standardCountryVar, "Value"))
    stdData[, measuredElementTrade := paste0("SD", measuredElementTrade)]
    stdData[, `:=`(c("flagObservationStatus", "flagMethod"),
                   list("E", "e"))]
    list(balanceData = balanceData, stdData = stdData)
}
