balanceTrade = function(data){
    balanceData = copy(data)

    balanceData[, reliableValue :=
                 ifelse(reportingReliability >= partnerReliability,
                        .SD[[valuePrefix]],
                        .SD[[paste0("reverse_", valuePrefix)]])]
    balanceData[, reliabilityFlag :=
                    aggregateObservationFlag(balanceData[[flagPrefix]],
                                             balanceData[[paste0("reverse_",
                                                                 flagPrefix)]],
                                             flagTable = tradeFlowFlagTable)]
                    

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
