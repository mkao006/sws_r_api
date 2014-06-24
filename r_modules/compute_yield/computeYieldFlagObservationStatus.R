

computeYieldFlagObservationStatus =
    function(productionFlagObservationStatus,
             areaHarvestedFlagObservationStatus, flagTable){

        ## Compute the information weight of the flag
        productionInformationWeight =
            flag2weight(productionFlagObservationStatus, flagTable)
        areaHarvestedInformationWeight =
            flag2weight(areaHarvestedFlagObservationStatus, flagTable)

        yieldFlagObservationStatus =
            weight2flag(pmin(productionInformationWeight,
                             areaHarvestedInformationWeight),
                        flagTable)

        yieldFlagObservationStatus
    }
