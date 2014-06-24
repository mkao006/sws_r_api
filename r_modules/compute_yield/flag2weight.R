flag2weight = function(flagObservationStatus, flagTable){
    index = match(flagObservationStatus, flagTable$flagObservationStatus)
    as.numeric(flagTable$flagObservationWeights[index])
}

