weight2flag = function(flagObservationWeight, flagTable){
    index = match(flagObservationWeight, flagTable$flagObservationWeight)
    as.character(flagTable$flagObservationStatus[index])
}
