##' Function to convert information weight to flag.
##'
##' The function converts a numeric information weight back to flags.
##'
##' @param flagObservationWeight The flag information weight
##' @param flagTable The table mapping the flag and weights
##'
##' @export

weight2flag = function(flagObservationWeight, flagTable){
    index = match(flagObservationWeight, flagTable$flagObservationWeight)
    as.character(flagTable$flagObservationStatus[index])
}
