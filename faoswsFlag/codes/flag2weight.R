##' Function to convert flag to information weight.
##'
##' The flag is converted to a numeric information weight for
##' quantification and further manipulation.
##'
##' @param flagObservationStatus The flag
##' @param flagTable The table mapping the flag and weights
##'
##' @export
##' 

flag2weight = function(flagObservationStatus,
                       flagTable = faoswsFlagTable){
    
    index = match(flagObservationStatus, flagTable$flagObservationStatus)
    as.numeric(flagTable$flagObservationWeights[index])
}

