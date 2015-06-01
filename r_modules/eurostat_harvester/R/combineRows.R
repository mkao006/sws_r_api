##' Combine Rows
##' 
##' Combine multiple Eurostat value/flags into one final value/flag pair. 
##' Multiple Eurostat elements may map to one CPC (for example, "C1120" and 
##' "C1130" are both wheat) and thus the values should be added to get the 
##' resulting value to put in the agriculture tables.
##' 
##' @param values A numeric vector containing the values from the multiple 
##'   observations.
##' @param flagObservationStatus A character vector containing the SWS 
##'   observation flags corresponding to values.
##' @param flagMethod A character vector containing the SWS method flags 
##'   corresponding to values.
##' @param Metadata A character vector containing the Metadata assosciated with 
##'   each element (usually just to pass back, unless updated).
##' 
##' @return A list of length four containing the aggregated value, aggregated
##'   observation flag, aggregated method flag, and new metadata.
##' 

combineRows = function(values, flagObservationStatus, flagMethod, Metadata){
    
    ## Data Quality checks
    stopifnot(length(values) == length(flagObservationStatus))
    stopifnot(length(values) == length(flagMethod))
    stopifnot(length(values) == length(Metadata))
    
    ## Length is 1, no need to aggregate or do anything
    if(length(values) == 1){
        out = list(values, flagObservationStatus, flagMethod, Metadata)
    } else if(any(flagObservationStatus == "M")){
        out = list(0, "M", "u", "/")
    } else {
        out = list(sum(values), "", "h", "aggregation across codes")
    }
    names(out) = c("Value", "flagObservationStatus", "flagMethod", "Metadata")
    return(out)
}