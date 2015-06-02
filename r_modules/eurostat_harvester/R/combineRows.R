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
##' @param flagRawEurostat A character vector containing the Eurostat flag 
##'   assosciated with each element.  This is needed because a "z" eurostat flag
##'   becomes a missing observation (observation flag = "M") by itself.  But, if
##'   aggregated, it should be considered as an observed value with a value of
##'   0.
##'   
##' @return A list of length four containing the aggregated value, aggregated 
##'   observation flag, aggregated method flag, and new metadata.
##'   

combineRows = function(values, flagObservationStatus, flagMethod, Metadata, flagRawEurostat){
    
    ## Data Quality checks
    stopifnot(length(values) == length(flagObservationStatus))
    stopifnot(length(values) == length(flagMethod))
    stopifnot(length(values) == length(Metadata))
    stopifnot(length(values) == length(flagRawEurostat))
    
    ## Length is 1, no need to aggregate or do anything
    if(length(values) == 1){
        out = list(values, flagObservationStatus, flagMethod, Metadata)
    ## Eurostat flag of "z" indicates not-applicable, and so should not be
    ## considered a missing value but rather a zero during aggregations.
    } else if(all(flagRawEurostat == "z")){
        out = list(0, "M", "u", "/")
    } else if(any(flagObservationStatus == "M" & flagRawEurostat != "z")){
        out = list(0, "M", "u", "/")
    } else {
        values[flagRawEurostat == "z"] = 0
        out = list(sum(values), "", "h", "aggregation across codes")
    }
    names(out) = c("Value", "flagObservationStatus", "flagMethod", "Metadata")
    return(out)
}