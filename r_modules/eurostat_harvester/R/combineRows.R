##' Combine Rows
##' 
##' Combine multiple Eurostat value/flags into one final value/flag pair. 
##' Multiple Eurostat elements may map to one CPC (for example, "C1120" and 
##' "C1130" are both wheat) and thus the values should be added to get the 
##' resulting value to put in the agriculture tables.
##' 
##' @param values A numeric vector containing the values from the multiple 
##'   observations.
##' @param flags A character vector containing the Eurostat flags corresponding 
##'   to values.
##'   
##' @return A list of length two containing the aggregated value and one flag
##'   which are the result of aggregating all the records.
##'   

combineRows = function(values, flags, missingFlags = c(":", "u", "c", "z")){
    ## Data Quality checks
    stopifnot(length(values) == length(flags))
    
    if(any(flags %in% missingFlags))
        return(list(0, "u"))
    return(list(sum(values), "h"))
}