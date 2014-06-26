##' This function performs aggregation on the flag
##'
##' The aggregation is based on the weight of the flag as designated
##' by the table. Official sources has high weight while data from
##' unreliable sources has low weight.
##'
##' @param ... Flags, see detail
##' @param flagTable The table which maps the relationship between the
##' flag and its information weight.
##'
##' @details The arguement for flag is the same to the function
##' sum. Multiple instances of the flag can be pass in to the function
##' to be aggregated.
##'
##' @seealso \code{\link{sum}}
##'
##' @export
##' 
aggregateFlag = function(..., flagTable){
    flags = list(...)

    ## Convert flags to numeric information weight as determined by
    ## the table.
    weights = lapply(X = flags, FUN = flag2weight, flagTable = flagTable)

    ## Aggregate the weight by taking the lowest weight
    aggregatedWeights = do.call(pmin, weights)

    ## Convert weight back to flag
    aggregatedFlag = weight2flag(aggregatedWeights, flagTable)
    aggregatedFlag
}
