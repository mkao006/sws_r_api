##' The function disaggregates total feed energy requirement by the
##' relative abunadnce of feed availability.
##'
##' @param data The contingency table with feed availability calculated.
##' @param areaVar The column name which represents area.
##' @param yearVar the column name which represents year.
##' @param feedAvailabilityWeight The column name corresponding to the
##' calculated feed availability weight.
##' @param feedRequirementVar The column name corresponding to the
##' total feed energy requirement.
##' @param feedUtilizationVar The final column name for the assignment
##' of the feed utilization calculated based on the disaggregation.
##'
##' @return A new data.table object with the feed utilization
##' calculated.


disaggregateFeedRequirement = function(data, areaVar, yearVar,
    feedAvailabilityWeight, feedRequirementVar, feedUtilizationVar){
    dataCopy = copy(data)
    dataCopy[, `:=`(c(feedUtilizationVar),
                    list(get(feedRequirementVar) *
                         get(feedAvailabilityWeight))),
             by = c(areaVar, yearVar)]
    dataCopy[is.na(get(feedUtilizationVar)),
             `:=`(c(feedUtilizationVar), 0)]
    dataCopy
}
