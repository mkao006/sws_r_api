##' The function calculates feed availability
##'
##' Feed availability here is defined as the residual item after
##' accounting for all other elements. Any item which has negative
##' feed availability is assigned zero.
##'
##' @param data The data.table which represents the contingency table without feed.
##' @param areaVar The column name which represents area.
##' @param yearVar the column name which represents year.
##' @param production The column name corresponding to production.
##' @param import The column name corresponding to imports.
##' @param export The column name corresponding to exports.
##' @param seed The column name corresponding to seed.
##' @param loss The column name corresponding to loss.
##' @param industrialUse The column name corresponding to industrial utilization.
##' @param food The column name corresponding to food.
##'
##' @return A new data.table object with two new columns, total feed
##' availability and the weights of feed availability.
##' 


calculateFeedAvailability = function(data, areaVar, yearVar, production, import,
    export, seed, loss, industrialUse, food){

    dataCopy = copy(data)
    dataCopy[, `:=`(c("Value_measuredElementCalorie_feedAvail"),
                    rowSums(dataCopy[, c(production, import), ,with = FALSE]) -
                    rowSums(dataCopy[, c(export, seed, loss, industrialUse, food),
                                     with = FALSE]))]
    dataCopy[Value_measuredElementCalorie_feedAvail < 0,
             Value_measuredElementCalorie_feedAvail := 0]
    dataCopy[, feedAvailableWeights :=
                 computeRatio(Value_measuredElementCalorie_feedAvail,
                              sum(Value_measuredElementCalorie_feedAvail)),
             by = c(areaVar, yearVar)]
    dataCopy
}
