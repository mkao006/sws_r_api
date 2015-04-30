##' Function to calculate the residual
##'
##' @param data The contingency table with all the elements included.
##' @param production The column name corresponds to production.
##' @param import The column name corresponds to import.
##' @param export The column name corresponds to export.
##' @param seed The column name corresponds to seed.
##' @param loss The column name corresponds to loss.
##' @param industrialUse The column name corresponds to industrial utilization.
##' @param food The column name corresponds to food.
##' @param feed The column name corresponds to feed.
##' @param residualVariable The name of the new column in which the
##' result of the residual is to be assigned.
##'
##' @return A data.table with additional column corresponding to the residual.

calculateResidual = function(data, production, import, export, seed, loss,
    industrialUse, food, feed, residualVariable){

    dataCopy = copy(data)
    dataCopy[, `:=`(c(residualVariable),
                    rowSums(dataCopy[, c(production, import), ,with = FALSE]) -
                    rowSums(dataCopy[, c(export, seed, loss, industrialUse, food,
                                         feed),
                                     with = FALSE]))]
}
