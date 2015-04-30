##' Function to standardize Trade
##'
##' A specific function is written for the standard deviation as the
##' formula for computing the aggregate of standard deviation is
##' different to the mean.
##'
##' @param data The data frame containing the trade standard deviation.
##' @param commodityTree The data frame which specifies the
##' standardization relationship.
##' @param weightVariable The conversion variable, in the case of FBS
##' standardization, it is the calorie conversion factor.
##' @param tradeStandardDeviationVariable The columns names
##' correspoonding to the standard deviations.
##' @param standardizationKey The keys for which standardization
##' should be performed.
##'
##' @param A data.table with standardized standard deviation.



standardizeTradeStd = function(data, commodityTree, weightVariable,
    tradeStandardDeviationVariable, standardizationKey){
    commodityTreeCopy = copy(commodityTree)
    setnames(commodityTreeCopy, old = "cpc_children_code", new = itemVar)

    dataTree = merge(data, commodityTreeCopy, by = itemVar, all.x = TRUE)
    standardized =
        dataTree[, lapply(tradeStandardDeviationVariable,
                          FUN = function(x){
                              if(length(.SD[[x]]) == 0){
                                  tmp = .SD[[x]]
                              } else {
                                  tmp = sqrt(sum(.SD[[weightVariable]]^2 * .SD[[x]]^2))
                              }
                          }),
                 by = c(standardizationKey)]

    setnames(standardized,
             old = paste0("V", 1:length(tradeStandardDeviationVariable)),
             new = tradeStandardDeviationVariable)
    standardized
}
