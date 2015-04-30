##' Function to standardize items by calorie
##'
##' Calorie standardization is simply adding up all the calories
##' within each standardized commodity grouping.
##'
##' @param data A data.taboe containing the data
##' @param commodityTree The data.table specifying the standardization
##' relationship.
##' @param standardizeVariable The vector of column names which
##' correspond to the variables requiring standardization.
##' @param standardizationKey The vector columns which the data.table
##' will perform the aggregation.
##'
##' @return A data.table with standardized calorie

calorieStandardization = function(data, commodityTree, standardizeVariable,
                                  standardizationKey){
    commodityTreeCopy = copy(commodityTree)
    setnames(commodityTreeCopy, old = "cpc_children_code", new = itemVar)

    dataTree = merge(data, commodityTreeCopy, by = itemVar, all.x = TRUE)

    standardized =
        dataTree[, lapply(standardizeVariable, FUN = function(x) sum(.SD[[x]])),
                 by = c(standardizationKey)]

    setnames(standardized, old = paste0("V", 1:length(standardizeVariable)),
             new = standardizeVariable)
    standardized
}
