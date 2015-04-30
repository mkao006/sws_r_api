##' Function to get the elements corresponding to items.
##'
##' This function should be part of the utility package.
##' 
##' @param measuredItemCPC The list of items codes under the CPC
##' classification.
##'
##' @return A data.table object returning the corresponding element
##' tuples of the specific items.

getProductionElement = function(measuredItemCPC){
    condition =
        paste0("WHERE cpc_code IN (",
               paste0(shQuote(as.character(measuredItemCPC)),
                      collapse = ", "), ")")
    yieldFormula =
        GetTableData(schemaName = "ess",
                     tableName = "item_yield_elements",
                     whereClause = condition)
    yieldFormula
}
