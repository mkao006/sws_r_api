##' Function to obtain the primary item under the CPC classification
##'
##' This function should be part of the utility package and also
##' should be refined.
##'
##' @return The list of CPC codes which corresponds to primary items.

getPrimaryMeasuredItemCPC = function(){
    itemTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = itemVar)
    itemTable[!is.na(type), code]
}
