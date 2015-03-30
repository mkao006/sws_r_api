##' Build CPC Hierarchy
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

buildCPCHierarchy = function(data, cpcItemVar, levels = 3){
    data[, `:=`(c(paste0("cpcLvl", 1:levels)),
                lapply(1:levels, FUN = function(x)
                    factor(substr(data[[cpcItemVar]], 1, x))))]
}