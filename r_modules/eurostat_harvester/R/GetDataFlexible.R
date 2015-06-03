##' Get Data Flexible
##' 
##' This function is a wrapper around GetData that cleans up the provided codes.
##' In some cases, a code will be passed that is invalid.  This function checks
##' that all codes are valid before running GetData and thus avoids throwing an
##' error.  If there are extra (invalid) codes, they are removed and a warning
##' is issued.
##' 
##' @param key See ?GetData.
##' @param flags See ?GetData.
##' @param normalized See ?GetData.
##' @param pivoting See ?GetData.
##' 
##' @return See ?GetData.
##' 

GetDataFlexible = function(key, flags = TRUE, normalized = TRUE, pivoting){
    domain = key@domain
    dataset = key@dataset
    key@dimensions = lapply(key@dimensions, function(dimen){
        allowedCodes = GetCodeList(domain = domain, dataset = dataset,
                                   dimension = dimen@name)$code
        dimen@keys = dimen@keys[dimen@keys %in% allowedCodes]
        dimen
    })
    GetData(key)
}