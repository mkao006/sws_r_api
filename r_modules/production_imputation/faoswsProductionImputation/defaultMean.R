##' The default mean model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMean = function(x){
    T = length(x)
    meanFit = rep(mean(x, na.rm = !all(is.na(x))), T)
    meanFit
}
