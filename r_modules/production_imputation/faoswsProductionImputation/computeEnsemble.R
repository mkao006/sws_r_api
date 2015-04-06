##' Function to combine the ensembles
##'
##' @param fits A list of fitted values from models.
##' @param weights A vector for the weights of each model.
##' @export


computeEnsemble = function(fits, weights){
    fitsMatrix = matrix(unlist(fits), ncol = length(fits))
    fitsMatrix[is.na(fitsMatrix)] = 0
    c(fitsMatrix %*% weights)
}
