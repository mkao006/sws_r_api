##' Function to compute the fits of all the component model
##'
##' @param x A numeric vector
##' @param ensembleModel A list of component models to be used to
##' build the ensemble.
##' 
##' @export


computeEnsembleFit = function(x, ensembleModel){
    lapply(ensembleModel,
           FUN = function(x, value) x(value), value = x)
}
