##' Function to compute the weights of the ensemble models
##'
##' @param  x A numeric vector to be imputed.
##' @param fits The fitted value from the models.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @export


computeEnsembleWeight = function(x, fits, restrictWeights = TRUE,
    maximumWeights = 0.7){
    benchmark = x
    error = sapply(fits,
        FUN = function(x){
            computeErrorRate(x = benchmark, fit = x)
            }
        )
    ## NOTE (Michael): Maybe change this to uniform weight
    error[error < 1e-3] = mean(error[error >= 1e-3], na.rm = TRUE)
    weights = (1/error^2)/sum(1/error^2, na.rm = TRUE)
    weights[is.na(weights)] = 0
    if(restrictWeights & any(weights > maximumWeights)){
        weights[weights < maximumWeights] =
            weights[weights < maximumWeights] *
                ((1 - maximumWeights)/
                 sum(weights[weights < maximumWeights]))
        weights[weights > maximumWeights] = maximumWeights
    }
    weights
}    
