##' Computes the flag weights based on the Kullback Leibler Divergence.
##'
##' @param x The vector of new representation (q)
##' @param benchmark The assumed true representation (p)
##'
##' @export


computeEntropyWeights = function(x, benchmark){
    weights = 1/(1 + KL.empirical(benchmark, x))

    if(weights == 1)
        weights = 1 - 1e-5

    weights
}
