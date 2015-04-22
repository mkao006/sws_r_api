##' This function compute the non-parametric likelihood of each table
##' sampled.
##'
##' @param balancedObject The Object returned by the balancing
##' algorithm.
##'
##' @return A vector of likelihood corressponding to each table.

npmle = function(balancedObject){
    optimalMatrix = sapply(balancedObject@tables, c)
    colSums(apply(optimalMatrix, 2, sampleDensity))
}
