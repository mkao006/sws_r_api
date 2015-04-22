##' Function to predict the density based on the estimated kernel
##' density.
##'
##' @param sample The sample to estimate the density
##'
##' @return The density corresponding to the sample.
densityEstimation = function(sample){
    sample.dens = density(sample)
    predictDensity(sample.dens, sample)
}
