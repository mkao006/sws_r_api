##' Function to estimation the sample density
##'
##' @param sample The sample to estimate the density
##'
##' @return The density corresponding to the sample.
##' 

sampleDensity = function(sample){
    sample.dens = density(sample, n = length(sample))
    ## predict density based on splines
    with(sample.dens, spline(x = x, y = y, xout = sample))$y
}
