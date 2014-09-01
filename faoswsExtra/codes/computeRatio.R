##' Function for computing the yield
##'
##' This is a function to compute yield based on production and area
##' harvested.
##'
##' @param production The value of production
##' @param areaHarvested The value of area harvested.
##'
##' @export
##'
##' 

computeRatio = function(numerator, denominator){
    as.numeric(ifelse((numerator == 0 & denominator == 0) |
                      denominator == 0, NA,
                      numerator/denominator))
}
