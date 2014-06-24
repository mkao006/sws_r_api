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
computeYield = function(production, areaHarvested){
    as.numeric(ifelse(production == 0 | areaHarvested == 0, NA,
                      production/areaHarvested))
}
