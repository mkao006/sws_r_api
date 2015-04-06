##' Function to check the consistency between area harvested and
##' production.
##'
##' The function checks whether zeroes occur concordantly.
##'
##' @param production Production value
##' @param areaHarvested Area harvested value
##'
##' @export
##' 

paZeroEquality = function(production, areaHarvested){
    productionZero = which(production == 0)
    areaHarvestedZero = which(areaHarvested == 0)
    list(zeroEquality = setequal(productionZero, areaHarvestedZero),
         productionInequality = setdiff(areaHarvestedZero, productionZero),
         areaHarvestedInequality = setdiff(productionZero, areaHarvestedZero)
         )
}
