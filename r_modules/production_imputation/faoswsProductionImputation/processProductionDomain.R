##' This is a wrapper for all the data manipulation step before the
##' preparation of the imputation.
##'
##' @param data The data
##' @param productionValue The name of the production variable.
##' @param areaHarvestedValue The name of the area harvested variable.
##' @param yieldValue The column name of the yield variable.
##' @param productionObservationFlag The observation flag of production.
##' @param productionMethodFlag The method flag of production.
##' @param areaHarvestedObservationFlag The observation flag of area
##' harvested.
##' @param areaHarvestedMethodFlag The method flag of area
##' harvested.
##' @param yieldObservationFlag The observation flag of yield.
##' @param yieldMethodFlag The method flag of yield.
##' @param removePriorImputation logical, whether prior imputation
##' should be removed.
##' @param removeConflictValues logical, whether conflict area
##' harvested value and production should be removed.
##' @param imputedFlag Flag value corresponding to values from prior
##' imputation, ignored if removePriorImputation is FALSE.
##' @param naFlag Flag value for missing values.
##' @param byKey The unique key identifier.
##' @export
##' 



processProductionDomain = function(data, productionValue,
    areaHarvestedValue, yieldValue, yearValue, productionObservationFlag,
    areaHarvestedObservationFlag, yieldObservationFlag,
    productionMethodFlag, areaHarvestedMethodFlag, yieldMethodFlag,
    removePriorImputation = TRUE, removeConflictValues = TRUE,
    imputedFlag = "E",  naFlag = "M", byKey = "areaCode"){
    
    if(removePriorImputation){
        removeImputation(data = data,
                         value = areaHarvestedValue,
                         flag = areaHarvestedObservationFlag,
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)
        
        removeImputation(data = data,
                         value = productionValue,
                         flag = productionObservationFlag,
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)

        removeImputation(data = data,
                         value = yieldValue,
                         flag = yieldObservationFlag,
                         imputedFlag = imputedFlag,
                         naFlag = naFlag)
    }

    remove0M(data = data,
             value = areaHarvestedValue,
             flag = areaHarvestedObservationFlag,
             naFlag = naFlag)
    
    remove0M(data = data,
             value = productionValue,
             flag = productionObservationFlag,
             naFlag = naFlag)
    
    remove0M(data = data,
             value = yieldValue,
             flag = yieldObservationFlag,
             naFlag = naFlag)        

    if(removeConflictValues){
        removeZeroConflict(productionValue = productionValue,
                           productionObservationFlag =
                               productionObservationFlag,
                           areaHarvestedValue = areaHarvestedValue,
                           areaHarvestedObservationFlag =
                               areaHarvestedObservationFlag,
                           yieldValue = yieldValue,
                           yieldObservationFlag = yieldObservationFlag,
                           data = data)
    }

#     dataProcessed =
#         removeNoInfo(data = data,
#                      value = yieldValue,
#                      flag = yieldObservationFlag,
#                      byKey = byKey)
#     dataProcessed
    data
}   
