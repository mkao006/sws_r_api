##' Function to compute area harvested when new production and yield
##' are given.
##'
##' @param data The data.table object containing the data.
##' @param imputationParameters A list of the parameters for the imputation
##' algorithms.  See defaultImputationParameters() for a starting point.
##' @param processingParameters A list of the parameters for the production
##' processing algorithms.  See defaultProductionParameters() for a starting
##' point.
##'
##' @export
##' 

balanceAreaHarvested = function(data, imputationParameters,
                                processingParameters){
    
    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                             imputationParameters = imputationParameters)
    if(!exists("ensuredProductionData") || !ensuredProductionData)
        ensureProductionInputs(data = data,
                               processingParameters = processingParameters)

    ### Save clutter by renaming "processingParameters" to "p" locally.
    p = processingParameters
    
    ### Impute only when area and yield are available and production isn't
    filter = data[,is.na(get(p$areaHarvestedValue)) & # area is missing
                  !is.na(get(p$yieldValue)) &         # yield is available
                  !is.na(get(p$productionValue))]     # production is available
    
    data[filter, c(p$areaHarvestedValue) :=
             computeRatio(get(p$productionValue), get(p$yieldValue))]
    data[filter, c(p$areaHarvestedObservationFlag) := aggregateObservationFlag(
        get(p$productionObservationFlag), get(p$yieldObservationFlag),
        flagTable = imputationParameters$flagTable)]
    ## Wrap last call in invisible() so no data.table is returned
    invisible(data[filter, c(p$areaHarvestedMethodFlag) :=
                       imputationParameters$newMethodFlag])
}
