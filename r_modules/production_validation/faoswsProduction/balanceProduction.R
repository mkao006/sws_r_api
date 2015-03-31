##' Function to compute production when new area harvested and yield
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

balanceProduction = function(data, imputationParameters, processingParameters){

    ### Data Quality Checks
    if(!exists("ensuredImputationData") || !ensuredImputationData)
        ensureImputationInputs(data = data,
                               imputationParameters = imputationParameters)
    
    ### Type "p" instead of "processingParameters"
    p = processingParameters
        
    ### Impute only when area and yield are available and production isn't
    filter = data[,!is.na(get(p$areaHarvestedValue)) & # area is available
                    is.na(get(p$productionValue)) &    # production is missing
                   !is.na(get(p$yieldValue))]          # yield is missing

    data[filter, c(p$productionValue) :=
             get(p$areaHarvestedValue) * get(p$yieldValue)]
    data[filter, c(p$productionObservationFlag) := aggregateObservationFlag(
        get(p$areaHarvestedObservationFlag), get(p$yieldObservationFlag),
        flagTable = imputationParameters$flagTable)]
    ## Wrap last call in invisible() so no data.table is returned
    invisible(data[filter, c(p$productionMethodFlag) :=
                 imputationParameters$newMethodFlag])
}
