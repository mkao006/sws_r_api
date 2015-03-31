##' This function imputes the whole production domain.
##'
##' The function will impute production, area harvested and yield at
##' the same time.
##'
##' Transformation in the yield formula is not allowed and will not be
##' taken into account.
##'
##' @param data The data
##' @param processingParameters A list of the parameters for the production
##' processing algorithms.  See defaultProductionParameters() for a starting
##' point.
##' @param yieldImputationParameters A list of the parameters for the
##' yield imputation.  See defaultImputationParameters() for a starting point.
##' @param productionImputationParameters A list of the parameters for the
##' production imputation.  See defaultImputationParameters() for a starting
##' point.
##'
##' @export
##' 

imputeProductionDomain = function(data, processingParameters,
                                  yieldImputationParameters,
                                  productionImputationParameters){

    ### Data Quality Checks
    ensureImputationInputs(data = data,
                           imputationParameters = yieldImputationParameters)
    ensureImputationInputs(data = data,
                           imputationParameters = productionImputationParameters)
    ensureProductionInputs(data = data,
                           processingParameters = processingParameters)
    stopifnot(yieldImputationParameters$variable == "yield")
    stopifnot(productionImputationParameters$variable == "production")
    
    cat("Initializing ... \n")
    dataCopy = copy(data)
    setkeyv(x = dataCopy, cols = c(processingParameters$byKey,
                                   processingParameters$yearValue))
    processProductionDomain(data = dataCopy,
                            processingParameters = processingParameters)

    ## Step two: Impute Yield
    cat("Imputing Yield ...\n")
    n.missYield = sum(is.na(dataCopy[[processingParameters$yieldValue]]))
#     if(!missing(yieldFormula))
#         yieldFormula =
#             as.formula(gsub(yearValue, "yearValue",
#                             gsub(yieldValue, "yieldValue",
#                                  deparse(yieldFormula))))
    
    imputeVariable(data = dataCopy,
                   imputationParameters = yieldImputationParameters)
    n.missYield2 = length(which(is.na(
        dataCopy[[processingParameters$yieldValue]])))
    cat("Number of values imputed: ", n.missYield - n.missYield2, "\n")
    cat("Number of values still missing: ", n.missYield2, "\n")

    ## Balance production now using imputed yield
    balanceProduction(data = data, imputationParameters = imputationParameters,
                      processingParameters = processingParameters)

    ## step three: Impute production
    cat("Imputing Production ...\n")
    n.missProduction = length(which(is.na(
        dataCopy[[processingParameters$productionValue]])))

    imputeVariable(data = dataCopy,
                   imputationParameters = productionImputationParameters)

    n.missProduction2 = length(which(is.na(
        dataCopy[[processingParameters$productionValue]])))
    cat("Number of values imputed: ",
        n.missProduction - n.missProduction2, "\n")
    cat("Number of values still missing: ", n.missProduction2, "\n")

    ## step four: balance area harvested
    cat("Imputing Area Harvested ...\n")
    n.missAreaHarvested =
        length(which(is.na(
            dataCopy[[processingParameters$areaHarvestedValue]])))

    balanceAreaHarvested(data = dataCopy,
                         # imputation parameters just needed for flag table
                         imputationParameters = yieldImputationParameters,
                         processingParameters = processingParameters)

    n.missAreaHarvested2 =
        length(which(is.na(
            dataCopy[[processingParameters$areaHarvestedValue]])))
    cat("Number of values imputed: ",
        n.missAreaHarvested - n.missAreaHarvested2, "\n")
    cat("Number of values still missing: ", n.missAreaHarvested2, "\n")
    
    dataCopy
}