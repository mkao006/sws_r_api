##' This function imputes the whole production domain.
##'
##' The function will impute production, area harvested and yield at
##' the same time.
##'
##' Transformation in the yield formula is not allowed and will not be
##' taken into account.
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
##' @param yearValue The column name corresponding to year
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param removePriorImputation logical, whether prior imputation
##' should be removed.
##' @param removeConflictValues logical, whether conflict area
##' harvested value and production should be removed.
##' @param imputedFlag Flag value corresponding to values from prior
##' imputation, ignored if removePriorImputation is FALSE.
##' @param imputationFlag Flag value for new imputation values.
##' @param naFlag Flag value for missing values.
##' @param maxdf The maximum degree of freedom for the spline. 
##' @param byKey The unique key identifier.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param yieldFormula The formula to be passed to the linear mixed
##' model for the imputation of yield, if missing default spline model
##' is fitted.
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##'
##' @export
##' 

imputeProductionDomain = function(data, productionValue,
    areaHarvestedValue, yieldValue, yearValue, productionObservationFlag,
    areaHarvestedObservationFlag,
    yieldObservationFlag, productionMethodFlag, areaHarvestedMethodFlag,
    yieldMethodFlag, flagTable = faoswsFlagTable,
    removePriorImputation = TRUE, removeConflictValues = TRUE,
    imputedFlag = "E", imputationFlag = "I", newMethodFlag = "",
    naFlag = "M", maxdf = 5, 
    byKey = "areaCode", restrictWeights = TRUE, maximumWeights = 0.7,
    ensembleModel = list(defaultMean = defaultMean,
        defaultLm = defaultLm, defaultExp = defaultExp,
        defaultLogistic = defaultLogistic, defaultLoess = defaultLoess,
        defaultSpline = defaultSpline, defaultArima = defaultArima,
        defaultMars = defaultMars, defaultNaive = defaultNaive),
        yieldFormula){

    cat("Initializing ... \n")
    dataCopy = copy(data)
    setkeyv(x = dataCopy, cols = c(byKey, yearValue))
    setnames(x = dataCopy,
             old = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     yearValue,
                     productionObservationFlag,
                     areaHarvestedObservationFlag,
                     yieldObservationFlag,
                     productionMethodFlag,
                     areaHarvestedMethodFlag,
                     yieldMethodFlag
                     ),
             new = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "yearValue",
                     "productionObservationFlag",
                     "areaHarvestedObservationFlag",
                     "yieldObservationFlag",
                     "productionMethodFlag",
                     "areaHarvestedMethodFlag",
                     "yieldMethodFlag")
             )

    ## These should be documented and checked.  Make sure the types
    ## are correct
    dataCopy[, productionValue := as.numeric(productionValue)]
    dataCopy[, areaHarvestedValue := as.numeric(areaHarvestedValue)]
    dataCopy[, yieldValue := as.numeric(yieldValue)]

    dataCopy =
        processProductionDomain(data = dataCopy,
                                productionValue = "productionValue",
                                productionObservationFlag =
                                    "productionObservationFlag",
                                productionMethodFlag =
                                    "productionMethodFlag",
                                areaHarvestedValue =
                                    "areaHarvestedValue",
                                areaHarvestedObservationFlag =
                                    "areaHarvestedObservationFlag",
                                yieldValue = "yieldValue",
                                yieldObservationFlag =
                                    "yieldObservationFlag",
                                removePriorImputation =
                                    removePriorImputation,
                                removeConflictValues =
                                    removeConflictValues,
                                imputedFlag = imputedFlag,
                                naFlag = naFlag,
                                byKey = byKey)    

    ## Step two: Impute Yield
    cat("Imputing Yield ...\n")
    n.missYield = length(which(is.na(dataCopy$yieldValue)))
    if(!missing(yieldFormula))
        yieldFormula =
            as.formula(gsub(yearValue, "yearValue",
                            gsub(yieldValue, "yieldValue",
                                 deparse(yieldFormula))))
    
    imputeYield(yieldValue = "yieldValue",
                yieldObservationFlag = "yieldObservationFlag",
                yieldMethodFlag = "yieldMethodFlag",
                yearValue = "yearValue",
                imputationFlag = imputationFlag,
                newMethodFlag = newMethodFlag,
                maxdf = maxdf,
                byKey = byKey,
                data = dataCopy,
                yieldFormula = yieldFormula)
    n.missYield2 = length(which(is.na(dataCopy$yieldValue)))
    cat("Number of values imputed: ", n.missYield - n.missYield2, "\n")
    cat("Number of values still missing: ", n.missYield2, "\n")

    ## step three: Impute production
    cat("Imputing Production ...\n")
    n.missProduction = length(which(is.na(dataCopy$productionValue)))

    imputeProduction(productionValue = "productionValue",
                     productionObservationFlag =
                         "productionObservationFlag",
                     productionMethodFlag =
                         "productionMethodFlag",
                     areaHarvestedValue = "areaHarvestedValue",
                     areaHarvestedObservationFlag =
                         "areaHarvestedObservationFlag",
                     yieldValue = "yieldValue",
                     yieldObservationFlag = "yieldObservationFlag",
                     newMethodFlag = newMethodFlag,
                     data = dataCopy,
                     ensembleModel = ensembleModel,
                     restrictWeights = restrictWeights,
                     maximumWeights = maximumWeights,
                     byKey = byKey,
                     flagTable = flagTable)

    n.missProduction2 = length(which(is.na(dataCopy$productionValue)))
    cat("Number of values imputed: ",
        n.missProduction - n.missProduction2, "\n")
    cat("Number of values still missing: ", n.missProduction2, "\n")

    ## step four: balance area harvested
    cat("Imputing Area Harvested ...\n")
    n.missAreaHarvested =
        length(which(is.na(dataCopy$areaHarvestedValue)))

    balanceAreaHarvested(productionValue = "productionValue",
                         productionObservationFlag =
                             "productionObservationFlag",
                         areaHarvestedValue = "areaHarvestedValue",
                         areaHarvestedObservationFlag =
                             "areaHarvestedObservationFlag",
                         areaHarvestedMethodFlag =
                             "areaHarvestedMethodFlag",
                         yieldValue = "yieldValue",
                         yieldObservationFlag = "yieldObservationFlag",
                         newMethodFlag = newMethodFlag,
                         data = dataCopy,
                         flagTable = flagTable)

    n.missAreaHarvested2 =
        length(which(is.na(dataCopy$areaHarvestedValue)))
    cat("Number of values imputed: ",
        n.missAreaHarvested - n.missAreaHarvested2, "\n")
    cat("Number of values still missing: ", n.missAreaHarvested2, "\n")
    
    ## Reset the names
    setnames(x = dataCopy,
             old = c("productionValue",
                     "areaHarvestedValue",
                     "yieldValue",
                     "yearValue",
                     "productionObservationFlag",
                     "areaHarvestedObservationFlag",
                     "yieldObservationFlag",
                     "productionMethodFlag",
                     "areaHarvestedMethodFlag",
                     "yieldMethodFlag"),
             new = c(productionValue,
                     areaHarvestedValue,
                     yieldValue,
                     yearValue,
                     productionObservationFlag,
                     areaHarvestedObservationFlag,
                     yieldObservationFlag,
                     productionMethodFlag,
                     areaHarvestedMethodFlag,
                     yieldMethodFlag)             
             )

    dataCopy
}
