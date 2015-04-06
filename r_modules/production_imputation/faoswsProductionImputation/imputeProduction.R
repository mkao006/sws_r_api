##' Function to impute production
##'
##' This is a wrapper of the ensemble imputation for the production
##' domain.
##'
##' @param productionValue The column name corresponding to production
##' value.
##' @param productionObservationFlag The column name corresponding to the
##' observation flag of production.
##' @param areaHarvestedValue The column name corresponding to area
##' harvested value.
##' @param areaHarvestedObservationFlag The column name corresponding to the
##' observation flag of area harvested.
##' @param yieldValue The columne name corresponding to yield value.
##' @param yieldObservationFlag The column name corresponding to the observation
##' flag of yield.
##' @param imputationFlag Flag value for new imputation values.
##' @param flagTable see data(faoswsFlagTable) in \pkg{faoswsFlag}
##' @param data The data.table object containing the data.
##' @param byKey The unique key identifier.
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' 
##' @export
##' 

imputeProduction = function(productionValue, productionObservationFlag,
    productionMethodFlag, areaHarvestedValue,
    areaHarvestedObservationFlag, areaHarvestedMethodFlag, yieldValue,
    yieldObservationFlag, yieldMethodFlag, imputationFlag = "I",
    newMethodFlag, data,
    byKey, restrictWeights = TRUE, maximumWeights = 0.7,
    ensembleModel = list(defaultMean = defaultMean,
        defaultLm = defaultLm, defaultExp = defaultExp,
        defaultLogistic = defaultLogistic, defaultLoess = defaultLoess,
        defaultSpline = defaultSpline, defaultArima = defaultArima,
        defaultMars = defaultMars, defaultNaive = defaultNaive),
    flagTable = faoswsFlagTable){


    ## By balancing first
    balanceProduction(productionValue = productionValue,
                      productionObservationFlag =
                          productionObservationFlag,
                      productionMethodFlag =
                          productionMethodFlag,
                      areaHarvestedValue = areaHarvestedValue,
                      areaHarvestedObservationFlag =
                          areaHarvestedObservationFlag,
                      newMethodFlag = newMethodFlag,
                      yieldValue = yieldValue,
                      yieldObservationFlag = yieldObservationFlag,
                      data = data,
                      flagTable = flagTable)

    ## Then imputation by ensemble
    setnames(x = data,
             old = c(productionValue, productionObservationFlag,
                 productionMethodFlag),
             new = c("productionValue", "productionObservationFlag",
                 "productionMethodFlag"))

    productionMissingIndex = is.na(data[, productionValue])
    data[, productionValue :=
         ensembleImpute(productionValue,
                        ensembleModel = ensembleModel,
                        plot = FALSE),
         by = byKey]
    data[productionMissingIndex & !is.na(productionValue),
         c("productionObservationFlag", "productionMethodFlag") :=
         list(imputationFlag, newMethodFlag)]
    
    setnames(x = data,
             old = c("productionValue", "productionObservationFlag",
                 "productionMethodFlag"),
             new = c(productionValue, productionObservationFlag,
                 productionMethodFlag))
}
