## load the library
suppressWarnings({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(faoswsProductionImputation)
    library(data.table)
    library(magrittr)
    library(igraph)
})


## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
standardizedItemVar = "cpc_standardized_code"
elementVar = "measuredElement"


## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",        
        token = "eff61a14-33cf-4126-a8ed-e455543ffff9"
        )
    R_SWS_SHARE_PATH = getwd()
    files = dir(path = "fbsCompiler/", full.names = TRUE)
    lapply(files, FUN = function(x) source(x))
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
}

## Obtain the year and country parameters
selectedCountry = swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
selectedYear = swsContext.datasets[[1]]@dimensions$timePointYears@keys

## Get all the primary measured item in CPC
primaryMeasuredItemCPC = getPrimaryMeasuredItemCPC(swsContext.datasets[[1]])

## NOTE (Michael): Need to check those cpc items which are not mapped
##                 in the commodity tree
commodityTree = getCPCHiearchy(level = 2)

## Obtain the calorie conversion factors
calorieConversionData = getCalorieConversionFactor()


## Compute standardization for each element
## ---------------------------------------------------------------------

## Production Standardization
standardizedProduction = 
    {
        ## Obtain production and nutrient data
        productionElements <<- getProductionElement(primaryMeasuredItemCPC)        
        productionData <<- getProductionData()
    } %>%
    ## Merge the production and nutrient data
    with(., merge(productionData, calorieConversionData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5510",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElement_250") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElement_250",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))



## Trade Standardization
standardizedTrade = 
    {
        ## Obtain trade and nutrient data
        tradeData <<- getTradeData()
    } %>%
    ## Merge the trade and nutrient data
    with(., merge(tradeData, calorieConversionData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElementTrade_5600",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElement_251") %>%
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElementTrade_5900",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElement_252") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               c("Value_measuredElement_251",
                                 "Value_measuredElement_252"),
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))

## Compute trade standard deviation
standardizedTradeStandardDeviation =
    {
        tradeStandardDeviation <<- getTradeStandardDeviation()
        population <<- getPopulationData()
    } %>%
    with(., merge(tradeStandardDeviation, calorieConversionData, by = itemVar)) %>%
    ## HACK (Michael): This item is not in the CPC tree, so we omit it
    ##                 for now.
    .[measuredItemCPC != "23162", ] %>%
    standardizeTradeStd(data = .,
                        commodityTree = commodityTree,
                        weightVariable =
                            "Value_measuredElementNutritive_904",
                        tradeStandardDeviationVariable =
                            c("Value_measuredElementTrade_SD5900",
                              "Value_measuredElementTrade_SD5600"),
                        standardizationKey =
                            c(areaVar, yearVar, "cpc_standardized_code")) %>%
    setnames(., old = "cpc_standardized_code", "measuredItemSuaFbs") %>%
    ## Set Estimation flag
    .[, `:=`(paste0(rep(c("flagObservationStatus", "flagMethod"), each = 2),
                    rep(c("_measuredElementTrade_SD5600",
                          "_measuredElementTrade_SD5900"), times = 2)),
             list("E", "E", "e", "e"))] %>%
    ## Calculate the per caput standard deviation
    merge(., population, by = c("geographicAreaM49", "timePointYears")) %>%
    calculatePerCaput(data = .,
                      populationVar = "Value_measuredElementPopulation_11",
                      valueColumns = c("Value_measuredElementTrade_SD5900",
                              "Value_measuredElementTrade_SD5600")) %>%
    .[, `:=`(c("Value_measuredElementPopulation_11",
               "flagPopulation_measuredElementPopulation_11"),
             NULL)] %>%
    ## Setting the order, if the order is incorrect, you can't save the data back.
    setcolorder(., c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs",
                     "Value_measuredElementTrade_SD5600",
                     "flagObservationStatus_measuredElementTrade_SD5600",
                     "flagMethod_measuredElementTrade_SD5600",
                     "Value_measuredElementTrade_SD5900",
                     "flagObservationStatus_measuredElementTrade_SD5900",
                     "flagMethod_measuredElementTrade_SD5900")) %>%
    saveTradeStandardDeviation(data = .)

                                          


## Seed Standardization
standardizedSeed = 
    {
        ## Obtain seed and nutrient data
        seedData <<- getSeedData()
    } %>%
    ## Merge the seed and nutrient data
    with(., merge(seedData, calorieConversionData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5525",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElement_55252") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElement_55252",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))


## Loss Standardization
##
standardizedLoss = 
    {
        ## Obtain loss and nutrient data
        lossData <<- getLossData()
    } %>%
    ## Merge the loss and nutrient data
    with(., merge(lossData, calorieConversionData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5120",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElement_51202") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElement_51202",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))


## Industrial Use Standardization
standardizedIndustrialUse = 
    {
        ## Obtain industrialUse and nutrient data
        industrialUseData <<- getIndustrialUseData()
    } %>%
    ## Merge the industrialUse and nutrient data
    with(., merge(industrialUseData, calorieConversionData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5150",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElement_51502") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElement_51502",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))

## Food Standardization
##
## NOTE (Michael): There are missing entries in the database, there
##                 should be food for rice (S2805), but the data does
##                 not exist.
##
## NOTE (Michael): There is no need to standardize food to calorie as
## it is already in calorie.
standardizedFood = getTotalFoodCalorie(unique(commodityTree$cpc_standardized_code))
setnames(standardizedFood,
         old = "Value_measuredElementCalorie_FoodTotal",
         new = "Value_measuredElement_51422")

## Build Contingency table
## ---------------------------------------------------------------------

## Merge all the standardized elements together to produce the
## contingency table without feed.
tableExcludeFeed =
    mergeAllData(standardizedProduction,
                 standardizedTrade,
                 standardizedSeed,
                 standardizedLoss,
                 standardizedIndustrialUse,
                 standardizedFood) %>%
    ## HACK (Michael): There are items that are not mapped in the
    ##                 current tree, we discard them for now.
    .[!is.na(cpc_standardized_code), ] %>%
    ## We assume missing values are zero at the Food Balance Sheet level
    missingValueToZero(data = .)

## Compute the feed utilization based on availability and the requirement
tableWithFeed =
    copy(tableExcludeFeed) %>%
    ## Calculate feed availability
    calculateFeedAvailability(data = .,
                              areaVar = areaVar,
                              yearVar = yearVar,
                              production = "Value_measuredElement_250",
                              import = "Value_measuredElement_251",
                              export = "Value_measuredElement_252",
                              seed = "Value_measuredElement_55252",
                              loss = "Value_measuredElement_51202",
                              industrialUse = "Value_measuredElement_51502",
                              food = "Value_measuredElement_51422") %>%
    {
        ## Get feed requirement and merge
        feedRequirement <<- getFeedRequirementData()
        merge(., feedRequirement,
              by = c("geographicAreaM49", "timePointYears"), all.x = TRUE)
    } %>%
    ## Disaggregate feed based on energy requirement
    disaggregateFeedRequirement(data = .,
                                areaVar = areaVar,
                                yearVar = yearVar,
                                feedAvailabilityWeight = "feedAvailableWeights",
                                feedRequirementVar = "Value_estimator_1",
                                feedUtilizationVar =
                                    "Value_measuredElement_55202") %>%
    .[, `:=`(c("Value_measuredElementCalorie_feedAvail",
               "feedAvailableWeights",
               "nutrientType",
               "Value_estimator_1",
               "flagObservationStatus_estimator_1",
               "flagMethod_estimator_1",
               "feedBaseUnit"), NULL)]

## Create final contingency table by calculating the residual
contingencyTable =
    copy(tableWithFeed) %>%
        calculateResidual(data = .,
                          production = "Value_measuredElement_250",
                          import = "Value_measuredElement_251",
                          export = "Value_measuredElement_252",
                          seed = "Value_measuredElement_55252",
                          loss = "Value_measuredElement_51202",
                          industrialUse = "Value_measuredElement_51502",
                          food = "Value_measuredElement_51422",
                          feed = "Value_measuredElement_55202",
                          residualVariable = "Value_measuredElement_50712")

## Calculate contingency table in per capita per day
contingencyTableCaput =
    copy(contingencyTable) %>%
    {
        population <<- getPopulationData()
        merge(contingencyTable,
              population[, list(geographicAreaM49, timePointYears,
                             Value_measuredElementPopulation_11)],
              by = c("geographicAreaM49", "timePointYears"), all.x = TRUE)
    } %>%
    calculatePerCaput(data = .,
                      populationVar = "Value_measuredElementPopulation_11",
                      valueColumns = grep("Value_measuredElement",
                          colnames(.), value = TRUE)) %>%
    .[, Value_measuredElementPopulation_11 := NULL] %>%
    ## Add observation status and method flag
    addFBSFlags(data = .,
                valueColumns = grep("Value_measuredElement", colnames(.),
                    value = TRUE),
                valuePrefix = "Value",
                flagObsStatusPrefix = "flagObservationStatus",
                flagMethodPrefix = "flagMethod") %>%
    setnames(. ,old = "cpc_standardized_code", new = "measuredItemSuaFbs") %>%
    saveContingencyCaputTable(data = .)



## checkTable =
##     contingencyTableCaput[, c("geographicAreaM49", "measuredItemSuaFbs",
##                               "timePointYears",
##                               grep("Value_measuredElement",
##                                    colnames(contingencyTable),
##                                    value = TRUE)),
##                           with = FALSE]
## setnames(checkTable,
##          old = grep("Value_measuredElement", colnames(contingencyTable),
##              value = TRUE),
##          new = c("production", "import", "export", "seed", "loss",
##              "industrialUse", "food", "feed", "stockChanges"))
## write.csv(checkTable,
##           file = "~/Desktop/contigency_table_example.csv",
##           row.names = FALSE, na = "")
