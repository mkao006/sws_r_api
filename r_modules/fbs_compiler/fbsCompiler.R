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
## Make this a parameter in the module
selectedCountry = "840"
selectedYear = "2010"

## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",        
        token = "eff61a14-33cf-4126-a8ed-e455543ffff9"
        )
    R_SWS_SHARE_PATH = getwd()
    files = dir(pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
    files = files[files != "./fbsCompiler.R"]
    lapply(files, FUN = function(x) source(x))
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
}

primaryMeasuredItemCPC = getPrimaryMeasuredItemCPC(swsContext.datasets[[1]])

## NOTE (Michael): Need to check those cpc items which are not mapped
##                 in the commodity tree
commodityTree = getFBSHiearchy(level = 2)

nutrientData = getNutrientData()


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
    with(., merge(productionData, nutrientData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5510",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_5510") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5510",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))



## Trade Standardization
standardizedTrade = 
    {
        ## Obtain trade and nutrient data
        tradeData <<- getTradeData()
    } %>%
    ## Merge the trade and nutrient data
    with(., merge(tradeData, nutrientData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElementTrade_5600",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_5600") %>%
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElementTrade_5900",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_5900") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               c("Value_measuredElementCalorie_5600",
                                 "Value_measuredElementCalorie_5900"),
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))

## Compute trade standard deviation
standardizedTradeStandardDeviation =
    {
        tradeStandardDeviation <<- getTradeStandardDeviation()
        population <<- getPopulationData()
    } %>%
    with(., merge(tradeStandardDeviation, nutrientData, by = itemVar)) %>%
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
    with(., merge(seedData, nutrientData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5525",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_5525") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5525",
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
    with(., merge(lossData, nutrientData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5120",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_5120") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5120",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))


## Industrial Use Standardization
standardizedIndustrialUse = 
    {
        ## Obtain industrialUse and nutrient data
        industrialUseData <<- getIndustrialUseData()
    } %>%
    ## Merge the industrialUse and nutrient data
    with(., merge(industrialUseData, nutrientData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_5150",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_5150") %>%
    ## Perform calorie standardization
    calorieStandardization(data = .,
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5150",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))

## Food Standardization
##
## NOTE (Michael): There are missing entries in the database, there
##                 should be food for rice (S2805), but the data does
##                 not exist.
standardizedFood = getTotalFoodCalorie(unique(commodityTree$cpc_standardized_code))


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
    missingValueToZero(data = .)

## Compute the feed utilization based on availability and the requirement
tableWithFeed =
    copy(tableExcludeFeed) %>%
    ## Calculate feed availability
    calculateFeedAvailability(data = .,
                              production = "Value_measuredElementCalorie_5510",
                              import = "Value_measuredElementCalorie_5600",
                              export = "Value_measuredElementCalorie_5900",
                              seed = "Value_measuredElementCalorie_5525",
                              loss = "Value_measuredElementCalorie_5120",
                              industrialUse = "Value_measuredElementCalorie_5150",
                              food = "Value_measuredElementCalorie_FoodTotal") %>%
    {
        ## Get feed requirement and merge
        feedRequirement <<- getFeedRequirementData()
        merge(., feedRequirement,
              by = c("geographicAreaM49", "timePointYears"), all.x = TRUE)
    } %>%
    disaggregateFeedRequirement(data = .,
                                areaVar = areaVar,
                                yearVar = yearVar,
                                feedAvailabilityWeight = "feedAvailableWeights",
                                feedRequirementVar = "Value_estimator_1",
                                feedUtilizationVar =
                                    "Value_measuredElementCalorie_5520") %>%
    .[, `:=`(c("Value_measuredElementCalorie_feedAvail",
               "feedAvailableWeights",
               "nutrientType",
               "Value_estimator_1",
               "flagObservationStatus_estimator_1",
               "flagMethod_estimator_1",
               "feedBaseUnit"), NULL)]

## Create final contingency table
contingencyTable =
    copy(tableWithFeed) %>%
        calculateResidual(data = .,
                          production = "Value_measuredElementCalorie_5510",
                          import = "Value_measuredElementCalorie_5600",
                          export = "Value_measuredElementCalorie_5900",
                          seed = "Value_measuredElementCalorie_5525",
                          loss = "Value_measuredElementCalorie_5120",
                          industrialUse = "Value_measuredElementCalorie_5520",
                          food = "Value_measuredElementCalorie_FoodTotal",
                          feed = "Value_measuredElementCalorie_5520")

## Calculate contingency table in per capita per day
##
## NOTE (Michael): Need to check the element used in the getPopulationData
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
                      valueColumns = grep("Value_measuredElementCalorie",
                          colnames(.), value = TRUE)) %>%
    .[, Value_measuredElementPopulation_11 := NULL] %>%
    ## Add observation status and method flag
    addFBSFlags(data = .,
                valueColumns = grep("Value_measuredElementCalorie", colnames(.),
                    value = TRUE),
                valuePrefix = "Value",
                flagObsStatusPrefix = "flagObservationStatus",
                flagMethodPrefix = "flagMethod") %T>%
    saveContingencyCaputTable(data = .)



## checkTable =
##     contingencyTableCaput[, c("geographicAreaM49", "cpc_standardized_code",
##                               "timePointYears",
##                               grep("Value_measuredElementCalorie",
##                                    colnames(contingencyTable),
##                                    value = TRUE)),
##                           with = FALSE]
## setnames(checkTable,
##          old = grep("Value_measuredElementCalorie", colnames(contingencyTable),
##              value = TRUE),
##          new = c("production", "import", "export", "seed", "loss",
##              "industrialUse", "food", "feed", "stockChanges"))
## write.csv(checkTable,
##           file = "~/Desktop/contigency_table_example.csv",
##           row.names = FALSE, na = "")
