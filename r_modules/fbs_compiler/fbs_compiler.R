## load the library
suppressWarnings({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(faoswsProductionImputation)
    library(data.table)
    library(magrittr)
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
    files = files[files != "./fbs_compiler.R"]
    lapply(files, FUN = function(x) source(x))

} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
}

primaryMeasuredItemCPC = getPrimaryMeasuredItemCPC(swsContext.datasets[[1]])


commodityTreePath = paste0(R_SWS_SHARE_PATH, "/cpcCommodityTreeReconstructed.csv")
cpcCommodityTree.dt =
    data.table(read.csv(file = commodityTreePath,
                        colClass = rep("character", 7)))

nutrientData = getNutrientData()

## Full production compiler
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
    ##
    ## NOTE (Michael): Need to check those cpc items which are not mapped
    ##                 in the commodity tree
    calorieStandardization(data = .,
                           commodityTree = cpcCommodityTree.dt,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5510",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))



## Full trade compiler
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
    ##
    ## NOTE (Michael): Need to check those cpc items which are not mapped
    ##                 in the commodity tree
    calorieStandardization(data = .,
                           commodityTree = cpcCommodityTree.dt,
                           standardizeVariable =
                               c("Value_measuredElementCalorie_5600",
                                 "Value_measuredElementCalorie_5900"),
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))

## NOTE (Michael): Need the trade standard deviation function
##                 defined.
##
## standardizedTradeStandardDeviation =
##     {
##         tradeStandardDeviation <<- getTradeStandardDeviation()
##     } %>%
##         merge(., nutrientData, by = itemVar) %>%
##         standardizeTradeStd(data = .,
##                             weightVariable =
##                                 "Value_measuredElementNutritive_904",
##                             tradeStandardDeviationVariable =
##                                 c("Value_measuredElementCalorie_5600",
##                                  "Value_measuredElementCalorie_5900"))
                                          


## Full seed compiler
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
        ##
        ## NOTE (Michael): Need to check those cpc items which are not mapped
        ##                 in the commodity tree
        calorieStandardization(data = .,
                               commodityTree = cpcCommodityTree.dt,
                               standardizeVariable =
                                   "Value_measuredElementCalorie_5525",
                               standardizationKey =
                                   c(areaVar, yearVar, "cpc_standardized_code"))




## Full loss compiler
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
        ##
        ## NOTE (Michael): Need to check those cpc items which are not mapped
        ##                 in the commodity tree
        calorieStandardization(data = .,
                               commodityTree = cpcCommodityTree.dt,
                               standardizeVariable =
                                   "Value_measuredElementCalorie_5120",
                               standardizationKey =
                                   c(areaVar, yearVar, "cpc_standardized_code"))


## Full industrialUse compiler
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
    ##
    ## NOTE (Michael): Need to check those cpc items which are not mapped
    ##                 in the commodity tree
    calorieStandardization(data = .,
                           commodityTree = cpcCommodityTree.dt,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5150",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))



## Full feed compiler
standardizedFeed = 
    {
        ## Obtain feed availability and requirement data
        feedAvailability <<- getFeedAvailabilityData()
        feedRequirement <<- getFeedRequirementData()
    } %>%
    ## Merge the feed availability and nutrient data
    with(., merge(feedAvailability, nutrientData, by = itemVar)) %>%
    ## Compute the calorie
    computeCalorie(data = .,
                   quantityVariable = "Value_measuredElement_feedAvail",
                   calorieVariable = "Value_measuredElementNutritive_904",
                   quantityToTonMultiplier = 1000,
                   calorieToTonMultiplier = 10,
                   outputName = "Value_measuredElementCalorie_feedAvail") %>%
    ## Perform calorie standardization
    ##
    calorieStandardization(data = .,
                           commodityTree = cpcCommodityTree.dt,
                           standardizeVariable =
                               "Value_measuredElementCalorie_feedAvail",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code")) %>%
    ## Merge availability with requirements
    merge(., feedRequirement,
          by = c("geographicAreaM49", "timePointYears"),
          all = TRUE) %>%
    ## Use feed availability to disaggregate feed requirements to
    ## standardized commotities.
    disaggregateFeedRequirement(data = .,
                                areaVar = areaVar,
                                yearVar = yearVar,
                                feedAvailabilityVar =
                                    "Value_measuredElementCalorie_feedAvail",
                                feedRequirementPointVar =
                                    "Value_estimator_1",
                                feedUtilizationVar =
                                    "Value_measuredElementCalorie_5520") %>%
    subset(x = ., select = c(areaVar, standardizedItemVar, yearVar,
                      "Value_measuredElementCalorie_5520"))



## Full food compiler

## Merge all standardized data
##
## NOTE (Michael): Need to add in standardized food when finished
##
## NOTE (Michael): Need to convert to per capita per day, the same
##                 formula can also be applied for the standard
##                 deviation, since there is no summation. It's a
##                 simple linear transformation.
allStandardizedData =
    mergeAllData(standardizedProduction, standardizedTrade, standardizedSeed,
                 standardizedFeed, standardizedLoss, standardizedIndustrialUse)

## NOTE (Michael): Compile the standardized trade calorie standard
##                 deviation and save back to a new dataset.
