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
    files = files[files != "./fbs_compiler.R"]
    lapply(files, FUN = function(x) source(x))

} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
}

primaryMeasuredItemCPC = getPrimaryMeasuredItemCPC(swsContext.datasets[[1]])

commodityTree = getFBSHiearchy()

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
                           commodityTree = commodityTree,
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
                           commodityTree = commodityTree,
                           standardizeVariable =
                               c("Value_measuredElementCalorie_5600",
                                 "Value_measuredElementCalorie_5900"),
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))


## NOTE (Michael): After the trade std is converted to CPC, then we
##                 convert to calorie.
standardizedTradeStandardDeviation =
    {
        tradeStandardDeviation <<- getTradeStandardDeviation()
        ## tradeStandardDeviation <<-
        ##     data.table(read.csv("trade_standard_deviation_quantity_example.csv",
        ##                         colClass = c(rep("character", 3),
        ##                             rep("numeric", 2))))
    } %>%
        merge(., nutrientData, by = itemVar) %>%
        standardizeTradeStd(data = .,
                            commodityTree = commodityTree,
                            weightVariable =
                                "Value_measuredElementNutritive_904",
                            tradeStandardDeviationVariable =
                                "standard_deviation",
                            standardizationKey =
                               c(areaVar, yearVar, "measuredElementTrade",
                                 "cpc_standardized_code"))
                                          


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
                               commodityTree = commodityTree,
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
                               commodityTree = commodityTree,
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
                           commodityTree = commodityTree,
                           standardizeVariable =
                               "Value_measuredElementCalorie_5150",
                           standardizationKey =
                               c(areaVar, yearVar, "cpc_standardized_code"))

## Full industrialUse compiler
##
## NOTE (Michael): There are missing entries in the database, there
##                 should be food for rice (S2805), but the data does
##                 not exist.
food = getTotalFoodCalorie(firstLevelFBS)





tableExcludeFeed =
    mergeAllData(standardizedProduction, standardizedTrade, standardizedSeed,
                 standardizedLoss, standardizedIndustrialUse, food)




## HACK (Michael): There are items that are not mapped in the current
##                 tree, we discard them for now.
tableExcludeMiss =
    tableExcludeFeed[!is.na(cpc_standardized_code), ]

valueColumns = grep("Value", colnames(tableExcludeMiss), value = TRUE)
tableExcludeMiss =
    tableExcludeMiss[, `:=`(c(valueColumns),
                                lapply(valueColumns,
                                       FUN = function(x){
                                           tmp = .SD[[x]]
                                           tmp[is.na(tmp)] = 0
                                           tmp
                                       }))]

## Calculate Feed Availability as residual
tableExcludeMiss[, Value_measuredElement_feedAvail :=
                         Value_measuredElementCalorie_5510 +
                         Value_measuredElementCalorie_5600 -
                         Value_measuredElementCalorie_5900 -
                         Value_measuredElementCalorie_5525 -
                         Value_measuredElementCalorie_5120 -
                         Value_measuredElementCalorie_5150 -
                         Value_measuredElementCalorie_FoodTotal]

tableExcludeMiss[Value_measuredElement_feedAvail < 0,
                     Value_measuredElement_feedAvail := 0]

feedRequirement =  getFeedRequirementData()

standardizedFinal =
    merge(tableExcludeMiss, feedRequirement,
          by = c("geographicAreaM49", "timePointYears"), all.x = TRUE)

standardizedFinal[, totalAvailabilityWeight :=
                      Value_measuredElement_feedAvail/
                          sum(Value_measuredElement_feedAvail),
                  by = c("geographicAreaM49", "timePointYears")] 


standardizedFinal[, Value_measuredElementCalorie_5520:= Value_estimator_1 *
                      totalAvailabilityWeight]

standardizedFinal[, Value_measuredElement_stockChanges :=
                         Value_measuredElementCalorie_5510 +
                         Value_measuredElementCalorie_5600 -
                         Value_measuredElementCalorie_5900 -
                         Value_measuredElementCalorie_5525 -
                         Value_measuredElementCalorie_5120 -
                         Value_measuredElementCalorie_5150 -
                         Value_measuredElementCalorie_5520 - 
                         Value_measuredElementCalorie_FoodTotal]

## Everything should be in per capita

write.csv(standardizedFinal[, c("geographicAreaM49", "cpc_standardized_code",
                                "timePointYears",
                                grep("Value_measuredElementCalorie",
                                     colnames(standardizedFinal), value = TRUE)),
                            with = FALSE],
          file = "~/Desktop/contigency_table_example.csv",
          row.names = FALSE, na = "")
