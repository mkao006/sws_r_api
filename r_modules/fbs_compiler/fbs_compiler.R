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


## NOTE (Michael): Need a function to get primary commodities
getPrimaryMeasuredItemCPC = function(dataContext){

    itemTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = itemVar)

    ## This is a hack to get primary commodities
    ## itemTable[nchar(gsub("[^0-9]", "", code)) == 4 & nchar(code) == 4 &
    ##           !is.na(type), code]
    itemTable[!is.na(type), code]
}

primaryMeasuredItemCPC = getPrimaryMeasuredItemCPC(swsContext.datasets[[1]])


commodityTreePath = paste0(R_SWS_SHARE_PATH, "/cpcCommodityTreeReconstructed.csv")
cpcCommodityTree.dt =
    data.table(read.csv(file = commodityTreePath,
                        colClass = rep("character", 7)))



## Full production compiler
standardizedProduction = 
    {
        ## Obtain production and nutrient data
        productionElements <<- getProductionElement(primaryMeasuredItemCPC)        
        productionData <<- getProductionData()
        nutrientData <<- getNutrientData()
        list(productionData = productionData,
             nutrientData = nutrientData)
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
        nutrientData <<- getNutrientData()
        list(tradeData = tradeData,
             nutrientData = nutrientData)
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


## Full seed compiler
standardizedSeed = 
    {
        ## Obtain seed and nutrient data
        seedData <<- getSeedData()
        nutrientData <<- getNutrientData()
        list(seedData = seedData,
             nutrientData = nutrientData)
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
## NOTE (Michael): This doesn't work, because the context is wrong
standardizedLoss = 
    {
        ## Obtain loss and nutrient data
        lossData <<- getLossData()
        nutrientData <<- getNutrientData()
        list(lossData = lossData,
             nutrientData = nutrientData)
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
        nutrientData <<- getNutrientData()
        list(industrialUseData = industrialUseData,
             nutrientData = nutrientData)
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


