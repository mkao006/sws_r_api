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
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
}





## NOTE (Michael): Need a function to get primary commodities
getPrimaryMeasuredItemCPC = function(dataContext){

    itemTable =
        GetCodeList(domain = "trade",
                    dataset = "total_trade_CPC",
                    dimension = itemVar)

    ## This is a hack to get primary commodities
    ## itemTable[nchar(gsub("[^0-9]", "", code)) == 4 & nchar(code) == 4 &
    ##           !is.na(type), code]
    itemTable[!is.na(type), code]
}

primaryMeasuredItemCPC = getPrimaryMeasuredItemCPC(swsContext.datasets[[1]])



## Get trade data
getTradeData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    tradeKey = DatasetKey(
        domain = "trade",
        dataset = "total_trade_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementTrade",
                      keys = c("5600", "5900")),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    ##
    ## NOTE (Michael): Need to check this, 570 items are queried, but only
    ##                 105 are returned. Also, there are no value for
    ##                 element 5518 which caused the type to be logical.
    tradeQuery = GetData(
        key = tradeKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradePivot
    )


    ## Convert time to numeric
    tradeQuery[, timePointYears := as.numeric(timePointYears)]
    tradeQuery
}


## Get nutrient data
getNutrientData = function(){
    nutrientKey =
        DatasetKey(domain = "suafbs",
                   dataset = "nutrient_factors_cpc",
                   dimensions = list(
                       ## NOTE (Michael): We take energy in Kcal here
                       Dimension(name = "measuredElementNutritive",
                                 keys = "904"),
                       Dimension(name = "measuredItemCPC",
                                 keys = primaryMeasuredItemCPC),
                       Dimension(name = "timePointFake",
                                 keys = "1")
                   )
                   )

    nutrientPivot = c(
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointFake", ascending = TRUE),
        Pivoting(code = "measuredElementNutritive", ascending = TRUE)
    )

    nutrientQuery =
        GetData(key = nutrientKey,
                flags = FALSE,
                normalized = FALSE,
                pivoting = nutrientPivot)
    nutrientQuery[, timePointFake := NULL]
    nutrientQuery
}


computeCalorie = function(data, quantityVariable, calorieVariable,
    quantityToTonMultiplier, calorieToTonMultiplier, outputName){
    tmp = copy(data)
    tmp[, `:=`(c(outputName),
                tmp[[quantityVariable]] * quantityToTonMultiplier *
                tmp[[calorieVariable]] * calorieToTonMultiplier)]
    tmp
}

calorieStandardization = function(data, commodityTree, standardizeVariable,
                                  standardizationKey){
    commodityTreeCopy = copy(commodityTree)
    setnames(commodityTreeCopy, old = "cpc_children_code", new = itemVar)

    dataTree = merge(data, commodityTreeCopy, by = itemVar, all.x = TRUE)

    standardized =
        dataTree[, lapply(standardizeVariable, FUN = function(x) sum(.SD[[x]])),
                 by = c(standardizationKey)]

    setnames(standardized, old = paste0("V", 1:length(standardizeVariable)),
             new = standardizeVariable)
    standardized
}


    

## NOTE (Michael): In order to build the trade profile, we need
##                 to know how to calculate the "trade". Do we
##                 include processed?  If no, then need the set of
##                 primary commodities, and meat for livestock; if
##                 yes, then we should only mapped to all the
##                 commodities which are within the standardization
##                 path.


commodityTreePath = paste0(R_SWS_SHARE_PATH, "/cpcCommodityTreeReconstructed.csv")
cpcCommodityTree.dt =
    data.table(read.csv(file = commodityTreePath,
                        colClass = rep("character", 7)))


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



## Pseudo codes:
##

## For primary:
## ---------------------------------------------------------------------
## (1) Obtain energy, protein, lipid, CHOLAVLDF and Sugar from meHS
## and sdHS. What is the difference between the two dataset. Also,
## what is the difference between the nutrients data used for trade
## and production. 

## (2) Obtain the edible, extraction rate and commodity from file from
## HSm.

## (3) Merge the consolidated trade data with the nutrient from step (1) and (2).

## (4) Calculate the energy, protein, fat, and CHOAVLDF for the trade.

## (5) Calculate the residual for feed, it is defined the same way as
## production. trade multiplied by (1 - extraction rate) then the
## nutrient conversion ratio. Probably should move this to domestic
## supply.

## (6) Aggregate the trade (both import and export), and also the
## columns calculated in step (5) and (6) by area, year, group and
## commodity. Again, need to understand the difference between the use
## of group and commodity.

## Domestic supply:
## (7) Add production and trade energy to obtain the domestic energy supply.

## For Processed:
## ---------------------------------------------------------------------

## (1) Load the D2D3s data set, what is the source of this data?





## Run production compiler to obtain the energy supply of each
## commodity group.


## Get all the HS code using GetCodeTree, then merge with Adam's
## file. Any HS code not mapped will go into HS to commodity group.

## Merge the energy supply by commodity with Adam's mapping, then
## determine the relative ratio. This ratio does not imply that the
## quantity factually came from these group and require sufficient
## availability, rather they suggests how the processed product was
## constructed.

## However, it is possible that the standardized export can be greater
## than the domestic supply. This needs to be checked.

## Build the relative production profile ratio for each parent
## commodity group for each country and year.

## Merge the import of the mirrored trade flow data and the exporter
## production profile, then standardize to obtain the standardized
## import.

## Merge the export of the mirrored trade flow data and the domestic
## production profile, then standardize to obtain the standardized
## export.


## All I need at the end is the trade in commodity group in per capita
