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


## Function to get all the item required by the Food Balance Sheet
getFBSmeasuredItemCPC = function(dataContext){
    ## itemTable =
    ##     GetCodeList(domain = slot(dataContext, "domain"),
    ##                 dataset = slot(dataContext, "dataset"),
    ##                 dimension = itemVar)
    ## HACK (Michael): Since we don't have the columne 'type' ready
    ##                 for selection, we will select all item which
    ##                 are under the CPC heading '0'.
    require(igraph)
    require(faoswsUtil)
    itemEdgeList =
        adjacent2edge(
            GetCodeTree(domain = slot(dataContext, "domain"),
                        dataset = slot(dataContext, "dataset"),
                        dimension = itemVar)
        )

    itemEdgeGraph = graph.data.frame(itemEdgeList)
    itemDist = shortest.paths(itemEdgeGraph, v = "0", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
    fbsItemCodes
}




## Get loss utilization data
getLossData = function(){
    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    lossKey = DatasetKey(
        domain = "lossWaste",
        dataset = "loss",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementSuaFbs",
                      keys = "5120"),
            Dimension(name = "measuredItemSuaFbs",
                      keys = FBSmeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    lossPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementSuaFbs", ascending = TRUE)
    )

    ## Query the data
    ##
    ## NOTE (Michael): Need to check this, 570 items are queried, but only
    ##                 105 are returned. Also, there are no value for
    ##                 element 5518 which caused the type to be logical.
    lossQuery = GetData(
        key = lossKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = lossPivot
    )


    ## Convert time to numeric
    lossQuery[, timePointYears := as.numeric(timePointYears)]
    lossQuery
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
                                 keys = FBSmeasuredItemCPC),
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


## Function to compute the calorie
computeCalorie = function(data, quantityVariable, calorieVariable,
    quantityToTonMultiplier, calorieToTonMultiplier, outputName){
    tmp = copy(data)
    tmp[, `:=`(c(outputName),
                tmp[[quantityVariable]] * quantityToTonMultiplier *
                tmp[[calorieVariable]] * calorieToTonMultiplier)]
    tmp
}

## Function to standardize calories
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


commodityTreePath = paste0(R_SWS_SHARE_PATH, "/cpcCommodityTreeReconstructed.csv")
cpcCommodityTree.dt =
    data.table(read.csv(file = commodityTreePath,
                        colClass = rep("character", 7)))

## Full loss compiler
standardizedLoss = 
    {
        FBSmeasuredItemCPC <<- getFBSmeasuredItemCPC(swsContext.datasets[[1]])
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
                       quantityVariable = "Value_measuredElement_5525",
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

## Pseudo codes:

## Assuming the loss rates are calculated, maybe we should take the
## loss directly.

## (1) Obtain the loss rate data, then compute the country/group
## average loss rate.

## (2) Use the countr/group average loss rate to impute commodities
## which are missing.

## (3) Mean aggregate the values to obtain the commodity level loss rate.

## (4) Multiply the loss rate to production and nutrient requirements
## to obtain the the nutrients of loss.



## The loss data should have been performed by individual module as
## well. Just need to multiplied by the nutrient and aggregate.


