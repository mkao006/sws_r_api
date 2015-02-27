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




## Get seed utilization data
getSeedData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = elementVar,
                      keys = "5525"),
            Dimension(name = itemVar,
                      keys = FBSmeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    seedPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    ##
    ## NOTE (Michael): Need to check this, 570 items are queried, but only
    ##                 105 are returned. Also, there are no value for
    ##                 element 5518 which caused the type to be logical.
    seedQuery = GetData(
        key = seedKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = seedPivot
    )


    ## Convert time to numeric
    seedQuery[, timePointYears := as.numeric(timePointYears)]
    seedQuery
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

## Full seed compiler
standardizedSeed = 
    {
        FBSmeasuredItemCPC <<- getFBSmeasuredItemCPC(swsContext.datasets[[1]])
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
