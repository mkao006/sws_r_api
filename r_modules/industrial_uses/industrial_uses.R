## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(magrittr)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "induseElement"
## Make this a parameter in the module
selectedYear = "2010"


## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "eff61a14-33cf-4126-a8ed-e455543ffff9"
        )
}

getCPCTreeItem = function(dataContext){
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
            GetCodeTree(domain = "agriculture",
                        dataset = "agriculture",
                        dimension = itemVar)
        )

    itemEdgeGraph = graph.data.frame(itemEdgeList)
    itemDist = shortest.paths(itemEdgeGraph, v = "CPC", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist) &
                                          colnames(itemDist) != "CPC"]
    fbsItemCodes
}


getProductionElement = function(measuredItemCPC){
    condition =
        paste0("WHERE cpc_code IN (",
               paste0(shQuote(as.character(measuredItemCPC)),
                      collapse = ", "), ")")
    yieldFormula =
        GetTableData(schemaName = "ess",
                     tableName = "item_yield_elements",
                     whereClause = condition)
    yieldFormula
}



## Get production data

getProductionData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    productionKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys),
            Dimension(name = elementVar,
                      keys = unique(productionElements$element_51)),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    productionPivot = c(
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
    productionQuery = GetData(
        key = productionKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = productionPivot
    )


    ## Convert time to numeric
    productionQuery[, timePointYears := as.numeric(timePointYears)]
    productionQuery
}



## Function to get bio-fuel utilization
getBioFuelData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    bioFuelKey = DatasetKey(
        domain = "industrialUse",
        dataset = "biofuel",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys),
            Dimension(name = elementVar,
                      keys = "5150"),
            Dimension(name = itemVar,
                      keys = allCPCItem),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    bioFuelPivot = c(
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
    bioFuelQuery = GetData(
        key = bioFuelKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = bioFuelPivot
    )
    setnames(bioFuelQuery,
             old = grep("induseElement", colnames(bioFuelQuery), value = TRUE),
             new = gsub("induseElement", "measuredElement",
                        grep("induseElement", colnames(bioFuelQuery),
                             value = TRUE)))


    ## Convert time to numeric
    bioFuelQuery[, timePointYears := as.numeric(timePointYears)]
    bioFuelQuery
}


getConsolidatedTradeData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    consolidatedTradeKey = DatasetKey(
        domain = "trade",
        dataset = "total_tf_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementTrade",
                      keys = "5150"),
            Dimension(name = itemVar,
                      keys = ),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    consolidatedTradePivot = c(
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
    consolidatedTradeQuery = GetData(
        key = consolidatedTradeKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = consolidatedTradePivot
    )

    setnames(consolidatedTradeQuery,
             old = "measuredElementTrade",
             new = "measuredElement")

    ## Convert time to numeric
    consolidatedTradeQuery[, timePointYears := as.numeric(timePointYears)]
    consolidatedTradeQuery
}


## Get production data and then trade consolidated data of primary
## products. Adam assumed that only primary commodity are subject to
## industrial uses.

## Merge the two data set

mergeProductionTrade = function(productionData, consolidatedTradeData,
                                key = c(areaVar, yearVar, "measuredElement")){
    merge(productionData, consolidatedTradeData, by = key)
}

## Calculate primary non-oil seed industrial uses as 5% of the
## production and trade
calculateNonOilSeedIndustrialUses = function(){
}

## Calculate industrial uses both protein and energy for oil seed, the
## formula is ((1 - (production/raw production)) * domestic supply) -
## loss - seed
calculateOilSeedIndustrialUses = function(){
}


## Calculate industrial uses both energy and protein for palm
## fruit. It is defined as domestic supply - production * 0.03. Where
## is the reamining 3 percent going?
calculateSpecialIndustrialUses = function(){}

calculateIndustrialUses = function(){}

## Add the industrial uses calculated above to the biofuel

## NOTE (Michael): I do not agree with the arbitrary assignment of the
##                 percentages, in addition it does not account for
##                 the other utilizations and thus creates an
##                 over-utilization in the balance which has to be
##                 accounted via stock changes.

saveIndustrialUses = function(data){
    SaveData(domain = "industrialUse",
             dataset = "industrialuse",
             normalized = FALSE,
             data = data)
}


allCPCItem = getCPCTreeItem()

## Run the whole industrial use module.
##
## NOTE (Michael): Need the new target dataset and the element code of
##                 industrial uses.
getBioFuelData() %>%
    saveIndustrialUses(data = .)


## In Adam's implementation, he assume that palm fruit is fully
## subject to industrial uses. However, this is incorrect. Palm oil is
## edible and is mainly used for food, it is the oll and cake derived
## from kernel that is subject to industrial uses and feed.
