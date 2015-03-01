suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(igraph)
})

verbose = FALSE

if(verbose){
    startingTime = Sys.time()
    currentTime = startingTime
}


## Year should be a paramameter selected.
selectedYear = "2010"

areaVar = "geographicAreaM49"
yearVar = "timePointYears"
## TODO (Michael): Change the item and element variable names after
##                 Nick has corrected the name in the database.
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"
## NOTE (Michael): The years were selected by Klaus but we reduced it for testing.
## selectedYear = as.character(1970:2013)
selectedYear = as.character(2000:2013)


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "09aeec5c-2cf5-46f7-90e7-f8b4bdc6dc93"
        )
    verbose = TRUE
    R_SWS_SHARE_PATH = getwd()
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
    ## R_SWS_SHARE_PATH = "hqlprsws1.hq.un.fao.org/sws_r_share"
}



## Function to obtain all CPC item 
getFBSmeasuredItemCPC = function(){
    itemEdgeList =
        ## TODO (Michael): Have to change this back to feed
        ##                 availability when the data set is set up.
        ## adjacent2edge(
        ##     GetCodeTree(domain = "feed",
        ##                 dataset = "feedAvailability",
        ##                 dimension = itemVar)
        ## )
        adjacent2edge(
            GetCodeTree(domain = "agriculture",
                        dataset = "agriculture",
                        dimension = itemVar)
        )
    itemEdgeGraph = graph.data.frame(itemEdgeList)
    itemDist = shortest.paths(itemEdgeGraph, v = "0", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
    fbsItemCodes
}

requiredItems = getFBSmeasuredItemCPC()

## Obtain all the countries in the feed domain
requiredCountries =
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = areaVar)[type == "country", code]

## Get production data
getProductionData = function(){
    productionKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = elementVar,
                      keys = "5510"),
            Dimension(name = itemVar,
                      keys = requiredItems),
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

getConsolidatedTradeData = function(){
    ## We only take import quantity for the waste module
    tradeKey = DatasetKey(
        domain = "trade",
        dataset = "total_trade_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = "measuredElementTrade",
                      keys = c("5600", "5900")),
            Dimension(name = "measuredItemCPC",
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeQuery = GetData(
        key = tradeKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradePivot
    )

    ## NOTE (Michael): The unit for trade is in kg while for
    ##                 production is ton, so we divide the trade by
    ##                 1000 to match production.
    tradeQuery[, Value_measuredElementTrade_5600 :=
                   computeRatio(Value_measuredElementTrade_5600, 1000)]
    
    setnames(tradeQuery,
             old = grep("measuredElementTrade",
                 colnames(tradeQuery), value = TRUE),
             new = gsub("measuredElementTrade", "measuredElement",
                 grep("measuredElementTrade",
                      colnames(tradeQuery), value = TRUE)))


    ## Convert time to numeric
    tradeQuery[, timePointYears := as.numeric(timePointYears)]
    tradeQuery

}


getSeedData = function(){
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = elementVar,
                      keys = "5525"),
            Dimension(name = itemVar,
                      keys = requiredItems),
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

getLossData = function(){

    ## NOTE (Michael): The cpc tree loaded in the loss data set is
    ##                 different to the rest. Thus I can not query
    ##                 item such as 0419.
    lossKey = DatasetKey(
        domain = "lossWaste",
        dataset = "loss",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = "measuredElementSuaFbs",
                      keys = "5120"),
            Dimension(name = "measuredItemSuaFbs",
                      keys = requiredItems),
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

getIndustrialUseData = function(){

    ## Need the element code of industrial uses
    industrialUseKey = DatasetKey(
        domain = "industrialUse",
        dataset = "industrialUse",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = elementVar,
                      keys = "5525"),
            Dimension(name = itemVar,
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    industrialUsePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    industrialUseQuery = GetData(
        key = industrialUseKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = industrialUsePivot
    )

    ## Convert time to numeric
    industrialUseQuery[, timePointYears := as.numeric(timePointYears)]
    industrialUseQuery

}

getFoodData = function(){

    ## TODO (Michael): Need to get the dataset and domain when the
    ##                 food data has been loaded.
    ##    
    ## Need the element code of industrial uses
    foodKey = DatasetKey(
        domain = "",
        dataset = "",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = elementVar
                      keys = ),
            Dimension(name = itemVar
                      keys = ),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    foodPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    foodQuery = GetData(
        key = foodKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = foodPivot
    )

    ## Convert time to numeric
    foodQuery[, timePointYears := as.numeric(timePointYears)]
    foodQuery

}

getFeedItemClassification = function(){
    ## classificationFilePath = paste0(R_SWS_SHARE_PATH, "/cpcFeedClassification.csv")
    ## data.table(read.csv(file = classificationFilePath,
    ##                     colClass = rep("character", 3)))
    data.table(read.csv(file = "cpcFeedClassification.csv"))
}

getOCBSPrimaryMapping = function(){}

getOCBSProcessedMapping = function(){}


computeOilDerivatives = function(data, extractionRateData){

    ## Aloowing cartesian join as one parent can have multiple child derivatives
    merged = merge(data, extractionRateData, allow.cartesian = TRUE)
    data[, value * crush_rate * extractionRate]
}

## This will be saved back to a new dataset called feed availability
calculateFeedAvailability = function(){
    ## Assume the data has been merged

    dataCopy = copy(data)

    ## Split the data based on classification for calculation
    nonFeedItem = subset(dataCopy, feedClassification == "not for feed")
    potentialFeedItem = subest(dataCopy, feedClassification == "potential feed")
    pureFeedItem = subset(dataCopy, feedClassification == "feedOnly")

    nonFeedItem[, `:=`(c(feedVariable), 0)]
    potentialFeedItem[, `:=`(c(feedVariable),
                             .SD[[productionVar]] + .SD[[importVar]] -
                             .SD[[exportVar]] - .SD[[seedVar]] -
                             .SD[[industrialUseVar]] - .SD[[lossVar]] -
                             .SD[[foodVar]])]

    ## For pure feed item we don't need to account for other utilizations
    pureFeedItem[, `:=`(c(feedVariable),
                        .SD[[productionVar]] + .SD[[importVar]] -
                        .SD[[exportVar]])]

    ## For oil item we have to first derive the availability then
    ## multiply by the crush rate and meal/oil extraction rates
    

}




