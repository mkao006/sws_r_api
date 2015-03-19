## NOTE (Michael): Feed availability should be computed with all
##                 commodities, thus so that the weight allocation is
##                 not mis-calculated.


suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(faoswsProductionImputation)
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
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
valuePrefix = "Value_"
flagObsPrefix = "flagObservationStatus_"
flagMethodPrefix = "flagMethod_"


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
}



## Function to obtain all CPC item 
getAllItemCPC = function(){
    itemEdgeList =
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

requiredItems = getAllItemCPC()

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

getTradeData = function(){

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

    ## NOTE (Michael): The unit for trade is in kg while for other
    ##                 elements are tonnes, so we divide the trade by
    ##                 1000 to match the unit.
    tradeQuery[, Value_measuredElementTrade_5600 :=
                   computeRatio(Value_measuredElementTrade_5600, 1000)]
    tradeQuery[, Value_measuredElementTrade_5900 :=
                   computeRatio(Value_measuredElementTrade_5900, 1000)]
    
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

    ## HACK (Michael): This is a hack, beacause the item hierachy
    ##                 configuration is different in the loss data set
    getLossItemCPC = function(){
        itemEdgeList =
            ## TODO (Michael): Have to change this back to feed
            ##                 availability when the data set is set up.
            ## adjacent2edge(
            ##     GetCodeTree(domain = "feed",
            ##                 dataset = "feedAvailability",
            ##                 dimension = itemVar)
            ## )
            adjacent2edge(
                GetCodeTree(domain = "lossWaste",
                            dataset = "loss",
                            dimension = "measuredItemSuaFbs")
            )
        itemEdgeGraph = graph.data.frame(itemEdgeList)
        itemDist = shortest.paths(itemEdgeGraph, v = "0", mode = "out")
        fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
        fbsItemCodes
    }

    lossItems = getLossItemCPC()
   
    
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
                      keys = lossItems),
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

    setnames(lossQuery,
             old = grep("measuredElementSuaFbs",
                 colnames(lossQuery), value = TRUE),
             new = gsub("measuredElementSuaFbs", "measuredElement",
                 grep("measuredElementSuaFbs",
                      colnames(lossQuery), value = TRUE)))
    setnames(lossQuery,
             old = "measuredItemSuaFbs",
             new = "measuredItemCPC")


    ## Convert time to numeric
    lossQuery[, timePointYears := as.numeric(timePointYears)]
    lossQuery

}

getIndustrialUseData = function(){


    industrialUseKey = DatasetKey(
        domain = "industrialUse",
        dataset = "industrialuse",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = elementVar,
                      keys = "5150"),
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

    standardizedFoodItem =
        GetCodeList(domain = "suafbs",
                    dataset = "fbs",
                    dimension = "measuredItemSuaFbs")[, code]
    
    foodItems = standardizedFoodItem[grepl("^S", standardizedFoodItem)]

    foodKey = DatasetKey(
        domain = "suafbs",
        dataset = "fbs",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = requiredCountries),
            Dimension(name = "measuredElementSuaFbs",
                      keys = "5142"),
            Dimension(name = "measuredItemSuaFbs",
                      keys = foodItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    foodPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementSuaFbs", ascending = TRUE)
    )

    ## Query the data
    foodQuery = GetData(
        key = foodKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = foodPivot
    )

    
    setnames(foodQuery,
             old = grep("measuredElementSuaFbs",
                 colnames(foodQuery), value = TRUE),
             new = gsub("measuredElementSuaFbs", "measuredElement",
                 grep("measuredElementSuaFbs",
                      colnames(foodQuery), value = TRUE)))
    setnames(foodQuery,
             old = "measuredItemSuaFbs",
             new = "measuredItemCPC")

    ## The value is in 1000t so we convert to tonnes.
    foodQuery[, Value_measuredElement_5142 :=
                  Value_measuredElement_5142 * 1000]

    ## HACK (Michael): This is a hack, we convert standardized item to
    ##                 the primary item. This is only acceptable if
    ##                 the current FBS framework works only with
    ##                 primary items.
    foodQuery[, measuredItemCPC := gsub("S", "", measuredItemCPC)]
    
    ## Convert time to numeric
    foodQuery[, timePointYears := as.numeric(timePointYears)]
    foodQuery

}

getFeedItemClassification = function(){
    feedClassification =
        GetTableData(schemaName = "ess", tableName = "feed_classification")
    setnames(feedClassification,
             old = c("cpc", "classification"),
             new = c("measuredItemCPC", "feedClassification"))
    feedClassification
}

## Function to impute missing crush rate
imputeCrushExtractionRates = function(data, countryVar, itemVar, yearVar,
    crushRateVar, oilExtractionRateVarDom,
    oilExtractionRateVarTrade, mealExtractionRateVarDom,
    mealExtractionRateVarTrade){

    dataCopy = copy(data)
    setkeyv(dataCopy, c(countryVar, itemVar, yearVar))

    ## Replace 0 with NA and then naive impute
    ##
    ## NOTE (Michael): Looking at the data, naive imputation (linear
    ##                 interpolation with last observation carried
    ##                 forward and backwards) seems to offer
    ##                 reasonable solution as most rates are constant.
    dataCopy[feedClassification == "oil seed",
             `:=`(c(crushRateVar,
                    oilExtractionRateVarDom,
                    oilExtractionRateVarTrade,
                    mealExtractionRateVarDom,
                    mealExtractionRateVarTrade),
                  lapply(c(crushRateVar,
                           oilExtractionRateVarDom,
                           oilExtractionRateVarTrade,
                           mealExtractionRateVarDom,
                           mealExtractionRateVarTrade),
                         FUN = function(x){
                             tmp = .SD[[x]]
                             tmp[tmp == 0] = NA
                             defaultNaive(tmp)
                         })),
             by = key(dataCopy)]

    ## Compute the commodity average rate by year
    dataCopy[feedClassification == "oil seed",
             `:=`(c("averageCrushRate",
                    "averageOilExtractionRateDom",
                    "averageOilExtractionRateTrade",
                    "averageMealExtractionRateDom",
                    "averageMealExtractionRateTrade"),
                  lapply(c(crushRateVar,
                           oilExtractionRateVarDom,
                           oilExtractionRateVarTrade,
                           mealExtractionRateVarDom,
                           mealExtractionRateVarTrade),
                         FUN = function(x) mean(.SD[[x]], na.rm = TRUE))),
             by = c(itemVar, yearVar)]

    ## Impute missing rates with commodity average rate
    dataCopy[is.na(dataCopy[[crushRateVar]]),
             `:=`(c(crushRateVar), averageCrushRate)]
    dataCopy[is.na(dataCopy[[oilExtractionRateVarDom]]),
             `:=`(c(oilExtractionRateVarDom), averageOilExtractionRateDom)]
    dataCopy[is.na(dataCopy[[oilExtractionRateVarTrade]]),
             `:=`(c(oilExtractionRateVarTrade), averageOilExtractionRateTrade)]
    dataCopy[is.na(dataCopy[[mealExtractionRateVarDom]]),
             `:=`(c(mealExtractionRateVarDom), averageMealExtractionRateDom)]
    dataCopy[is.na(dataCopy[[mealExtractionRateVarTrade]]),
             `:=`(c(mealExtractionRateVarTrade), averageMealExtractionRateTrade)]
    dataCopy[,`:=`(c("averageCrushRate",
                     "averageOilExtractionRateDom",
                     "averageOilExtractionRateTrade",
                     "averageMealExtractionRateDom",
                     "averageMealExtractionRateTrade"), NULL)]
    dataCopy
}



    

calculateFeedAvailability = function(data, itemVar, childItemVar, yearVar,
    feedVariable,
    productionVar, importVar, exportVar, seedVar, industrialUseVar, lossVar,
    foodVar, crushRateVar,
    oilExtractionRateVarDom, oilExtractionRateVarTrade, mealExtractionRateVarDom,
    mealExtractionRateVarTrade){

    ## Assume the data has been merged
    dataCopy = copy(data)

    ## Calculate trade to production ratio for weighting domestic and
    ## traded meal/oil extraction rates
    dataCopy[, netTrade := dataCopy[[importVar]] - dataCopy[[exportVar]]]
    dataCopy[netTrade < 0, netTrade := 0]
    dataCopy[is.na(dataCopy[[productionVar]]), `:=`(c(productionVar), 0)]
    dataCopy[, productionRatio :=
                 dataCopy[[productionVar]]/(dataCopy[[productionVar]] + netTrade)]


    ## Calculate feed availability based on classification
    dataCopy[feedClassification == "not for feed",
             `:=`(c(feedVariable), 0)]
    dataCopy[feedClassification == "potential feed",
             `:=`(c(feedVariable),
                  rowSums(.SD[, c(productionVar, importVar), with = FALSE],
                          na.rm = TRUE) -
                  rowSums(.SD[, c(exportVar, seedVar, industrialUseVar, lossVar,
                                  foodVar), with = FALSE],
                          na.rm = TRUE))]
    dataCopy[feedClassification == "feedOnly",
             `:=`(c(feedVariable),
                  rowSums(.SD[, c(productionVar, importVar), with = FALSE],
                          na.rm = TRUE) -
                  rowSums(.SD[, exportVar, with = FALSE], na.rm = TRUE))]

    ## NOTE (Michael): This is temporary, oil seeds will have to be
    ##                 converted to oil and meals when the "roll down"
    ##                 procedure is implemented overall.
    dataCopy[feedClassification == "oil seeds",
             `:=`(c(feedVariable),
                  rowSums(.SD[, c(productionVar, importVar), with = FALSE],
                          na.rm = TRUE) -
                  rowSums(.SD[, c(exportVar, seedVar, industrialUseVar, lossVar,
                                  foodVar), with = FALSE],
                          na.rm = TRUE))]
    
    ## If feed availability is negative, then assign zero symboling no
    ## availability.
    dataCopy[dataCopy[[feedVariable]] < 0, `:=`(c(feedVariable), 0)]

    ## Assign the flag for feed
    dataCopy[, `:=`(c(gsub(valuePrefix, flagObsPrefix, feedVariable),
                      gsub(valuePrefix, flagMethodPrefix, feedVariable)),
                    list("E", "e"))]
    dataCopy
}



## Function to merge all necessary data
mergeAllData = function(...){
    datasets = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all = TRUE)
    },
           x = datasets[-1], init = datasets[[1]])
}


saveFeedData = function(data){
    SaveData(domain = "feed",
                dataset = "feed_availability",
                data = data[, c(areaVar, itemVar, yearVar,
                    paste0(c(valuePrefix, flagObsPrefix, flagMethodPrefix),
                           elementVar, "_5033")), with = FALSE],
                normalized = FALSE)
}


## Calculate feed availability
feedAvailability =
    {
        if(verbose){
            cat("Extracting raw data\n")
            currentTime = Sys.time()
        }
        production <<- getProductionData()
        trade <<- getTradeData()
        seed <<- getSeedData()
        loss <<- getLossData()
        indUse <<- getIndustrialUseData()
        food <<- getFoodData()
        feedClassification <<- getFeedItemClassification()
    } %>%
    {
        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
            cat("Merge All Required Data\n")
        }
        ## Merge all the data together
        mergeAllData(production, trade, seed, loss, indUse, food) %>%
        merge(x = ., feedClassification, all.x = TRUE,
              by = "measuredItemCPC")
    } %>%
    {
        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
            cat("Impute extraction rates\n")
        }
        ## Impute crush and oil/meal extraction rates
        imputeCrushExtractionRates(data = .,
                                   countryVar = "geographicAreaM49",
                                   itemVar = "measuredItemCPC",
                                   yearVar = "timePointYears",
                                   crushRateVar = "Value_measuredElement_52",
                                   oilExtractionRateVarDom =
                                       "Value_measuredElement_53",
                                   oilExtractionRateVarTrade =
                                       "Value_measuredElement_54",
                                   mealExtractionRateVarDom =
                                       "Value_measuredElement_63",
                                   mealExtractionRateVarTrade =
                                       "Value_measuredElement_64")
    } %>%
    {
        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
            cat("Calculate Feed Availability\n")
    }

        ## Calculate feed availability
        calculateFeedAvailability(data = .,
                                  itemVar = "measuredItemCPC",
                                  childItemVar = "measuredItemCPCChild",
                                  yearVar = "timePointYears",
                                  feedVariable = "Value_measuredElement_5033",
                                  productionVar = "Value_measuredElement_5510",
                                  importVar = "Value_measuredElement_5600",
                                  exportVar = "Value_measuredElement_5900",
                                  seedVar = "Value_measuredElement_5525",
                                  industrialUseVar =
                                      "Value_measuredElement_5150",
                                  lossVar = "Value_measuredElement_5120",
                                  foodVar = "Value_measuredElement_5142",
                                  crushRateVar = "Value_measuredElement_52",
                                  oilExtractionRateVarDom =
                                      "Value_measuredElement_53",
                                  oilExtractionRateVarTrade =
                                      "Value_measuredElement_54",
                                  mealExtractionRateVarDom =
                                      "Value_measuredElement_63",
                                  mealExtractionRateVarTrade =
                                      "Value_measuredElement_64")
    } %>%
    saveFeedData(data = .)


if(verbose){
    endTime = Sys.time()
    timeUsed = endTime - currentTime
    cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
    currentTime = endTime
}


## usafa =
##     feedAvailability[geographicAreaM49 == "840" & timePointYears == 2010,
##                      c("geographicAreaM49", "measuredItemCPC", "timePointYears",
##                        grep("Value_measuredElement_[0-9]{4}",
##                             colnames(feedAvailability), value = TRUE)),
##                      with = FALSE]
## setnames(usafa, old = grep("Value", colnames(usafa), value = TRUE),
##          new = c("production", "export", "import", "seed", "loss", "industrialUse",
##          "food", "feedAvailability"))
## write.csv(usafa, file = "~/Desktop/usa2010FeedAvailability.csv",
##           row.names = FALSE, na = "")
