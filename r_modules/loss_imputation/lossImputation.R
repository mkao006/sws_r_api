suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(lme4)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(igraph)
})

updateModel = TRUE


## Year should be a paramameter selected.
selectedYear = as.character(1992:2015)

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
        token = "007e9eea-5766-41c8-9495-8ad7be4124cc"
        )
    R_SWS_SHARE_PATH = getwd()
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
    updateModel = as.logical(swsContext.computationParams$updateModel)
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


getProductionData = function(){
    allCountries =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")[type == "country", code]
    
    productionKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = allCountries),
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

getImportData = function(){
    allCountries =
        GetCodeList(domain = "trade",
                    dataset = "total_trade_CPC",
                    dimension = "geographicAreaM49")[type == "country", code]

    importKey = DatasetKey(
        domain = "trade",
        dataset = "total_trade_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = allCountries),
            Dimension(name = "measuredElementTrade",
                      keys = "5600"),
            Dimension(name = "measuredItemCPC",
                      keys = requiredItems),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    importPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    importQuery = GetData(
        key = importKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = importPivot
    )

    ## NOTE (Michael): The unit for trade is in kg while for other
    ##                 elements are tonnes, so we divide the trade by
    ##                 1000 to match the unit.
    importQuery[, Value_measuredElementTrade_5600 :=
                   computeRatio(Value_measuredElementTrade_5600, 1000)]
    
    setnames(importQuery,
             old = grep("measuredElementTrade",
                 colnames(importQuery), value = TRUE),
             new = gsub("measuredElementTrade", "measuredElement",
                 grep("measuredElementTrade",
                      colnames(importQuery), value = TRUE)))


    ## Convert time to numeric
    importQuery[, timePointYears := as.numeric(timePointYears)]
    importQuery

}




getOfficialLossData = function(){
    allCountries =
        GetCodeList(domain = "lossWaste",
                    dataset = "loss",
                    dimension = "geographicAreaM49")[type == "country", code]

    ## HACK (Michael): This is a hack, beacause the item hierachy
    ##                 configuration is different in the loss data set
    getLossItemCPC = function(){
        itemEdgeList =
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
                      keys = allCountries),
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
    lossQuery[flagObservationStatus_measuredElement_5120 == "", ]
}

getSelectedLossData = function(){

    
    ## Pivot to vectorize yield computation
    lossPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementSuaFbs", ascending = TRUE)
    )

    ## Query the data
    lossQuery = GetData(
        key = swsContext.datasets[[1]],
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


getLossWorldBankData = function(){
    allCountries =
        GetCodeList(domain = "WorldBank",
                    dataset = "wb_ecogrw",
                    dimension = "geographicAreaM49")[type == "country", code]
   
    infrastructureKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_infrastructure",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = allCountries),
                           Dimension(name = "wbIndicator",
                                     keys = "IS.ROD.PAVE.ZS"),
                           Dimension(name = "timePointYears",
                                     keys = selectedYear)
                       )
                   )
    
    gdpKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_ecogrw",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = allCountries),
                           Dimension(name = "wbIndicator",
                                     keys = c("NY.GDP.MKTP.PP.KD",
                                         "NY.GDP.PCAP.KD")),
                           Dimension(name = "timePointYears",
                                     keys = selectedYear)
                       )
                   )

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "wbIndicator", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE)
    )

    base =
        data.table(geographicAreaM49 = character(),
                   wbIndicator = character(),
                   timePointYears = character(),
                   Value = numeric())

    merged =
        Reduce(f = function(base, key){
            rbind(base, GetData(key, pivoting = newPivot))
        }, x = list(infrastructureKey, gdpKey), init = base)
    
    casted =
        dcast.data.table(merged,
                         geographicAreaM49 + timePointYears ~ wbIndicator,
                         value.var = "Value")
    setnames(casted,
             old = c("IS.ROD.PAVE.ZS", "NY.GDP.MKTP.PP.KD",
                 "NY.GDP.PCAP.KD"),
             new = c("sharePavedRoad", "gdpPPP", "gdpPerCapita"))
    casted[, timePointYears := as.numeric(timePointYears)]
    setkeyv(casted, cols = c("geographicAreaM49", "timePointYears"))
    casted
}


## Function to load the loss food group classification
getLossFoodGroup = function(){
    lossFoodGroup = GetTableData(schemaName = "ess", tableName = "loss_food_group")
    setnames(lossFoodGroup, old = colnames(lossFoodGroup),
             new = c("measuredItemFS", "measuredItemNameFS", "foodGroupName",
                 "foodGroup", "foodGeneralGroup", "foodPerishableGroup",
                 "measuredItemCPC"))
    lossFoodGroup[, list(measuredItemCPC, foodGroupName,
                         foodGeneralGroup, foodPerishableGroup)]
    lossFoodGroup
}

## Function to load the loss region classification
getLossRegionClass = function(){
    regionMapping =
        GetTableData(schemaName = "ess", tableName = "loss_region_mapping")
    setnames(regionMapping, old = colnames(regionMapping),
             new = c("geographicAreaM49", "lossRegionClass"))    
    regionMapping
}


imputeSharePavedRoad = function(wbData, pavedRoadVar){
    foo = function(x){
        if(length(na.omit(x)) >= 2){
            tmp = na.locf(na.approx(x, na.rm = FALSE), na.rm = FALSE)
        } else {
            tmp = x
        }
        tmp
    }
    wbData[, `:=`(c(pavedRoadVar),
                  foo(.SD[[pavedRoadVar]])),
         by = "geographicAreaM49"]
}

mergeAllLossData = function(lossData, ...){
    explanatoryData = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all.x = TRUE)
    },
           x = explanatoryData, init = lossData
           )
}


removeCarryLoss = function(data, lossVar){
    data[, variance := var(.SD[[lossVar]], na.rm = TRUE),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data[, duplicateValue := duplicated(.SD[[lossVar]]),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data = data[!(variance == 0 & duplicateValue), ]
    data[, `:=`(c("variance", "duplicateValue"), NULL)]
    data         
}

imputeLoss = function(data, lossVar, lossObservationFlagVar, lossMethodFlagVar,
    lossModel){
    imputedData = copy(data)
    imputedData[, predicted := exp(predict(lossModel, newdata = imputedData,
                                allow.new.levels = TRUE))]
    imputedData[(is.na(imputedData[[lossVar]]) |
                 imputedData[[lossObservationFlagVar]] %in% c("E", "I", "T")) &
                !is.na(predicted),
                `:=`(c(lossVar, lossObservationFlagVar, lossMethodFlagVar),
                     list(predicted, "I", "e"))]
    imputedData[, predicted := NULL]
    imputedData
}


saveImputedLoss = function(data){
    saveSelection =
        subset(data,
               select = c("geographicAreaM49", "measuredItemCPC",
                   "timePointYears", "Value_measuredElement_5120",
                   "flagObservationStatus_measuredElement_5120",
                   "flagMethod_measuredElement_5120"))
    setnames(saveSelection,
             old = grep("measuredElement", colnames(saveSelection), value = TRUE),
             new = gsub("measuredElement", "measuredElementSuaFbs",
                 grep("measuredElement", colnames(saveSelection), value = TRUE)))
    setnames(saveSelection,
             new = "measuredItemSuaFbs",
             old = "measuredItemCPC")

    saveSelection[, measuredItemSuaFbs := as.character(measuredItemSuaFbs)]
    
    SaveData(domain = "lossWaste",
             dataset = "loss",
             data = saveSelection,
             normalized = FALSE)
}




if(updateModel){
    finalModelData = 
        {
            requiredItems <<- getAllItemCPC()
            production <<- getProductionData()
            import <<- getImportData()
            loss <<- getOfficialLossData()
            ## NOTE (Michael): Don't really need world bank data, as those
            ##                 variables does not aid to the model
            ##
            ## wb <<- getLossWorldBankData()
            lossFoodGroup <<- getLossFoodGroup()
            lossRegionClass <<- getLossRegionClass()
            countryTable <<-
                GetCodeList(domain = "agriculture",
                            dataset = "agriculture",
                            dimension = "geographicAreaM49")[type == "country",
                                list(code, description)]
            setnames(countryTable,
                     old = c("code", "description"),
                     new = c("geographicAreaM49", "geographicAreaM49Name"))
        } %>%
        mergeAllLossData(lossData = loss,
                         production, import, lossFoodGroup,
                         lossRegionClass, countryTable) %>%
        subset(x = .,
               subset = Value_measuredElement_5120 > 0 &
                        foodGeneralGroup == "primary" &
                        Value_measuredElement_5510 != 0,
               select = c("geographicAreaM49", "geographicAreaM49Name",
                   "measuredItemCPC", "timePointYears",
                   "Value_measuredElement_5120", "Value_measuredElement_5510",
                   "Value_measuredElement_5600", "foodGroupName",
                   "foodPerishableGroup", "lossRegionClass")) %>%
        removeCarryLoss(data = ., lossVar = "Value_measuredElement_5120") %>%
        ## Convert variables to factor
        .[, `:=`(c("measuredItemCPC", "foodGroupName", 
                   "foodPerishableGroup", "lossRegionClass"),
                 lapply(c("measuredItemCPC", "foodGroupName", 
                          "foodPerishableGroup", "lossRegionClass"),
                        FUN = function(x) as.factor(.SD[[x]])
                        )
                 )
          ]

    ## NOTE (Michael): Here we have not yet added in import yet, since
    ##                 there are only data for 2010. However, import
    ##                 should be added when it is available.
    
    ## lossLmeModel =
    ##     lmer(log(Value_measuredElement_5120) ~ -1 + timePointYears +
    ##          log(Value_measuredElement_5510 + 1) + 
    ##          (log(Value_measuredElement_5510 + 1)|
    ##               lossRegionClass/geographicAreaM49Name:
    ##                   foodPerishableGroup/foodGroupName/measuredItemCPC),
    ##          data = finalModelData)

    lossLmeModel =
        lmer(log(Value_measuredElement_5120) ~ timePointYears +
             log(Value_measuredElement_5510 + 1) + 
             (-1 + log(Value_measuredElement_5510 + 1)|
                  foodPerishableGroup/foodGroupName/measuredItemCPC/geographicAreaM49Name),
             data = finalModelData)
    
    lossModelPath = paste0(R_SWS_SHARE_PATH, "/lossLmeModel")
    saveRDS(lossLmeModel, lossModelPath)
} else {
    lossLmeModel = readRDS(file = lossModelPath)
}





finalPredictData = 
    {
        if(!updateModel){
            requiredItems <<- getAllItemCPC()
            production <<- getProductionData()
            import <<- getImportData()
            lossFoodGroup <<- getLossFoodGroup()
            lossRegionClass <<- getLossRegionClass()
            countryTable <<-
                GetCodeList(domain = "agriculture",
                            dataset = "agriculture",
                            dimension = "geographicAreaM49")[type == "country",
                                list(code, description)]
            setnames(countryTable,
                     old = c("code", "description"),
                     new = c("geographicAreaM49", "geographicAreaM49Name"))
        }
        loss <<- getSelectedLossData()
    } %>%
    mergeAllLossData(lossData = loss,
                     production, import, lossFoodGroup,
                     lossRegionClass, countryTable) %>%
    subset(x = .,
           subset = Value_measuredElement_5120 > 0 &
                    foodGeneralGroup == "primary" &
                    Value_measuredElement_5510 != 0,
           select = c("geographicAreaM49", "geographicAreaM49Name",
               "measuredItemCPC", "timePointYears", "Value_measuredElement_5120",
               "flagObservationStatus_measuredElement_5120",
               "flagMethod_measuredElement_5120",
               "Value_measuredElement_5510", "Value_measuredElement_5600",
               "foodGroupName", "foodPerishableGroup", "lossRegionClass")) %>%
    removeCarryLoss(data = ., lossVar = "Value_measuredElement_5120") %>%
    ## Convert variables to factor
    .[, `:=`(c("measuredItemCPC", "foodGroupName", 
               "foodPerishableGroup", "lossRegionClass"),
             lapply(c("measuredItemCPC", "foodGroupName", 
                      "foodPerishableGroup", "lossRegionClass"),
                    FUN = function(x) as.factor(.SD[[x]])
                    )
             )
      ]


## Impute selected data
finalPredictData %>%
    imputeLoss(data = .,
               lossVar = "Value_measuredElement_5120",
               lossObservationFlagVar =
                   "flagObservationStatus_measuredElement_5120",
               lossMethodFlagVar = "flagMethod_measuredElement_5120",
               lossModel = lossLmeModel) %>%
    saveImputedLoss(data = .)


## par(mfrow = c(1, 2))
## hist(finalModelData[Value_measuredElement_5120 < Value_measuredElement_5510,
##                     Value_measuredElement_5120/Value_measuredElement_5510],
##      breaks = 100, xlim = c(0, 1))
## hist(finalPredictData[Value_measuredElement_5120 < Value_measuredElement_5510,
##                       Value_measuredElement_5120/Value_measuredElement_5510],
##      breaks = 100, xlim = c(0, 1))
