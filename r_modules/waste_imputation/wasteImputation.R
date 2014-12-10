suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(pryr)
    library(biglm)
})



## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "40662c94-ecff-40cf-b2f7-d739bc031411"
        )
}

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
requiredElements = c("5510", "5610", "5712", "5015")
names(requiredElements) = c("production", "import", "stockWithdrawl", "loss")
requiredItems = GetCodeList("agriculture", "agriculture", "measuredItemCPC")$code
valuePrefix = "Value_measuredElement_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"



## Define required functions
## ---------------------------------------------------------------------


## Function to get the 2 external world bank data sets and merge them
getLossExternalData = function(){
    ## ## TODO (Michael): Get the data using R API
    ## worldBankGeneralData =
    ##     data.table(read.csv(file = "data/worldBankGeneralData.csv"))
    ## ## worldBankGeneral
    ## worldBankClimateData =
    ##     data.table(read.csv(file = "data/worldBankClimateData.csv"))
    worldBankdGeneralData =
        GetTableData(schemaName = "ess", tableName = "worldBankGeneralData")
    worldBankClimateData =
        GetTableData(schemaName = "ess", tableName = "worldBankClimateData")
    
    merged = Reduce(f = function(x, y){
        merge(x, y, all = TRUE, by = intersect(colnames(x), colnames(y)))
    }, x = list(worldBankGeneralData, worldBankClimateData))
    merged[, geographicAreaM49 := as.character(geographicAreaM49)]
    merged
}

## Function to load the loss food group classification
getLossFoodGroup = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base
    ## data.table(read.csv(file = "data/lossFoodGroup.csv"))
    GetTableData(schemaName = "ess", tableName = "lossFoodGroup")
}

## Function to load the loss region classification
getLossRegionClass = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base.
    ## regionMapping = data.table(read.csv(file = "data/lossRegionMapping.csv"))
    ## regionMapping[, geographicAreaM49 := as.character(geographicAreaM49)]
    regionMapping =
        GetTableData(schemaName = "ess", tableName = "lossRegionMapping")
    regionMapping
}

## Function to load the national fbs dataset
getNationalFbs = function(){
    ## NOTE (Michael): This will be replaced by the GetMapping
    ##                 function when loaded into the data base.
    ## data.table(read.csv(file = "data/nationalFbs.csv"))
    GetTableData(schemaName = "ess", tableName = "nationalFbs")
}

## Function to load the data required for loss estimation
getLossData = function(){
    ## Set up the query
    ##
    ## NOTE (Michael): The year is set by Klaus
    allCountryCodesTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")


    dimensions =
        list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountryCodesTable[type == "country", code]),
            Dimension(name = "measuredItemCPC", keys = requiredItems),
            Dimension(name = "measuredElement", keys = unname(requiredElements)),
            Dimension(name = "timePointYears", keys = as.character(1969:2013))
        )

    newDataKey =
        DatasetKey(domain = "agriculture",
                   dataset = "agriculture",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    query = GetData(
        key = newDataKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
    )
    query[, timePointYears := as.numeric(timePointYears)]
    query
}


## NOTE (Michael): Function to get trade data, however, there are no
##                 data in the trade domain. Thus the data in this
##                 example is simulated. See hackData function.
getTradeData = function(){
    ## Set up the query
    ##

    ## NOTE (Michael): The year is set by Klaus
    allCountryCodesTable =
        GetCodeList(domain = "trade",
                    dataset = "total_trade",
                    dimension = "geographicAreaM49")

    cerealTree =
        adjacent2edge(GetCodeTree(domain = "trade",
                    dataset = "total_trade",
                    dimension = "measuredItemHS",
                    roots = "10"))

    tradeElements =
        GetCodeList(domain = "trade",
                    dataset = "total_trade",
                    dimension = "measuredElementTrade")

    
    dimensions =
        list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountryCodesTable[type == "country", code]),
            Dimension(name = "measuredItemHS", keys = unique(cerealTree$children)),
            Dimension(name = "measuredElementTrade", keys = tradeElements[, code]),
            Dimension(name = "timePointYears", keys = as.character(2000:2013))
        )

    newDataKey =
        DatasetKey(domain = "trade",
                   dataset = "total_trade",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemHS", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )
    
    ## Query the data
    query = GetData(
        key = newDataKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
    )
    query[, timePointYears := as.numeric(timePointYears)]
    query
}


## This is the function to get stock variation data, however the data
## does not exist and is simulated.
getStockVariationData = function(){}


## Function to merge the national fbs data to the loss data
mergeNationalFbs = function(lossData, nationalFbs){
    lossWithNationalFbs = rbind(lossData, nationalFbs, fill = TRUE)
    lossWithNationalFbs[, fromNationalFbs :=
                            c(rep(0, NROW(lossData)), rep(1, NROW(nationalFbs)))]
    lossWithNationalFbs
}
    

## Function to merge all the required data
mergeAllLossData = function(lossData, lossExternalData, lossFoodGroup,
    lossRegionClass){
    Reduce(f = function(x, y){
        merge(x, y, all.x = TRUE, by = intersect(colnames(x), colnames(y)))
    },
           x = list(lossExternalData, lossFoodGroup, lossRegionClass),
           init = lossData
           )
}


## HACK (Michael): Fill in non-classified food group and region
fillUnclassifiedFoodGroup = function(data,
    foodGroupClassification = c("foodGroupName", "foodGeneralGroup",
        "foodPerishableGroup")){
    sapply(foodGroupClassification,
           FUN = function(x){
               data[is.na(data[[x]]), `:=`(c(x), "unclassified")]
               invisible()
           }
           )
    data
}

fillUnclassifiedRegion = function(data, regionClassification = "lossRegionClass"){
    sapply(regionClassification,
           FUN = function(x){
               data[is.na(data[[x]]), `:=`(c(x), "unclassified")]
               invisible()
           }
           )
    data    
}

## This function fills in data requirments which are not currently in the data
dataHack = function(data){
    ## HACK (Michael): This is a hack to simulate trade data
    data[, Value_measuredElement_5610 :=
             abs(rnorm(.N, Value_measuredElement_5510,
                       sd(Value_measuredElement_5510, na.rm = TRUE))),
         by = c("geographicAreaM49", "measuredItemCPC")]
    ## HACK (Michael): Since trade and stock variation data are not
    ##                 available, we simulate the loss rate data as well.
    data[sample(1:NROW(data), NROW(data) * 0.5),
         lossRatio := runif(.N, min = 0.1, max = 0.9)]
    data[!is.finite(lossRatio) | lossRatio < 0, lossRatio := NA]
    data
}


## Function to calculate the ratio
##
calculateLossRatio = function(data,
    productionValue = paste0(valuePrefix, requiredElements["production"]),
    importValue = paste0(valuePrefix, requiredElements["import"]),
    stockWithdrawlValue = paste0(valuePrefix, requiredElements["stockWithdrawl"]),
    lossValue = paste0(valuePrefix, requiredElements["loss"])){

    data[data[[stockWithdrawlValue]] >= 0,
         lossBase := 
             rowSums(.SD[, c(productionValue, importValue, stockWithdrawlValue),
                         with = FALSE],
                     na.rm = TRUE)]
    data[data[[stockWithdrawlValue]] < 0,
         lossBase := 
             rowSums(.SD[, c(productionValue, importValue), with = FALSE],
                     na.rm = TRUE)]
    
    data[, lossRatio := computeRatio(.SD[[lossValue]], lossBase)]
    data
}



## NOTE (Michael): Try to remove the hard code
##
## Function to perform final manipulation
preEstimationProcessing = function(data){
    ## Convert variables to factor for modelling
    factorVariables = c("geographicAreaM49", "measuredItemCPC", "foodGroupName",
                  "foodGeneralGroup", "foodPerishableGroup", "lossRegionClass")
    data[, `:=`(c(paste0(factorVariables, "Factor")),
                lapply(data[, factorVariables, with = FALSE], as.factor))]
    

    ## TODO (Michael): Need to remove these hard coded processing
    data[is.na(Value_measuredElement_5610), Value_measuredElement_5610 := 0]
    data[is.na(Value_measuredElement_5510), Value_measuredElement_5510 := 0]
    data[, importToProductionRatio :=
             computeRatio(Value_measuredElement_5610, Value_measuredElement_5510)]
    data[, scaledTimePointYears := timePointYears - 1960]
    data[is.na(fromNationalFbs), fromNationalFbs := 0]

    ## NOTE (Klaus): GDP per capita over 25000 are truncated and
    ##               assume it does not have any relevant effects on
    ##               the changes in losses.
    data[gdpPerCapita > 25000, gdpPerCapita := 25000]

    ## NOTE (Klaus): Assume the food group level of meat is the same as
    ##               meat and fishes.
    levels(data$foodGroupNameFactor) =
        with(data,
             ifelse(levels(foodGroupNameFactor) == "meat", "meat and fish",
                    levels(foodGroupNameFactor)))

    data
}


## Function to create the desired estimation sample
##
## NOTE (Michael): This is currently obsolete
splitLossData = function(data, estimationSubset){

    ## NOTE (Michael): This is hard coded selection by Klaus
    if(missing(estimationSubset))
        estimationSubset =
            expression(which(timePointYears > 1969 & 
                             importToProductionRatio < 1 &
                             lossRatio != 0 &
                             geographicAreaM49 != "170" &
                             foodGeneralGroup == "primary" &
                             !is.na(gdpPerCapita) &
                             !is.na(sharePavedRoad)))
   
    estimationSubsetIndex = eval(substitute(estimationSubset), data)

    estimationData = data[estimationSubsetIndex, ]
    predictionData = data[-estimationSubsetIndex, ]
    list(estimationData = estimationData, predictionData = predictionData)
}




## Function to estimate the loss regression
lossRegression = function(estimationData){

    ## REGESSION (1): Item-specific dummies

    itemSpecificLoss.lm =
        lm(I(log(lossRatio+0.05)) ~ measuredItemCPCFactor + lossRegionClassFactor +
           scaledTimePointYears + foodPerishableGroupFactor + sharePavedRoad +
           sharePavedRoad:foodPerishableGroupFactor + I(log(gdpPerCapita)) +
           I(log(gdpPerCapita)^2) +
           I(log(gdpPerCapita)):foodPerishableGroupFactor +
           I(log(gdpPerCapita)^2):foodPerishableGroupFactor +
           fromNationalFbs, data = estimationData)


    ## REGESSION (2): No item-specific dummies.
    ##
    ## This regression is performed for impute losses for commodities
    ## for which no (or very few) observations are available.
    ##
    ## Use item group-specific dummies. (Both, item and group dummies,
    ## cannot be used at the same scaledTimePointYears.)

    foodGroupLoss.lm =
        lm(I(log(lossRatio + 0.05)) ~ foodGroupNameFactor + lossRegionClassFactor +
           scaledTimePointYears + foodPerishableGroupFactor + sharePavedRoad +
           sharePavedRoad:foodPerishableGroupFactor + I(log(gdpPerCapita)) +
           I(log(gdpPerCapita)^2) +
           I(log(gdpPerCapita)):foodPerishableGroupFactor +
           I(log(gdpPerCapita)^2):foodPerishableGroupFactor +
           fromNationalFbs, data = estimationData)

    list(itemSpecificModel = itemSpecificLoss.lm,
         foodGroupModel = foodGroupLoss.lm)
}


## Function to take the model and make imputation
lossModelPrediction = function(model, predictionData, lossRatio){
    imputedData = copy(predictionData)
    lapply(model, function(x){
        missingLossRatio = which(is.na(imputedData[[lossRatio]]))
        imputedData[missingLossRatio,
                       `:=`(c(lossRatio),
                            list(c(exp(predict(x, newdata = .SD)) - 0.05)))]
    })
    imputedData[, `:=`(c(paste0(valuePrefix, requiredElements[["loss"]])),
                lossBase * lossRatio)]
    imputedData
}
           
## Function to select the required variable and dimension for the
## estimation of the model.
selectRequiredVariable = function(data){
    data[foodGeneralGroup == "primary",
         list(geographicAreaM49, measuredItemCPC, timePointYears,
              Value_measuredElement_5015,
              flagObservationStatus_measuredElement_5015,
              flagMethod_measuredElement_5015, fromNationalFbs, gdpPerCapita,
              sharePavedRoad, lossBase, lossRatio, geographicAreaM49Factor,
              measuredItemCPCFactor, foodGroupNameFactor, foodGeneralGroupFactor,
              foodPerishableGroupFactor, lossRegionClassFactor,
              importToProductionRatio, scaledTimePointYears)]
}
                
## Function to save the data back 
SaveLossData = function(data){
    saveSelection =
        data[, intersect(colnames(data), colnames(lossData)),
             with = FALSE]
    ## HACK (Michael): This is a hack, since there are no flag assignments yet
    saveSelection[, `:=`(c("flagObservationStatus_measuredElement_5015",
                           "flagMethod_measuredElement_5015"),
                         list("I", "e"))]
    saveSelection[, timePointYears := as.character(timePointYears)]
    SaveData(domain = "agriculture", dataset = "agriculture",
             data = saveSelection,
             normalized = FALSE)
}

## The full waste estimation, imputation process.
## ---------------------------------------------------------------------

## Build the final data set
finalLossData =
    {
        lossData <<- getLossData()
        nationalFbs <<- getNationalFbs()
        lossDataWithNationalFbs <<- mergeNationalFbs(lossData, nationalFbs)
        lossExternalData <<- getLossExternalData()
        lossFoodGroup <<- getLossFoodGroup()
        lossRegionClass <<- getLossRegionClass()
        gc()
        list(lossData = lossDataWithNationalFbs,
             lossExternalData = lossExternalData,
             lossFoodGroup = lossFoodGroup,
             lossRegionClass = lossRegionClass)
    } %>%
    with(.,
         mergeAllLossData(lossData, lossExternalData, lossFoodGroup,
                          lossRegionClass)
         )



## Build the data
trainPredictData =
    finalLossData %>%
    fillUnclassifiedFoodGroup %>%
    fillUnclassifiedRegion %>%
    calculateLossRatio %>%
    dataHack %>%
    preEstimationProcessing %>%
    selectRequiredVariable


## Estimate the model and then make the prediction
lossModel =
    trainPredictData %>%
    lossRegression

## Make imputation
imputedData =
    lossModelPrediction(model = lossModel,
                        predictionData = trainPredictData,
                        lossRatio = "lossRatio")

## Save the loss data back
SaveLossData(imputedData)
