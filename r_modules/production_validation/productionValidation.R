suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(caret)
})

updateModel = TRUE


## Year should be a paramameter selected.
selectedYear = "2010"


## Setting up variables
standardCountryVar = "geographicAreaM49"
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"
reverseTradePrefix = "reverse_"


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "6b16fcc4-8eb6-4cec-9009-680f11d0330a"
        )
    R_SWS_SHARE_PATH = getwd()
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
    updateModel = as.logical(swsContext.computationParams$updateModel)
}


getAllHistory = function(){

    allCountries =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")[type == "country", code]

    ## Lets just test on subtree
    allItems =
        adjacent2edge(GetCodeTree(domain = "agriculture",
                                  dataset = "agriculture",
                                  dimension = "measuredItemCPC",
                                  roots = "01"))$children

    ## Only data after 1990 has history
    allYears =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "timePointYears")[description != "wildcard" ,code]
    allYears = as.numeric(allYears)
    allYears = allYears[allYears >= 1990]
    
    ## Create the new expanded keys
    newKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountries),
            Dimension(name = "measuredElement",
                      keys = c("5312", "5510", "5412")),
            Dimension(name = "measuredItemCPC",
                      keys = allItems),
            Dimension(name = yearVar,
                      keys = as.character(allYears))
            )
        )

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
        )

    allHistory = GetHistory(key = newKey)

    ## Convert time to numeric and levels to factor
    allHistory[, timePointYears := as.numeric(timePointYears)]
    allHistory[, `:=`(c("geographicAreaM49", "measuredItemCPC", "measuredElement",
                        "timePointYears", "flagMethod", "flagObservationStatus"),
                      list(factor(geographicAreaM49, levels = allCountries), 
                           factor(measuredItemCPC, level = allItems),
                           factor(measuredElement,
                                  levels = c("5312", "5510", "5412")),
                           as.numeric(timePointYears),
                           factor(flagMethod),
                           factor(flagObservationStatus)))]

    ## Remove missing observation
    allHistory = allHistory[flagObservationStatus != "M", ]

    ## create the binary response
    allHistory[, valid := factor(ifelse(is.na(EndDate), "1", "0"))]

    ## Removing duplicated values.
    allHistory[valid == "1", validValue := Value]
    allHistory[, validValue := na.omit(unique(.SD[, validValue])),
               by = c("geographicAreaM49", "measuredItemCPC", "measuredElement",
                   "timePointYears")]
    allHistory = allHistory[!(valid == "0" & validValue == Value), ]
    allHistory[, validValue := NULL]
    allHistory
}


allHistory = getAllHistory()

validationRule =
    valid ~ -1 + geographicAreaM49 + measuredItemCPC + measuredElement +
        timePointYears + flagObservationStatus + flagMethod + Value

## validationRule =
##     valid ~ geographicAreaM49 + measuredItemCPC + 
##         timePointYears + flagObservationStatus + flagMethod 


## Model paths
## ---------------------------------------------------------------------
logisticModelPath = paste0(R_SWS_SHARE_PATH, "/boostedLogisticValidationModel")
nnetModelPath = paste0(R_SWS_SHARE_PATH, "/nnetValidationModel")
rfModelPath = paste0(R_SWS_SHARE_PATH, "/rfValidationModel")
fdaModelPath = paste0(R_SWS_SHARE_PATH, "/fdaValidationModel")
plsModelPath = paste0(R_SWS_SHARE_PATH, "/plsValidationModel")


## Control parameters
## ---------------------------------------------------------------------
logisticControl = trainControl(classProbs = TRUE,
    summaryFunction = twoClassSummary)
nnetControl = trainControl()
rfControl = trainControl(method = "cv", number = 5)
fdaControl = trainControl(method = "cv", number = 5)
plsControl = trainControl(method = "cv", number = 3)



## This is for testing
## ---------------------------------------------------------------------
subHistory = allHistory[sample(NROW(allHistory), 2000), ]


inTrain = createDataPartition(y = subHistory$valid, p = 0.75, list = FALSE)
training = subHistory[inTrain[, 1], ]
testing = subHistory[-inTrain[, 1], ]

logitBoostFit =
    train(validationRule,
          data = data.frame(training), method = "LogitBoost", metric = "ROC",
          trControl = logisticControl)
logitBoostPredicted = predict(logitBoostFit, testing)
confusionMatrix(data = logitBoostPredicted, testing$valid)

nnetFit =
    train(validationRule,
          data = data.frame(training),
          method = "nnet",
          tuneGrid = expand.grid(size = 1, decay = seq(0, 10, by = 1)/1000))
nnetPredicted = predict(nnetFit, testing)
confusionMatrix(data = nnetPredicted, testing$valid)

## lssvmRadialFit =
##     lssvm(validationRule,
##           data = data.frame(training), kernel = "rbfdot")
## lssvmRadialPredicted = predict(lssvmRadialFit, testing)
## confusionMatrix(data = lssvmRadialPredicted, testing$valid)


rfFit =
    train(validationRule,
          data = data.frame(training), 
          method = "rf", prox = TRUE, allowParallel = TRUE,
          trControl = rfControl)
rfPredicted = predict(rfFit, testing)
confusionMatrix(data = rfPredicted, testing$valid)


fdaFit =
    train(validationRule, 
          data = data.frame(training),
          method = "fda",  trControl = fdaControl)
fdaPredicted = predict(fdaFit, testing)
confusionMatrix(data = fdaPredicted, testing$valid)


system.time({
plsFit =
    train(validationRule,
          data = data.frame(training),
          method = "pls", 
          tuneGrid = data.frame(ncomp = 1:10))
})
plsPredicted = predict(plsFit, testing)
confusionMatrix(data = plsPredicted, testing$valid)


system.time({
somFit =
    train(validationRule,
          data = data.frame(training),
          method = "xyf", 
          tuneGrid = data.frame(xdim = 20, ydim = 10, xweight = 0.5,
              topo = "hexagonal"))
})
somPredicted = predict(somFit, testing)
confusionMatrix(data = somPredicted, testing$valid)


system.time({
knnFit =
    train(validationRule,
          data = data.frame(training),
          method = "knn", 
          tuneGrid = data.frame(k = 100))
})
knnPredicted = predict(knnFit, testing)
confusionMatrix(data = knnPredicted, testing$valid)


bagEarthFit =
    train(validationRule,
          data = data.frame(training),
          method = "bagEarth",  trControl = bagEarthControl,
          tuneGrid = expand.grid(degree = 1,
              nprune = seq(1, length(attr(terms(validationRule), "term.labels")),
                  by = 3)))
bagEarthPredicted = predict(bagEarthFit, testing)
confusionMatrix(data = bagEarthPredicted, testing$valid)




## The actual validation estimation starts from here
## ---------------------------------------------------------------------


## Use the default control and train boosted classification tree

## Model (1): Boosted logistic regression

if(updateModel){
    logitBoostFit =
        train(validationRule,
              data = data.frame(allHistory), method = "LogitBoost", metric = "ROC",
              trControl = logisticControl)
    save(logitBoostFit, file = logisticModelPath)
} else {
    load(logisticModelPath)
}

## Model (2): Neural network

if(updateModel){
    nnetFit =
        train(validationRule,
              data = data.frame(allHistory),
              method = "nnet",
              tuneGrid = expand.grid(size = 0:2, decay = c(1:10)/1000))
    save(nnetfit, file = nnetModelPath)
} else {
    load(nnetModelPath)
}


## Model (3): Simple random forest

if(updateModel){
    rfFit =
        ## train(x = training[, list(geographicAreaM49, measuredItemCPC,
        ##                       timePointYears, flagObservationStatus,
        ##                       flagMethod)],
        ##       y = training$valid,
        train(validationRule,
              data = data.frame(allHistory), 
              method = "rf", prox = TRUE, allowParallel = TRUE,
              trControl = rfControl)
    save(rfFit, file = rfModelPath)
} else {
    load(rfModelPath)
}
    


## Model (4): flexible discriminant analysis

if(updateModel){
    fdaFit =
        train(validationRule, 
              data = data.frame(allHistory),
              method = "fda",  trControl = fdaControl)
    save(fdaFit, file = fdaModelPath)
} else {
    load(fdaModelPath)
}



## Model (5): bagged MARS

if(updateModel){
    plsFit =
        train(validationRule,
              data = data.frame(training),
              method = "pls", 
              tuneGrid = data.frame(ncomp = 1:10))    
    save(plsFit, file = plsModelPath)
} else {
    load(plsModelPath)
}

    
## swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys = c("0111", "0112")
getValidationData = function(){
    allCountries =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")[type == "country", code]    
    validationData = GetData(swsContext.datasets[[1]])
    validationData = validationData[geographicAreaM49 %in% allCountries, ]
    validationData[, `:=`(c("geographicAreaM49", "measuredItemCPC",
                            "measuredElement",
                            "timePointYears", "flagMethod",
                            "flagObservationStatus"),
                          list(factor(geographicAreaM49, levels = allCountries), 
                               factor(measuredItemCPC),
                               factor(measuredElement,
                                      levels = c("5312", "5510", "5412")),
                               as.numeric(timePointYears),
                               factor(flagMethod),
                               factor(flagObservationStatus)))]
    validationData
}


validateData = function(validationData,
    validationModel = list("logitBoostFit", "lssvmRadialFit", "fdaFit",
        "rfFit", "bagEarthFit")){
    validated = copy(validationData)

    predictedClass = sapply(validationModel, FUN = function(x) paste0(x, "Class"))

    warningSign = function(x){
        abs(as.numeric(as.character(x)) - 1)
    }
    check <<- copy(validated)
    validated[, `:=`(c(predictedClass),
                     lapply(validationModel,
                            FUN = function(x){
                                warningSign(predict(get(x, envir = .GlobalEnv),
                                                    newdata = validated))
                                }))]
    validated[, `:=`(c("Severity"),
                     rowSums(validated[, predictedClass, with = FALSE],
                             na.rm = TRUE))]
    ## Added description
    validated[Severity == 0, Description := "Value is valid"]
    validated[Severity %in% c(1:3),
              Description :=
                  paste0("Marked value is suspicious, with Severity of (",
                         Severity, ")")]
    validated[Severity %in% c(4:5),
              Description :=
                  paste0("Marked value requires revision, with Severity of (",
                         Severity, ")")]    
    ## validated[, `:=`(c(predictedClass), NULL)]
    validated
}


getValidationData() %>%
    ## The flagmethod "s" is an error, probably a test by someone.
    .[!flagMethod %in% c("s", "n"), ] %>%
    validateData(.,
                 validationModel = list("logitBoostFit", "nnetFit", "fdaFit",
                     "rfFit", "plsFit")) %>%
    SaveValidation(domain = "agriculture",
                   dataset = "agriculture",
                   validation = .)

## Actually, it is actually ok to misclassify valid value. because
## they are just value which has not been validated. The important
## thing is that of the values which were overwritten (invalid), how
## many were marked as invalid by the algorithms.

