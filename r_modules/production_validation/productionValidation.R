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
                      ## keys = c("2009", "2010"))
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
    valid ~ geographicAreaM49 + measuredItemCPC + measuredElement +
        timePointYears + flagObservationStatus + flagMethod + Value

## validationRule =
##     valid ~ geographicAreaM49 + measuredItemCPC + 
##         timePointYears + flagObservationStatus + flagMethod 


## Model paths
## ---------------------------------------------------------------------
logisticModelPath = paste0(R_SWS_SHARE_PATH, "/boostedLogisticValidationModel")
lssvmModelPath = paste0(R_SWS_SHARE_PATH, "/lssvmValidationModel")
rfModelPath = paste0(R_SWS_SHARE_PATH, "/rfValidationModel")
fdaModelPath = paste0(R_SWS_SHARE_PATH, "/fdaValidationModel")
bagEarthModelPath = paste0(R_SWS_SHARE_PATH, "/bagEarthValidationModel")


## Control parameters
## ---------------------------------------------------------------------
logisticControl = trainControl(classProbs = TRUE,
    summaryFunction = twoClassSummary)
svmControl = trainControl(method = "cv", number = 5)
rfControl = trainControl(method = "cv", number = 5)
fdaControl = trainControl(method = "cv", number = 5)
bagEarthControl = trainControl(method = "cv", number = 3)



## ## This is for testing
## ## ---------------------------------------------------------------------
## subHistory = allHistory[sample(NROW(allHistory), 1250), ]


## inTrain = createDataPartition(y = subHistory$valid, p = 0.75, list = FALSE)
## training = subHistory[inTrain[, 1], ]
## testing = subHistory[-inTrain[, 1], ]

## logitBoostFit =
##     train(validationRule,
##           data = data.frame(training), method = "LogitBoost", metric = "ROC",
##           trControl = logisticControl)
## logitBoostPredicted = predict(logitBoostFit, testing)
## confusionMatrix(data = logitBoostPredicted, testing$valid)

## lssvmRadialFit =
##     lssvm(validationRule,
##           data = data.frame(training), kernel = "rbfdot")
## lssvmRadialPredicted = predict(lssvmRadialFit, testing)
## confusionMatrix(data = lssvmRadialPredicted, testing$valid)


## rfFit =
##     train(validationRule,
##           data = data.frame(training), 
##           method = "rf", prox = TRUE, allowParallel = TRUE,
##           trControl = rfControl)
## rfPredicted = predict(rfFit, testing)
## confusionMatrix(data = rfPredicted, testing$valid)


## fdaFit =
##     train(validationRule, 
##           data = data.frame(training),
##           method = "fda",  trControl = fdaControl)
## fdaPredicted = predict(fdaFit, testing)
## confusionMatrix(data = fdaPredicted, testing$valid)


bagEarthFit =
    train(validationRule,
          data = data.frame(training),
          method = "bagEarth",  trControl = bagEarthControl,
          tuneGrid = data.frame(degree = 1, nprune = 20))
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

## Model (2): least squares svm radial basis function kernel 

## system.time(
##     {
##         lssvmRadialFit =
##             train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
##                   flagObservationStatus + flagMethod,
##                   data = copy(training), method = "lssvmLinear",
##                   trControl = svmControl)
##     }
## )

if(updateModel){
    library(kernlab)
    lssvmRadialFit =
        lssvm(validationRule,
              data = data.frame(allHistory), kernel = "rbfdot")
    save(logitBoostFit, file = lssvmModelPath)
} else {
    load(lssvmModelPath)
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
    bagEarthFit =
        train(validationRule,
              data = data.frame(allHistory),
              method = "bagEarth",  trControl = bagEarthControl,
              tuneGrid = data.frame(degree = 1, nprune = 20))
    save(bagEarthFit, file = bagEarthModelPath)
} else {
    load(bagEarthModelPath)
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
    validated[, `:=`(c(predictedClass), NULL)]
    validated
}


getValidationData() %>%
    .[!flagMethod %in% c("s", "n"), ] %>%
    .[, flagMethod := factor(flagMethod)] %>%
    validateData(.,
                 validationModel = list("logitBoostFit", "fdaFit",
                     "rfFit", "bagEarthFit")) %>%
    SaveValidation(domain = "agriculture",
                   dataset = "agriculture",
                   validation = .)

## NOTE (Michael): Need to watch out for new levels.

## Actually, it is actually ok to misclassify valid value. because
## they are just value which has not been validated. The important
## thing is that of the values which were overwritten (invalid), how
## many were marked as invalid by the algorithms.


