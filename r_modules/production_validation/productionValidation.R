suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(caret)
})

updateModel = FALSE


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
        token = "5c0de0e5-0705-4ccf-a798-f7a2f2e8adc2"
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
                      keys = "5510"),
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
    allHistory[, timePointYears := as.numeric(timePointYears)]
    allHistory[, `:=`(c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears", "flagMethod", "flagObservationStatus"),
                      list(factor(geographicAreaM49, levels = allCountries), 
                           factor(measuredItemCPC, level = allItems),
                           as.numeric(timePointYears),
                           factor(flagMethod),
                           factor(flagObservationStatus)))]
    allHistory = allHistory[flagObservationStatus != "M", ]
    ## There are no history before 1990
    allHistory = allHistory[timePointYears >= 1900, ]
    allHistory[, valid := factor(ifelse(is.na(EndDate), "1", "0"))]
    allHistory[valid == "1", validValue := Value]
    allHistory[, validValue := na.omit(unique(.SD[, validValue])),
               by = c("geographicAreaM49", "measuredItemCPC", "measuredElement",
                   "timePointYears")]
    allHistory = allHistory[!(valid == "0" & validValue == Value), ]
    allHistory[, validValue := NULL]
    allHistory
}


allHistory = getAllHistory()
allHistory = allHistory[sample(NROW(allHistory), 300), ]


## inTrain = createDataPartition(y = allHistory$valid, p = 0.75, list = FALSE)
## training = allHistory[inTrain[, 1], ]
## testing = allHistory[-inTrain[, 1], ]


## Use the default control and train boosted classification tree

logisticControl = trainControl(classProbs = TRUE,
    summaryFunction = twoClassSummary)

## Model (1): Boosted logistic regression
logisticModelPath = paste0(R_SWS_SHARE_PATH, "/boostedLogisticValidationModel")
if(updateModel){
    logitBoostFit =
        train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
              data = data.frame(allHistory), method = "LogitBoost", metric = "ROC",
              trControl = logisticControl)
    save(logitBoostFit, file = logisticModelPath)
} else {
    load(logisticModelPath)
}

## logitBoostPredicted = predict(logitBoostFit, testing)
## confusionMatrix(data = logitBoostPredicted, testing$valid)

## Model (2): least squares svm radial basis function kernel 
svmControl = trainControl(method = "cv", number = 5)
lssvmModelPath = paste0(R_SWS_SHARE_PATH, "/lssvmValidationModel")

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
        lssvm(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
              data = data.frame(allHistory), kernel = "rbfdot")
    save(logitBoostFit, file = lssvmModelPath)
} else {
    load(lssvmModelPath)
}

## lssvmRadialPredicted = predict(lssvmRadialFit, testing)
## confusionMatrix(data = lssvmRadialPredicted, testing$valid)


## Model (3): Simple random forest
rfControl = trainControl(method = "cv", number = 5)
rfModelPath = paste0(R_SWS_SHARE_PATH, "/rfValidationModel")

if(updateModel){
    rfFit =
        ## train(x = training[, list(geographicAreaM49, measuredItemCPC,
        ##                       timePointYears, flagObservationStatus,
        ##                       flagMethod)],
        ##       y = training$valid,
        train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
              data = data.frame(allHistory), 
              method = "rf", prox = TRUE, allowParallel = TRUE,
              trControl = rfControl)
    save(rfFit, file = rfModelPath)
} else {
    load(rfModelPath)
}
    

## rfPredicted = predict(rfFit, testing)
## confusionMatrix(data = rfPredicted, testing$valid)


## Model (4): flexible discriminant analysis
fdaModelPath = paste0(R_SWS_SHARE_PATH, "/fdaValidationModel")
fdaControl = trainControl(method = "cv", number = 5)

if(updateModel){
    fdaFit =
        train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
              data = data.frame(allHistory),
              method = "fda",  trControl = fdaControl)
    save(fdaFit, file = fdaModelPath)
} else {
    load(fdaModelPath)
}

## fdaPredicted = predict(fdaFit, testing)
## confusionMatrix(data = fdaPredicted, testing$valid)



## Model (5): bagged MARS
bagEarthModelPath = paste0(R_SWS_SHARE_PATH, "/bagEarthValidationModel")
bagEarthControl = trainControl(method = "cv", number = 3)

if(updateModel){
    bagEarthFit =
        train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
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
                            "timePointYears", "flagMethod",
                            "flagObservationStatus"),
                          list(factor(geographicAreaM49, levels = allCountries), 
                               factor(measuredItemCPC), as.numeric(timePointYears),
                               factor(flagMethod),
                               factor(flagObservationStatus)))]
    validationData
}


validateData = function(validationData,
    validationModel = list("logitBoostFit", "lssvmRadialFit", "fdaFit",
        "rfFit", "bagEarthFit")){
    validated = copy(validationData)
    validated[, `:=`(c("logitBoostClass", "lssvmRadialClass", "fdaClass",
                       "rfClass", "bagEarthClass"),
                     lapply(validationModel,
                            FUN = function(x) predict(get(x), newdata = .SD)))]
    
    warningSign = function(x){
        abs(as.numeric(as.character(x)) - 1)
    }

    validated[, `:=`(c("Severity"),
                     rowSums(sapply(c("logitBoostClass", "lssvmRadialClass",
                                      "fdaClass", "rfClass", "bagEarthClass"),
                                FUN = function(x) warningSign(.SD[[x]]))))]
    validated[Severity == 0, Description := "Value is valid"]
    validated[Severity %in% c(1:3),
              Description :=
                  paste0("Marked value is suspicious, with Severity of (",
                         Severity, ")")]
    validated[Severity %in% c(4:5),
              Description :=
                  paste0("Marked value requires revision, with Severity of (",
                         Severity, ")")]    
    validated[, `:=`(c("logitBoostClass", "lssvmRadialClass", "fdaClass",
                       "rfClass", "bagEarthClass"), NULL)]
    validated
}


## validationData = getValidationData()
## validationData = validationData[flagMethod != "s", ]
## validationData[, flagMethod := factor(flagMethod)]
## validatedData =
##     validateData(validationData,
##                  validationModel = list("logitBoostFit", "fdaFit",
##                      "rfFit", "bagEarthFit"))

getValidationData() %>%
    .[flagMethod != "s", ] %>%
    .[, flagMethod := factor(flagMethod)] %>%
    validateData(.,
                 validationModel = list("logitBoostFit", "fdaFit",
                     "rfFit", "bagEarthFit")) %>%
    SaveValidation(domain = "agriculture",
                   dataset = "agriculture",
                   validation = .)

## SaveValidation(domain = "agriculture",
##                dataset = "agriculture",
##                validation = validatedData[1:5, ])


## with(valid, table(valid, validationLevel))
## valid[, misclassificationRate :=
##           ifelse(validationLevel == 0 & valid == "0", 1, 0)]
## table(valid$misclassificationRate)

## Actually, it is actually ok to misclassify valid value. because
## they are just value which has not been validated. The important
## thing is that of the values which were overwritten (invalid), how
## many were marked as invalid by the algorithms.


