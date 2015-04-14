suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(caret)
})

updateModel = TRUE
testing = FALSE

## Year should be a paramameter selected.
selectedYear = "2010"


## Setting up variables
standardCountryVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    ## Define directories
    apiDirectory = "~/Documents/Github/sws_r_api/r_modules/production_validation/faoswsProduction/"
    packageDirectory = "~/Documents/Github/sws_production/faoswsProduction/R/"
    
    ## Get SWS Parameters
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "58bfac1d-04cb-4150-856d-e0d10aec7b22"
    )
    R_SWS_SHARE_PATH = paste0(apiDirectory, "/..")

    ## Copy over scripts from package directory
    unlink(apiDirectory)
    file.copy(from = dir(packageDirectory, pattern = ".*\\.R$",
                         full.names = TRUE),
              to = apiDirectory)

    ## Source copied scripts for this local test
    for(file in dir(apiDirectory, full.names = T))
        source(file)
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/browningj/production/validationModels"
    updateModel = as.logical(swsContext.computationParams$updateModel)
}

allHistory = getAllHistory()

# validationRule =
#     valid ~ -1 + geographicAreaM49 + measuredItemCPC + measuredElement +
#         timePointYears + flagObservationStatus + flagMethod + Value +
#             productionValue

validationRule =
    valid ~ -1 + geographicAreaM49 + measuredItemCPC + measuredElement +
        Value + productionValue


## Model paths
## ---------------------------------------------------------------------
logisticModelPath = paste0(R_SWS_SHARE_PATH, "/boostedLogisticValidationModel")
nnetModelPath = paste0(R_SWS_SHARE_PATH, "/nnetValidationModel")
rfModelPath = paste0(R_SWS_SHARE_PATH, "/rfValidationModel")
fdaModelPath = paste0(R_SWS_SHARE_PATH, "/fdaValidationModel")
plsModelPath = paste0(R_SWS_SHARE_PATH, "/plsValidationModel")


## Control parameters
## ---------------------------------------------------------------------

validationClassError = function(data, leve = NULL, model = NULL){
    n.missClassified = NROW(data[data$obs == 0 & data$pred == 1, ])
    n.correctClassified = NROW(data[data$obs == data$pred, ])
    out = c(n.missClassified/(n.missClassified + n.correctClassified),
        n.missClassified)
    names(out) = c("vce", "nm")
    out
}

logisticControl = trainControl(classProbs = TRUE)
nnetControl = trainControl(method = "boot", number = 25)
rfControl = trainControl(method = "boot", number = 10)
fdaControl = trainControl(method = "boot", number = 10)
plsControl = trainControl(method = "boot", number = 10)
bagEarthControl = trainControl(method = "boot", number = 10)



## This is for testing
## ---------------------------------------------------------------------

if(testing){
    subHistory = allHistory[sample(NROW(allHistory), 1000), ]

    inTrain = createDataPartition(y = subHistory$valid, p = 0.75, list = FALSE)
    training = subHistory[inTrain[, 1], ]
    testing = subHistory[-inTrain[, 1], ]

    logitBoostFit =
        train(validationRule,
              data = data.frame(training), method = "LogitBoost",
              tuneGrid = data.frame(nIter = 1:50),
              trControl = logisticControl)
    logitBoostPredicted = predict(logitBoostFit, testing)
    confusionMatrix(data = logitBoostPredicted, testing$valid)
    ## 42 correct, 33 incorrect, 174 NA's

    nnetFit =
        train(validationRule,
              data = data.frame(training),
              method = "nnet",
              trControl = nnetControl,
              tuneGrid = expand.grid(size = 1,
                  decay = 1/seq(1000, 10000, length = 10)))
    nnetPredicted = predict(nnetFit, testing)
    confusionMatrix(data = nnetPredicted, testing$valid)
    ## 130 correct, 119 incorrect (almost all "N")

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
    ## 125 correct, 124 incorrect

    fdaFit =
        train(validationRule, 
              data = data.frame(training),
              method = "fda",  trControl = fdaControl)
    fdaPredicted = predict(fdaFit, testing)
    confusionMatrix(data = fdaPredicted, testing$valid)
    ## 127 correct, 122 incorrect

    system.time({
        plsFit =
            train(validationRule,
                  data = data.frame(training),
                  method = "pls", 
                  tuneGrid = data.frame(ncomp = 1:10))
    })
    plsPredicted = predict(plsFit, testing)
    confusionMatrix(data = plsPredicted, testing$valid)
    ## 135 correct, 114 incorrect

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
    ## 122 correct, 127 incorrect

    bagEarthFit =
        train(validationRule,
              data = data.frame(training),
              method = "bagEarth",  trControl = bagEarthControl,
              tuneGrid = expand.grid(degree = 1,
                  nprune = seq(1,
                      length(attr(terms(validationRule), "term.labels")),
                      by = 3)))
    bagEarthPredicted = predict(bagEarthFit, testing)
    confusionMatrix(data = bagEarthPredicted, testing$valid)

}


## The actual validation estimation starts from here
## ---------------------------------------------------------------------

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
    validated[Severity %in% c(1:2),
              Description :=
                  paste0("Marked value is suspicious, with Severity of (",
                         Severity, ")")]
    validated[Severity %in% c(3:4),
              Description :=
                  paste0("Marked value requires verification, with Severity of (",
                         Severity, ")")]
    validated[Severity %in% c(5),
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

