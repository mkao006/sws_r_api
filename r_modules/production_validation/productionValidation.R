suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(caret)
})

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
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "5c0de0e5-0705-4ccf-a798-f7a2f2e8adc2"
        )
}


## allHistory = GetHistory(swsContext.datasets[[1]])
load("allProductionHistory.RData")
allHistory[, `:=`(c("geographicAreaM49", "measuredItemCPC",
                    "timePointYears", "flagMethod", "flagObservationStatus"),
                  list(as.factor(geographicAreaM49), 
                       as.factor(measuredItemCPC), as.numeric(timePointYears),
                       as.factor(flagMethod), as.factor(flagObservationStatus)))]
allHistory = allHistory[flagObservationStatus != "M", ]
allHistory[, valid := factor(ifelse(is.na(EndDate), "valid", "flag"))]
allHistory[valid == "valid", validValue := Value]
allHistory[, validValue := na.omit(unique(.SD[, validValue])),
           by = c("geographicAreaM49", "measuredItemCPC", "measuredElement",
               "timePointYears")]
allHistory = allHistory[!(valid == "flag" & validValue == Value), ]
allHistory[, validValue := NULL]


subHistory = allHistory[sample(NROW(allHistory), 10000), ]
## subHistory = allHistory
inTrain = createDataPartition(y = subHistory$valid, p = 0.5, list = FALSE)

training = subHistory[inTrain[, 1], ]
testing = subHistory[-inTrain[, 1], ]

## Use the default control and train boosted classification tree

control = trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

## Model (1): Boosted logistic regression
system.time(
    {
        logitBoostFit =
            train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
                  data = copy(training), method = "LogitBoost", metric = "ROC",
                  trControl = control)
    }
)

logitBoostPredicted = predict(logitBoostFit, testing)
confusionMatrix(data = logitBoostPredicted, testing$valid)


svmControl = trainControl(method = "cv", number = 5)

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

system.time({
    lssvmRadialFit =
        lssvm(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod,
              data = copy(training), kernel = "rbfdot")
})

lssvmRadialPredicted = predict(lssvmRadialFit, testing)
confusionMatrix(data = lssvmRadialPredicted, testing$valid)


## Model (3): Simple random forest
rfControl = trainControl(method = "cv", number = 5)
system.time(
    {
        rfFit =
            ## train(x = training[, list(geographicAreaM49, measuredItemCPC,
            ##                       timePointYears, flagObservationStatus,
            ##                       flagMethod)],
            ##       y = training$valid,
            train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                      flagObservationStatus + flagMethod,
                  data = copy(training), 
                  method = "rf", prox = TRUE, allowParallel = TRUE,
                  trControl = rfControl)
    }
)


rfPredicted = predict(rfFit, testing)
confusionMatrix(data = rfPredicted, testing$valid)


## Model (4): flexible discriminant analysis
fdaControl = trainControl(method = "cv", number = 5)
system.time(
    {
        fdaFit =
            train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod, data = copy(training),
                  method = "fda",  trControl = fdaControl)
    }
)


fdaPredicted = predict(fdaFit, testing)
confusionMatrix(data = fdaPredicted, testing$valid)



## Model (5): bagged MARS
bagEarthControl = trainControl(method = "cv", number = 3)
system.time(
    {
        bagEarthFit =
            train(valid ~ geographicAreaM49 + measuredItemCPC + timePointYears +
                  flagObservationStatus + flagMethod, data = copy(training),
                  method = "bagEarth",  trControl = bagEarthControl,
                  tuneGrid = data.frame(degree = 1, nprune = 20))
    }
)


bagEarthPredicted = predict(bagEarthFit, testing)
confusionMatrix(data = bagEarthPredicted, testing$valid)




## Combining the fits
valid = copy(testing)

valid[, `:=`(c("logitBoostClass", "lssvmRadialClass", "fdaClass", "rfClass",
               "bagEarthClass"),
             lapply(c("logitBoostFit", "lssvmRadialFit", "fdaFit", "rfFit",
                      "bagEarthFit"),
                    FUN = function(x) predict(get(x), newdata = .SD)))]

hackConvert = function(x){
    abs(as.numeric(x) - 2)
}

valid[, `:=`(c("validationLevel"),
             rowSums(sapply(c("logitBoostClass", "lssvmRadialClass", "fdaClass",
                              "rfClass", "bagEarthClass"),
                            FUN = function(x) hackConvert(.SD[[x]]))))]

with(valid, table(valid, validationLevel))
valid[, misclassificationRate :=
          ifelse(validationLevel == 0 & valid == "flag", 1, 0)]
table(valid$misclassificationRate)

## Actually, it is actually ok to misclassify valid value. because
## they are just value which has not been validated. The important
## thing is that of the values which were overwritten (invalid), how
## many were marked as invalid by the algorithms.


