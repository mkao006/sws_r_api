##' Validate Data
##' 
##' @param data A data.table object containing the data to be validated.
##' @param valueColumnName The column name of data which contains the value
##' to be validated.
##' @param byKey A vector of column names which should be used to specify
##' different slices of the data.  Each unique byKey will be validated
##' separately.  Moreover, for models with independent variables (such as
##' basicLmTest) time is assumed to go from 1 to the number of observations,
##' and this won't be valid if an incorrect byKey is provided.
##' @param validationModels A list of functions which should be used to
##' validate the data.  The functions should only require one argument: y. To
##' update the default arguments from the basic models, you may need to write
##' some small wrapper functions (see example).
##' 
##' @return A numerical vector of the same length as nrow(data).  The number
##' represents the number of models which identified that particular row of
##' data as a potential outlier.
##' 
##' @example
##' d = data.table(y = rnorm(20), grp = rep(1:2, each = 10))
##' intervalTest = function(y){
##'     basicIntervalTest(y = y, upper = 1.5, lower = -1.5)
##' }
##' lmTest = function(y){
##'     basicLmTest(y = y, robust = FALSE)
##' }
##' validateData(data = d, valueColumnName = "y", byKey = "grp",
##'              validationModels = list(basicMeanTest, lmTest, intervalTest))
##' 
##' @export
##' 

validateData = function(data, valueColumnName, byKey, validationModels){

    ## Data Quality Checks
    stopifnot(valueColumnName %in% colnames(data))
    stopifnot(byKey %in% colnames(data))

    localData = copy(data)
    ## Initialize columns to numeric zeros:
    localData[, flaggedCounts := numeric(nrow(localData))]
    localData[, successfulTests := numeric(nrow(localData))]
    sapply(validationModels, function(fun){
        localData[, testResult := fun(get(valueColumnName)), by = byKey]
        localData[, flaggedCounts := flaggedCounts +
                       ifelse(is.na(testResult), 0, testResult), by = byKey]
        localData[, successfulTests := successfulTests +
                       as.numeric(!is.na(testResult)), by = byKey]
    })
    localData[, testResult := NULL]
    localData
}