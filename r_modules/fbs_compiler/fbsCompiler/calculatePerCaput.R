##' Function to calculate the Per Caput value or per capita per day.
##'
##' @param data The data.table
##' @param populationVar The column name corresponds to population
##' @param valueColumns The column names for which the per caput value
##' will be calculated.
##'
##' @return A data.table containing only the specified columns with
##' the value being the caput of the inputs.

calculatePerCaput = function(data, populationVar, valueColumns){
    if(missing(valueColumns))
        valueColumns = grep("Value", colnames(data), value = TRUE)
    dataCopy = copy(data)    
    dataCopy[, `:=`(c(valueColumns),
                    lapply(valueColumns,
                           FUN = function(x){
                               computeRatio(.SD[[x]],
                                            .SD[[populationVar]] * 365)
                           }))]
    dataCopy
}
