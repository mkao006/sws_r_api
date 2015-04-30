##' Function to compute the calorie equivalence
##'
##' @param data The data table
##' @param quantityVariable The column name corresponding to the
##' quantity to be converted to calorie.
##' @param calorieVariable The column corresponding to the calorie
##' conversion factors.
##' @param quantityToTonMultiplier A scalar which converts the unit of
##' measure for quantity to tonnes.
##' @param calorieToTonMultiplier A scalar which converts the unit of
##' measure for the calorie conversion factor to per tonnes.
##' @param outputName The column name to be assigned for the
##' calculated valeus.
##'
##' @return A new data table with a new column representing the
##' calculated calorie.

computeCalorie = function(data, quantityVariable, calorieVariable,
    quantityToTonMultiplier, calorieToTonMultiplier, outputName){
    dataCopy = copy(data)
    if(NROW(dataCopy) > 0)
        dataCopy[, `:=`(c(outputName),
                   list(get(quantityVariable) * quantityToTonMultiplier *
                            get(calorieVariable) * calorieToTonMultiplier))]
    dataCopy
}
