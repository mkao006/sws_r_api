##' Get Selected Seed Data
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

getSelectedSeedData = function(dataContext){
    
    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )

    selectedSeed =
        GetData(key = dataContext, normalized = FALSE, pivoting = newPivot)
    selectedSeed[, timePointYears := as.numeric(timePointYears)]
    selectedSeed
}