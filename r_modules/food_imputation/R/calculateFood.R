##' Calculate Food
##' 
##' @param food The food consumption at time t.
##' @param elas The elasticity of the commodity.  This will vary by country and 
##'   commodity.
##' @param gdp_pc Per person GDP (gross domestic product) at time t.
##' @param functionalForm Currently one of 0, 1, 2, or 3.  Specifies if a log-log,
##'   semi-log, or inverse-log food demand model should be used.
##'   
##' @return A numeric vector of the estimated food consumption.
##' 

calculateFood <- function(food, elas, gdp_pc, functionalForm){
    ## Data Quality Checks
    stopifnot(length(food) == length(elas))
    stopifnot(length(food) == length(gdp_pc))
    
    ## 0, 1, 2 and 3 are the functional forms linking the human food consumption
    ## and GDP.
    ## Elasticity parameter and functional forms were provided by Josef at FBS 
    ## aggregated level.
    ## Elasticity parameter and functional forms are commodity and country 
    ## dependant.
    if(functionalForm == 0){
        func = linear
    } else if(functionalForm == 1){
        func = logLog
    } else if(functionalForm == 2){
        func = semiLog
    } else if(functionalForm == 3){
        func = logInverse
    } else {
        stop("A functionalForm other than 0, 1, 2, or 3 was encountered!",
             "  This is not currently implemented.")
    }
    
    ## gdp_pc_t1 should be the same as gdp_pc but offset by 1 (i.e. one year
    ## ahead).
    N = length(gdp_pc)
    gdp_pc_t1 = c(NA, gdp_pc[-N])
    func(food_t0 = food, elas = elas, gdp_pc_t0 = gdp_pc,
         gdp_pc_t1 = gdp_pc_t1)
}