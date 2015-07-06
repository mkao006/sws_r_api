##' Functional forms
##' 
##' These three different functions provide estimates for the food consumption 
##' values in year t+1 given the consumption in year t, changes in income, and 
##' the elasticity of the particular commodity.
##' 
##' @param food_t0 The food consumption at time t.
##' @param elas The elasticity of the commodity.  This will vary by country and
##'   commodity.
##' @param gdp_pc_t0 Per person GDP (gross domestic product) at time t.
##' @param gdp_pc_t1 Per person GDP (gross domestic product) at time t+1.
##'   
##' @return An estimate for the food consumption in year t1.
##' 

## logLog is form 1
linear <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
  food_t0
    
}

## logLog is form 1
logLog <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
  food_t0 + exp(elas*log(gdp_pc_t1/gdp_pc_t0))
    
}
  
  
## semiLog is form 2
semiLog <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
  food_t0+food_t0*elas*log(gdp_pc_t1/gdp_pc_t0)
}
  
  
## logInverse is form 3
logInverse <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
  food_t0 *(exp(elas*(1-1/(gdp_pc_t1/gdp_pc_t0))))
}
