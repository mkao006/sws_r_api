##' Food Module
##' 
##' This function takes a dataset with the appropriate variables (food,
##' population, GDP, foodFunction, elasticity, and foodPerCapita) and
##' estimates food values (and standard errors) for the next year
##' 
##' @param data A data.table object containing the food data.
##' 
##' @return
##' 

FoodModule <- function(data){
  
  ## First, sort the data by area and time.  The time sorting is important as we
  ## will later assume row i+1 is one time step past row i.
  setkeyv(data, c("geographicAreaM49", "timePointYears"))
    
  ## The funcional form 4 (originally presented in Josef's data) was replaced by
  ## functional form 3 The functional form 32 is a typo. It was replaced by
  ## functional form 3.
  data$foodFunction<-ifelse(data$foodFunction==4 | data$foodFunction==32,3,data$foodFunction)
  data[, foodHat := calculateFood(food = .SD$food, elas = .SD$elasticity,
                                  gdp_pc = .SD$GDP/.SD$population,
                                  ## We can use the first value since they're all the same:
                                  functionalForm = .SD$foodFunction[1]),
       by = c("measuredItemFS", "geographicAreaM49")]  
  
#   ## calculate calories  
#   data$hat_cal_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_cal_pc_2012*
#                                         data$hat_food_pc_2013/data$hat_food_pc_2012,0)
#   
#   ## calculate proteins 
#   data$hat_prot_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_prot_pc_2012*
#                                          data$hat_food_pc_2013/data$hat_food_pc_2012,0)
#   
#   ## calculate fats 
#   data$hat_fat_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_fat_pc_2012*
#                                         data$hat_food_pc_2013/data$hat_food_pc_2012,0)                  
    
  
  # In statistics, a forecast error is the difference between the actual or real
  # and the predicted or forecast value of a time series or any other phenomenon
  # of interest.
  # In simple cases, a forecast is compared with an outcome at a single
  # time-point and a summary of forecast errors is constructed over a collection
  # of such time-points. Here the forecast may be assessed using the difference
  # or using a proportional error.
  # By convention, the error is defined using the value of the outcome minus the
  # value of the forecast.
  data[, error := food - foodHat]
  return(output)
  
}