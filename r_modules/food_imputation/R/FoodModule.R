FoodModule <- function(data){
  
  ## The funcional form 4 (originally presents in Josef's data) was replaced by functional form 3
  ## The functional form 32 is a typo. It was replaced by functional form 3.
  data$func_form<-ifelse(data$func_form==4 | data$func_form==32,3,data$func_form)
  
  
  ## f1, f2 and f3 are the functional forms linking the human food consumption and GDP. 
  ## Elasticity parameter and functional forms were provided by Josef at FBS aggregated level.
  ## Elasticity parameter and functional forms are commodity and country depending.
  
  
  ## f1 is a log - log function
  f1 <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
    food_t0 + exp(elas*log(gdp_pc_t1/gdp_pc_t0))
    
  }
  
  
  ## f2 is a semi - log function
  f2 <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
    food_t0+food_t0*elas*log(gdp_pc_t1/gdp_pc_t0)
  }
  
  
  ## f3 is a log - inverse function
  f3 <- function(food_t0,elas,gdp_pc_t0,gdp_pc_t1){
    
    food_t0 *(exp(elas*(1-1/(gdp_pc_t1/gdp_pc_t0))))
  }
  
  
  
  
  for(i in 1:dim(data)[1]){
    
    
    ## calculate the values if the function is linear
    if(data$func_form[i] == 0) {
      
      data$hat_food_pc_2008[i] <- data$food_pc_2007[i] 
      
      data$hat_food_pc_2009[i] <- data$food_pc_2008[i] 
      
      data$hat_food_pc_2010[i] <- data$food_pc_2009[i] 
      
      data$hat_food_pc_2011[i] <- data$food_pc_2010[i] 
      
      data$hat_food_pc_2012[i] <- data$food_pc_2011[i]
  
      data$hat_food_pc_2013[i] <- data$hat_food_pc_2012[i]
      
    }
    
    
    ## calculate values if function is log - log function
    else if (data$func_form[i] == 1) {
      
      
      data$hat_food_pc_2008[i] <- f1(data$food_pc_2007[i],data$elas[i],
                                          data$gdp_pc_2007[i],data$gdp_pc_2008[i])
      
      data$hat_food_pc_2009[i] <- f1(data$food_pc_2008[i],data$elas[i],
                                          data$gdp_pc_2008[i],data$gdp_pc_2009[i])
      
      data$hat_food_pc_2010[i] <- f1(data$food_pc_2009[i],data$elas[i],
                                          data$gdp_pc_2009[i],data$gdp_pc_2010[i]) 
      
      data$hat_food_pc_2011[i] <- f1(data$food_pc_2010[i],data$elas[i],
                                          data$gdp_pc_2010[i],data$gdp_pc_2011[i]) 
      
      data$hat_food_pc_2012[i] <- f1(data$food_pc_2011[i],data$elas[i],
                                          data$gdp_pc_2011[i],data$gdp_pc_2012[i])
      
      data$hat_food_pc_2013[i] <- f1(data$hat_food_pc_2012[i],data$elas[i],
                                          data$gdp_pc_2012[i],data$gdp_pc_2013[i])
      
    }
    
    ## calculate values if function is semi-log 
    else if (data$func_form[i] == 2) {
      
      data$hat_food_pc_2008[i] <- f2(data$food_pc_2007[i],data$elas[i],
                                          data$gdp_pc_2007[i],data$gdp_pc_2008[i])
      
      data$hat_food_pc_2009[i] <- f2(data$food_pc_2008[i],data$elas[i],
                                          data$gdp_pc_2008[i],data$gdp_pc_2009[i])
      
      data$hat_food_pc_2010[i] <- f2(data$food_pc_2009[i],data$elas[i],
                                          data$gdp_pc_2009[i],data$gdp_pc_2010[i]) 
      
      data$hat_food_pc_2011[i] <- f2(data$food_pc_2010[i],data$elas[i],
                                          data$gdp_pc_2010[i],data$gdp_pc_2011[i]) 
      
      data$hat_food_pc_2012[i] <- f2(data$food_pc_2011[i],data$elas[i],
                                          data$gdp_pc_2011[i],data$gdp_pc_2012[i])
      
      data$hat_food_pc_2013[i] <- f2(data$hat_food_pc_2012[i],data$elas[i],
                                          data$gdp_pc_2012[i],data$gdp_pc_2013[i])
    }
    
    ## calculate values if function is log-inverse   
    else if (data$func_form[i] == 3) {
      
      data$hat_food_pc_2008[i] <- f3(data$food_pc_2007[i],data$elas[i],
                                          data$gdp_pc_2007[i],data$gdp_pc_2008[i])
      
      data$hat_food_pc_2009[i] <- f3(data$food_pc_2008[i],data$elas[i],
                                          data$gdp_pc_2008[i],data$gdp_pc_2009[i])
      
      data$hat_food_pc_2010[i] <- f3(data$food_pc_2009[i],data$elas[i],
                                          data$gdp_pc_2009[i],data$gdp_pc_2010[i]) 
      
      data$hat_food_pc_2011[i] <- f3(data$food_pc_2010[i],data$elas[i],
                                          data$gdp_pc_2010[i],data$gdp_pc_2011[i]) 
      
      data$hat_food_pc_2012[i] <- f3(data$food_pc_2011[i],data$elas[i],
                                          data$gdp_pc_2011[i],data$gdp_pc_2012[i])
      
      data$hat_food_pc_2013[i] <- f3(data$hat_food_pc_2012[i],data$elas[i],
                                          data$gdp_pc_2012[i],data$gdp_pc_2013[i])
      
    }
  }
  
  
  ## calculate food (t)  
  
  data$hat_food_2008 <- data$hat_food_pc_2008 * data$pop_2008
  
  data$hat_food_2009 <- data$hat_food_pc_2009 * data$pop_2009
  
  data$hat_food_2010 <- data$hat_food_pc_2010 * data$pop_2010
  
  data$hat_food_2011 <- data$hat_food_pc_2011 * data$pop_2011
  
  data$hat_food_2012 <- data$hat_food_pc_2012 * data$pop_2012
  
  data$hat_food_2013 <- data$hat_food_pc_2013 * data$pop_2013
  
  
  ## calculate calories
  data$hat_cal_pc_2008 <- ifelse(data$food_pc_2007 > 0, data$cal_pc_2007*
                                        data$hat_food_pc_2008/data$food_pc_2007,0)
  
  data$hat_cal_pc_2009 <- ifelse(data$hat_food_pc_2008 > 0, data$hat_cal_pc_2008*
                                        data$hat_food_pc_2009/data$hat_food_pc_2008,0)
  
  data$hat_cal_pc_2010 <- ifelse(data$food_pc_2009 > 0, data$cal_pc_2009*
                                        data$hat_food_pc_2010/data$food_pc_2009,0)
  
  data$hat_cal_pc_2011 <- ifelse(data$food_pc_2010 > 0, as.numeric(data$cal_pc_2010)*
                                        data$hat_food_pc_2011/data$food_pc_2010,0)
  
  data$hat_cal_pc_2012 <- ifelse(data$food_pc_2011 > 0, as.numeric(data$cal_pc_2011)*
                                        data$hat_food_pc_2012/data$food_pc_2011,0)
  
  data$hat_cal_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_cal_pc_2012*
                                        data$hat_food_pc_2013/data$hat_food_pc_2012,0)
  
  
  ## calculate proteins 
  data$hat_prot_pc_2008 <- ifelse(data$food_pc_2007 > 0, data$prot_pc_2007*
                                         data$hat_food_pc_2008/data$food_pc_2007,0)
  
  data$hat_prot_pc_2009 <- ifelse(data$hat_food_pc_2008 > 0, data$hat_prot_pc_2008*
                                         data$hat_food_pc_2009/data$hat_food_pc_2008,0)
  
  data$hat_prot_pc_2010 <- ifelse(data$food_pc_2009 > 0, data$prot_pc_2009*
                                         data$hat_food_pc_2010/data$food_pc_2009,0)
  
  data$hat_prot_pc_2011 <- ifelse(data$hat_food_pc_2010 > 0, data$hat_prot_pc_2010*
                                         data$hat_food_pc_2011/data$hat_food_pc_2010,0)
  
  data$hat_prot_pc_2012 <- ifelse(data$food_pc_2011 > 0, data$prot_pc_2011*
                                         data$hat_food_pc_2012/data$food_pc_2011,0)
  
  data$hat_prot_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_prot_pc_2012*
                                         data$hat_food_pc_2013/data$hat_food_pc_2012,0)
  
  ## calculate fats 
  data$hat_fat_pc_2008 <- ifelse(data$food_pc_2007 > 0, data$fat_pc_2007*
                                        data$hat_food_pc_2008/data$food_pc_2007,0)
  
  data$hat_fat_pc_2009 <- ifelse(data$hat_food_pc_2008 > 0, data$hat_fat_pc_2008*
                                        data$hat_food_pc_2008/data$hat_food_pc_2009,0)
  
  
  data$hat_fat_pc_2010 <- ifelse(data$food_pc_2009 > 0, data$fat_pc_2009*
                                        data$hat_food_pc_2010/data$food_pc_2009,0)
  
  data$hat_fat_pc_2011 <- ifelse(data$hat_food_pc_2010 > 0, data$hat_fat_pc_2010*
                                        data$hat_food_pc_2011/data$hat_food_pc_2010,0)
  
  data$hat_fat_pc_2012 <- ifelse(data$food_pc_2011 > 0, data$fat_pc_2011*
                                        data$hat_food_pc_2012/data$food_pc_2011,0)
  
  data$hat_fat_pc_2013 <- ifelse(data$hat_food_pc_2012 > 0, data$hat_fat_pc_2012*
                                        data$hat_food_pc_2013/data$hat_food_pc_2012,0)
  
  
  # Expected Value for food (expressed in t, kcal, prot, fat) pc in 2012 and 2013
  
  ExpValue<- data[,c("M49_cod","com_sua_cod","com_cod",
                     "food_pc_2011","hat_food_pc_2012","hat_food_pc_2013",
                     "cal_pc_2011","hat_cal_pc_2012","hat_cal_pc_2013",
                     "prot_pc_2011","hat_prot_pc_2012","hat_prot_pc_2013",
                     "fat_pc_2011","hat_fat_pc_2012","hat_fat_pc_2013")]
                  
    
  
  # In statistics, a forecast error is the difference between the actual or real and the predicted or forecast value 
  # of a time series or any other phenomenon of interest.
  # In simple cases, a forecast is compared with an outcome at a single time-point and a summary of forecast errors 
  # is constructed over a collection of such time-points. Here the forecast may be assessed using the difference or 
  # using a proportional error. 
  # By convention, the error is defined using the value of the outcome minus the value of the forecast.
  
  delta_food_pc <- cbind(data$food_pc_2008-data$hat_food_pc_2008,
                         data$food_pc_2009-data$hat_food_pc_2009,
                         data$food_pc_2010-data$hat_food_pc_2010,
                         data$food_pc_2011-data$hat_food_pc_2011)
  data$res_mean_food_pc <- apply(delta_food_pc,1,mean)
  data$res_sd_food_pc <- apply(delta_food_pc,1,sd)
  
  
  delta_cal_pc <- cbind(data$cal_pc_2008-data$hat_cal_pc_2008,
                        data$cal_pc_2009-data$hat_cal_pc_2009,
                        data$cal_pc_2010-data$hat_cal_pc_2010,
                        data$cal_pc_2011-data$hat_cal_pc_2011)
  data$res_mean_cal_pc <- apply(delta_cal_pc,1,mean)
  data$res_sd_cal_pc <- apply(delta_cal_pc,1,sd)
  
  delta_prot_pc <- cbind(data$prot_pc_2008-data$hat_prot_pc_2008,
                         data$prot_pc_2009-data$hat_prot_pc_2009,
                         data$prot_pc_2010-data$hat_prot_pc_2010,
                         data$prot_pc_2011-data$hat_prot_pc_2011)
  data$res_mean_prot_pc <- apply(delta_prot_pc,1,mean)
  data$res_sd_prot_pc <- apply(delta_prot_pc,1,sd)
  
  delta_fat_pc <- cbind(data$fat_pc_2008-data$hat_fat_pc_2008,
                        data$fat_pc_2009-data$hat_fat_pc_2009,
                        data$fat_pc_2010-data$hat_fat_pc_2010,
                        data$fat_pc_2011-data$hat_fat_pc_2011)
  data$res_mean_fat_pc <- apply(delta_fat_pc,1,mean)
  data$res_sd_fat_pc <- apply(delta_fat_pc,1,sd)
  
  
  StandDev <- data[,c("M49_cod","com_sua_cod","com_cod",
                      "res_mean_food_pc","res_sd_food_pc",
                      "res_mean_cal_pc","res_sd_cal_pc",
                      "res_mean_prot_pc","res_sd_prot_pc",
                      "res_mean_fat_pc","res_sd_fat_pc")]
  
  
  output <- list(data,ExpValue,StandDev)
  names(output) <- c("data","ExpValue","StandDev") 
  return(output)
  
}