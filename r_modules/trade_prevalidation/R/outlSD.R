outlSD <- function(data, yearVar = "year", valueVar = "value") {
  
  calcs <- list(lazyeval::interp(~abs(sd(valueVar, na.rm = T) -
                                        mean(valueVar, na.rm = T)) < valueVar,
                                 valueVar = as.name(valueVar)))
  
  calcs <- setNames(calcs, "isoutl")
  
  isOutl <- data %>%
    mutate_(.dots = calcs)
  
  isOutl$isoutl
}