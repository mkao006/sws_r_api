#' Detects unit's dimension
#' 
#' Dimensions are head, weight, volume, cost (of flow), price (of unit).
#' 
#' @param unit Character vector with unit values from table of trade elements
#' 
#' @value Factor with dimensions

groupUnits <- function(unit) {
  
  head   <- c("head", "1000 head")
  weight <- c("t", "1000 t", "kg")
  volume <- c("l", "m3")
  cost   <- c("$", "1000 $", "US$")
  price  <- c("$/t", "$/m3", "$/head")
  
  unitgroup <- ifelse(unit %in% head, "head",
                      ifelse(unit %in% weight, "weight",
                             ifelse(unit %in% volume, "volume",
                                    ifelse(unit %in% cost, "cost",
                                           ifelse(unit %in% price, "price",
                                                  NA)))))
  
  factor(unitgroup, levels = c("head", "weight", "volume", "cost", "price"))
  
}