#' Function to get trade flows for all available reporters and 
#' partners for given item, element and year.
#' 
#' @param item Vector of item codes from HS
#' @param element Vector of element codes
#' @param year Vecotr of years
#' @param ... Other parameters passed to getComtradeData()
#' 
#' @return data.table

getCommodityFullData <- function(item    = NULL, 
                                 element = NULL,
                                 year    = NULL, ...) {
  
  getComtradeData(reporter = getAllCountryCodes("reporter"),
                  partner  = getAllCountryCodes("partner"),
                  year     = year,
                  item     = item,
                  element  = element,
                  ...)
  
}