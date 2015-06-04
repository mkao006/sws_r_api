#' Function to get trade flows for all available commodities and 
#' partners for given reporter, element and year.
#' 
#' @param item Vector of item codes from HS
#' @param element Vector of element codes
#' @param year Vecotr of years
#' @param ... Other parameters passed to getComtradeData()
#' 
#' @return data.table

getReporterFullData <- function(reporter,
                                 element = NULL,
                                 year    = NULL, ...) {
  
  getComtradeData(reporter = reporter,
                  partner  = getAllCountryCodes("partner"),
                  year     = year,
                  item     = getAllItems()$code,
                  element  = element,
                  ...)
  
}