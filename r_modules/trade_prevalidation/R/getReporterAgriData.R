#' Function to get trade flows for agricultural commodities and 
#' partners for given reporter, element and year.
#' 
#' 
#' @param reporter M49 code of reporter area. 
#' @param element Vector of element codes
#' @param year Vecotr of years
#' @param ... Other parameters passed to getComtradeData()
#' 
#' @return data.table

getReporterAgriData <- function(reporter,
       element = NULL,
       year    = NULL, ...) {
  
  
  getComtradeData(reporter = reporter,
                  partner  = getAllCountryCodes("partner"),
                  year     = year,
                  item     = getAgriHSCodes(),
                  element  = element,
                  ...)
  
}