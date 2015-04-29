#' Get table with trade elements
#' 

getTradeElementTable <- function(domain  = "trade",
                                 dataset = "ct_raw_tf",
                                 elementVar = "measuredElementTrade") {
  
  if(!is.SWSEnvir()) stop("No SWS environment detected.")
  
  faosws::GetCodeList(domain = domain,
                      dataset =  dataset,
                      dimension = elementVar)
}