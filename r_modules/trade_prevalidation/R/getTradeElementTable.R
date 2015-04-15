#' Get table with trade elements
#' 

getTradeElementTable <- function(domain  = "trade",
                                 dataset = "ct_raw_tf",
                                 elementVar = "measuredElementTrade") {
  
  faosws::GetCodeList(domain = domain,
                      dataset =  dataset,
                      dimension = elementVar)
}