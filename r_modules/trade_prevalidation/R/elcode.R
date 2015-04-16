elcode <- function(...) {
  
  codes <- as.character(list(...))
  getElementCodeDesc(codes)
  
}

getElementCodeDesc <- function(codes) {
  
  getTradeElementTable() %>%
    normalizeTradeElementTable %>%
    filter(code %in% codes)
  
}