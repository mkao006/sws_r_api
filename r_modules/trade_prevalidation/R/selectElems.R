#' Returns codes of trade elements which are satisfied query conditions
#' 
#' @param ... String to pass to filter with conditions on direction, backflow,
#' unit and unitgroup
#' @param detailed Logical, should full description of code be returned. 
#' FALSE by default.
#' 
#' @return character vector of trade elements codes if detailed is false 
#' or data.table if detailed is true.
#' @import dplyr
#' @examples
#' selectElems(direction == "in", unitgroup == "cost")
#' selectElems(direction == "in")
#' selectElems(direction == "in", !is.na(unit))



selectElems <- function(..., detailed = F) {
  
  els <- getTradeElementTable() %>%
    normalizeTradeElementTable %>%
    filter(...)     # NSE here? Can you top this? :)
  
  if(detailed) return(els)
  
  els %>%
    select_(~code) %>%
    unlist %>%      # To drop data.table
    as.vector       # To drop names
}
