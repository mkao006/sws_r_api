#' Returns codes of trade elements which are satisfied query conditions
#' 
#' @param ... String to pass to filter with conditions on direction, backflow,
#' unit and unitgroup
#' 
#' @return character vector of trade elements codes.
#' @import dplyr
#' @examples
#' elems(direction == "in", unitgroup == "cost")
#' elems(direction == "in")
#' elems(direction == "in", !is.na(unit))



elems <- function(...) {
  
  getTradeElementTable() %>%
    normalizeTradeElementTable %>%
    filter(...) %>% # NSE here? Can you top this? :)
    select_(~code) %>%
    unlist %>%      # To drop data.table
    as.vector       # To drop names
}
