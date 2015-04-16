#' Detect trade flow direction based on element's code and description
#' 
#' @param code Vector with codes of elements 
#' @param description Vector with description of elements
#' 
#' @value Factor with levels "in" and "out".

detectTradeDirection <- function(code, description) {
  
  if(length(code) != length(description)) 
    stop("Code and description vectors are not of the same length.")
  
  flow <- ifelse(stringr::str_detect(code, 
                                     "56\\d{2}") |              # Inflow if code is 56xx
                   stringr::str_detect(description,
                                       "Import|Inflow"),        # OR desc contains Import or Inflow
                 "in",
                 ifelse(stringr::str_detect(code, 
                                            "59\\d{2}") |       # Similar for outflow
                          stringr::str_detect(description, "Export|Outflow"), 
                        "out", 
                        NA))                                    # NA if nothing detected
  as.factor(flow)
}