#' Function cleans area datasets from SWS: removes empty 
#' columns and adds columns with start and end years.
#' 
#' @param areas Data.table from getAllPartnersRaw() or 
#' getAllReportersRaw()
#' 
#' @return Data.tambe with code, description, type, 
#' startYear and endYear.
#' 
#' @import dplyr

normalizeAreas <- function(areas) {
  areas %>%
    select_(~code, ~description, ~type) %>%
    mutate_(years = ~stringr::str_replace_all(stringr::str_extract(description, 
                                                                   stringr::regex("\\(-?\\d{4}-?(\\d{4})?-?\\)")), 
                                              stringr::regex("\\(|\\)"), ""),
            startYear = ~extractStartYear(years),
            endYear   = ~extractEndYear(years)
            ) %>%
    select_(~code, ~description, ~type, ~startYear, ~endYear)
            
}

extractStartYear <- function(year) {
  
  if(length(year) > 1) return(unlist(lapply(year, extractStartYear)))
  
  if(!stringr::str_detect(year,  
                          stringr::regex("^(\\d{4})-(\\d{4})?$")) | is.na(year)) return(as.numeric(NA))
  
  as.numeric(stringr::str_extract(year, stringr::regex("^\\d{4}")))
  
}

extractEndYear <- function(year) {
  
  if(length(year) > 1) return(unlist(lapply(year, extractEndYear)))
  
  if(!stringr::str_detect(year,  
                          stringr::regex("-\\d{4}$")) | is.na(year)) return(as.numeric(NA))
  
  as.numeric(stringr::str_extract(year, stringr::regex("\\d{4}$")))
  
}
