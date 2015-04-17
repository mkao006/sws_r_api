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
#' @import stringr

normalizeAreas <- function(areas) {
  areas %>%
    select_(~code, ~description, ~type) %>%
    mutate_(years = ~str_replace_all(str_extract(description, 
                                 perl("\\(-?\\d{4}-?(\\d{4})?-?\\)")), perl("\\(|\\)"), ""),
            startYear = ~extractStartYear(years),
            endYear   = ~extractEndYear(years)
            ) %>%
    select_(~code, ~description, ~type, ~startYear, ~endYear)
            
}

extractStartYear <- function(year) {
  
  if(length(year) > 1) return(unlist(lapply(year, extractStartYear)))
  
  if(!str_detect(year,  
                 perl("^(\\d{4})-(\\d{4})?$")) | is.na(year)) return(as.numeric(NA))
  
  as.numeric(str_extract(year, perl("^\\d{4}")))
  
}

extractEndYear <- function(year) {
  
  if(length(year) > 1) return(unlist(lapply(year, extractEndYear)))
  
  if(!str_detect(year,  
                 perl("-\\d{4}$")) | is.na(year)) return(as.numeric(NA))
  
  as.numeric(str_extract(year, perl("\\d{4}$")))
  
}
