#' Return description of countries from SWS for given codes
#' 
#' @param areacodes Area codes to look for. Numeric will be converted 
#' into character.
#' @param type "reporter" or "partner". Which table to use for look up. 
#' Currently these tables are similar. "reporter" by default.
#' @drop logical. Should only vector with descriptions be returned. 
#' TRUE by default. 
#' 
#' @return Vector with descriptions, if drop is TRUE, else tbl/data.table 
#' with code and description columns.

getCountryName <- function(areacodes, type = "reporter", drop = T) {
    
  if(!is.element(type, c("reporter", "partner")))
     stop("Uknown type. Possible variants are partner and reporter.")
  
  areacodes <- as.character(areacodes)
  
  if(type == "reporter") areas <- getAllReportersRaw()
  if(type == "partner")  areas <- getAllPartnersRaw()

  
  areas <- areas %>%
    normalizeAreas %>% # Maybe remove this?
    filter(is.element(code, areacodes)) %>%  #NSE here?
    select_(~code, ~description)
  
  areas <- left_join(data.table(code = areacodes), areas, by = "code")
  
  if(drop) return(areas$description)
  
  areas
}