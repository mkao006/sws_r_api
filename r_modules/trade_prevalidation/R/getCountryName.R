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
  
  areacodes <- data.table(pos = seq_along(areacodes), # Column to store original order
                          code = as.character(areacodes))
  
  if(type == "reporter") areas <- getAllReportersRaw()
  if(type == "partner")  areas <- getAllPartnersRaw()

  
  areas <- areas %>%
    normalizeAreas %>% # Maybe remove this?
    filter(is.element(code, areacodes$code)) %>%  #NSE here?
    select_(~code, ~description)
  
  areas <- left_join(areacodes, areas, by = "code") %>%
    arrange_(~pos) %>% # Arrange by original order
    select_(~code, ~description)
  
  if(drop) return(areas$description)
  
  areas
}