getCountryName <- function(areacode, type = "reporter", drop = T) {
    
  if(!is.element(type, c("reporter", "partner")))
     stop("Uknown type. Possible variants are partner and reporter.")
  
  areacode <- as.character(areacode)
  
  if(type == "reporter") areas <- getAllReportersRaw()
  if(type == "partner")  areas <- getAllPartnersRaw()

  
  areas <- areas %>%
    normalizeAreas %>%
    filter(is.element(code, areacode)) %>%  #NSE here?
    select_(~code, ~description)
  
  areas <- left_join(data.table(code = areacode), areas, by = "code")
  
  if(drop) return(areas$description)
  
  areas
}