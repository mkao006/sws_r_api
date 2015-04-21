#' Looks for country code for given character pattern of country description.
#' 
#' @param pattern regular expression. Character of length 1.
#' @param ignore.case logical. Ignore case while searching. TRUE by default.
#' @param perl logical. Should pattern use the Perl regular expression engine.
#' @param type character string: "reporter" either "partner". Which table of 
#' areas use for search. Currently they are identical in SWS. reporter by default.

getCountryCode <- function(pattern, 
                           ignore.case = T,
                           perl = T,
                           type = "reporter") {
    
  if(length(pattern) != 1) stop("Argument pattern must be length of one")
  
  if(!is.element(type, c("reporter", "partner")))
    stop("Uknown type. Possible variants are partner and reporter.")
  
  if(ignore.case) pattern <- stringr::ignore.case(pattern)
  if(perl) pattern <- stringr::perl(pattern)
    
  if(type == "reporter") areas <- getAllReportersRaw()
  if(type == "partner") areas  <- getAllPartnersRaw()
     
  areas <- areas %>%
    normalizeAreas %>%
    filter(stringr::str_detect(description, pattern)) #NSE here?
  
  if(nrow(areas) > 1) warning("More than 1 match for your query.")
  
  areas
}