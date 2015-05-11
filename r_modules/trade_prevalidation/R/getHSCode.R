#' Looks for HS commodity codes for given character pattern of commodity description.
#' 
#' @param pattern regular expression. String of length 1.
#' @param ignore.case logical. Ignore case while searching. TRUE by default.
#' @param perl logical. Should pattern use the Perl regular expression engine.
#' 
#' @return data.table with code and description columns


getHSCode <- function(pattern, 
                      ignore.case = T,
                      perl = T) {
  
  if(length(pattern) != 1) stop("Argument pattern must be length of one")
 
  if(ignore.case) pattern <- stringr::ignore.case(pattern)
  if(perl) pattern <- stringr::perl(pattern)
  
  getAllItems() %>%
    filter(stringr::str_detect(description, pattern)) #NSE here?

}