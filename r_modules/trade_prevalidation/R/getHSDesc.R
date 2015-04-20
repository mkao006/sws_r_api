getHSDesc <- function(hs) {
  
  hs <- as.character(hs)
  
  desc <- getAllItems() %>%
    filter(is.element(code, hs)) %>% #NSE here?
    as.data.frame()
  
  desc
}