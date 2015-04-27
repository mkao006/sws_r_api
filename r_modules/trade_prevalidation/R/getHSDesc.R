getHSDesc <- function(hs) {
  
  hs <- as.character(hs)
  
  dplyr::left_join(data.table(code = hs),
                   getAllItems())
  
  
}