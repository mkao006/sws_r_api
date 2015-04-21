elcode <- function(...) {
  
  codes <- as.character(list(...))
  getElementCodeDesc(codes)
  
}

getElementCodeDesc <- function(codes) {
  
  codes <- data.table(pos = seq_along(codes), # Column to store original order
                      code = as.character(codes))
  
  codedescs <- getTradeElementTable() %>%
    normalizeTradeElementTable() %>%
    filter(code %in% codes$code)
  
  left_join(codes, codedescs, by = "code") %>%
    arrange_(~pos) %>% # Arrange by original order
    select(-pos)
  
}