getHSDesc <- function(hs) {
  
  hs <- data.table(pos  = seq_along(hs),
                   code = as.character(hs))
  
  left_join(hs,
            getAllItems(), by = "code") %>%
    arrange_(~pos) %>%
    select_(~-pos)
  
}