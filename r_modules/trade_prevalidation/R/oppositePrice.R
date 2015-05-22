oppositePrice <- function(rep, par, direc, value) {
  df <- data.frame(rep = as.character(rep),
                   par = as.character(par), 
                   direc = as.character(direc),
                   value = value,
                   stringsAsFactors = F)
  
  apply(df, 1, function(r, df) {
    value <- df$value[df$par == r[1] & df$rep == r[2] & df$direc != r[3]]
    if(length(value) > 1) stop(paste("Multiple matches for", r[1], r[2], r[3]))
    if(length(value) == 0) value <- NA
    value
  }, df)
  
}
