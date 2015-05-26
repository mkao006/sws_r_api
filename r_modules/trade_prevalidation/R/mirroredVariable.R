#' Returns corresponding value from opposite side of trade flow
#' 
#' @param reporter Vector with reporters' codes
#' @param partner Vector with partners' codes
#' @param direction Vector with trade flow directions
#' @param variable Vector with values to get mirrored data from
#' 
#' @return Vector with values from opposite side of trade flow

mirroredVariable <- function(reporter, partner, direction, variable) {
  df <- data.frame(rep = as.character(reporter),
                   par = as.character(partner), 
                   direc = as.character(direction),
                   value = variable,
                   stringsAsFactors = F)
  
  apply(df, 1, function(r, df) {
    value <- df$value[df$par == r[1] & df$rep == r[2] & df$direc != r[3]]
    if(length(value) > 1) stop(paste("Multiple matches for", r[1], r[2], r[3]))
    if(length(value) == 0) value <- NA
    value
  }, df)
  
}
