#' Calculate order of magnitude. Useful for testing of misplaced decimal points.
#' 
#' @param x Numeric vector of tested values.
#' @param x0 Numeric vector of base values.
#' @param maxorder Maxim order to check from. 4 by default.
#' 
#' @return Vector of length x


magnitudeOrder <- function(x, x0, maxorder = 4) {
  
  if(!((length(x0) != length(x)) | (length(x0) != 1))) { 
    stop("Length of base vector must be equal 1 or length of tested vector.")
  }
  
  df <- data.frame(x = x, x0 = x0)
  
  apply(df, 1, function(r) {
    
    # TODO: change this fornext to apply to increase speed
    for(i in maxorder:1) {
      if(is.na(r[1])) break()
      if(r[1] > r[2] * 10 ^ (i - 1)) break()
    }
    if(is.na(r[1])) return(NA)
    i 
    
  })
}