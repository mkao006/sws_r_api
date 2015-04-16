#' Function to check is it SWS environment
#' 
#' Detecting existence of SWS Envir in calling env
#' by counting swsContext.* vars
#' 
#' @return logical value. TRUE if there are 8 swsContext vars

is.SWSEnvir <- function() {
  length(ls(pos = 1, pattern = "^swsContext\\.")) == 8
}