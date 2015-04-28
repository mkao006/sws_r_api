#' Are we in SWS or at local developer's machine
#' 
#' Environment variable DEBUG_MODE is set to "FALSE" in SWS. So if
#' it doesn't exist or is set to TRUE, we are in develop mode.
#' 
#' @param debugvar Character. Name of system variable to check. 
#' "DEBUG_MODE" by default.
#' 
#' @return TRUE if it's debug/development mode, FALSE - in SWS.

is.DebugMode <- function(debugvar = "DEBUG_MODE") {
  
  DEBUG_MODE <- Sys.getenv("DEBUG_MODE")
  
  if(DEBUG_MODE %in% c("", "TRUE", "T", "1")) # If system envir is not set, 
    return(TRUE)                              # Sys.getenv() returnes it as ""
                                             
  if(DEBUG_MODE %in% c("FALSE", "F", "0"))
    return(FALSE)
  
  stop(paste0("Impossible to recognize developer mode:\nDEBUG_MODE is set to ", 
              DEBUG_MODE))
}