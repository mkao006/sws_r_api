getAllCountryCodes <- function(type = "reporter") {
  
  p <- "partner"
  r <- "reporter"
  
  if(!(type %in% c(p, r))) stop(paste0(
    "Unrecognized type of area. Variants are: ",
    p, ", ", r))
  
  if(type == r) dimen <- "reportingCountryM49"
  if(type == p)  dimen <- "partnerCountryM49"
  
  d <- getAllReportersRaw(dimen = dimen)
  
  d[d$type == "country",]$code
  
}