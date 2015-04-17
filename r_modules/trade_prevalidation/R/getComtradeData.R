getComtradeData <- function(reporter = NULL,
                            partner  = NULL,
                            year     = NULL, 
                            item     = NULL, 
                            element  = NULL,
                            dmn      = "trade",
                            dtset    = "ct_raw_tf",
                            rtvar    = "reportingCountryM49",
                            ptvar    = "partnerCountryM49",
                            itmvar   = "measuredItemHS",
                            elevar   = "measuredElementTrade",
                            yrvar    = "timePointYears") {
  
  if(!is.SWSEnvir()) stop("No SWS environment detected.")
  
  ascharnotnullarg <- function(arg) {
    if(!is.null(arg) & !is.character(arg)) arg <- as.character(arg)
    arg
  }
  
  reporter <- ascharnotnullarg(reporter)
  partner  <- ascharnotnullarg(partner)
  year     <- ascharnotnullarg(year)
  item     <- ascharnotnullarg(item)
  element  <- ascharnotnullarg(element)
  
  dms <- list(Dimension(name = rtvar,
                        keys = reporter),
              Dimension(name = ptvar,
                        keys = partner),
              Dimension(name = itmvar,
                        keys = item),
              Dimension(name = elevar,
                        keys = element),
              Dimension(name = yrvar, 
                        keys = year))
  
  k <- DatasetKey(domain     = dmn,
                  dataset    = dtset,
                  dimensions = dms)
  
  d <- GetData(key = k,
               normalized = T, 
               pivoting = c(Pivoting(code = rtvar),
                            Pivoting(code = ptvar),
                            Pivoting(code = itmvar),
                            Pivoting(code = elevar),
                            Pivoting(code = yrvar)
               ))
  
  d
}