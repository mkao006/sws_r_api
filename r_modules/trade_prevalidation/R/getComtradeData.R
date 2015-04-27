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
  
  args <- list(reporter = reporter, 
               partner  = partner,
               year     = year,
               item     = item,
               element  = element)
  nullargs <- sapply(args, is.null)
  if(any(nullargs)) stop(paste0("There are missing arguments: ", 
                                names(args)[nullargs]))
  
  
  ascharnotnullarg <- function(arg) {
    if(!is.null(arg) & !is.character(arg)) arg <- as.character(arg)
    arg <- unname(arg) # SWS doesn't like unamed vectors
    arg
  }
  
  reporter <- ascharnotnullarg(reporter)
  partner  <- ascharnotnullarg(partner)
  year     <- ascharnotnullarg(year)
  item     <- ascharnotnullarg(item)
  element  <- ascharnotnullarg(element)
  
  dms <- list(faosws::Dimension(name = rtvar,
                                keys = reporter),
              faosws::Dimension(name = ptvar,
                                keys = partner),
              faosws::Dimension(name = itmvar,
                                keys = item),
              faosws::Dimension(name = elevar,
                                keys = element),
              faosws::Dimension(name = yrvar, 
                                keys = year))
  
  k <- faosws::DatasetKey(domain     = dmn,
                          dataset    = dtset,
                          dimensions = dms)
  
  d <- faosws::GetData(key = k,
                       normalized = T, 
                       pivoting = c(faosws::Pivoting(code = rtvar),
                                    faosws::Pivoting(code = ptvar),
                                    faosws::Pivoting(code = itmvar),
                                    faosws::Pivoting(code = elevar),
                                    faosws::Pivoting(code = yrvar)
                       ))
  
  d
}