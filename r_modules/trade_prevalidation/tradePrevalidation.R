#' Main prevalidation R-module script


rProf <- TRUE


suppressPackageStartupMessages(library(dplyr))

token       <- "11ac4873-4747-43cd-b942-711d0ebfe844"
module_name <- "trade_prevalidation"
domain      <- "trade"
dataset     <- "ct_raw_tf"
reportVar   <- "reportingCountryM49"
partnerVar  <- "partnerCountryM49"
itemVar     <- "measuredItemHS"
eleVar      <- "measuredElementTrade"
yearVar     <- "timePointYears"

startMoment <- format(Sys.time(), "%Y%m%d%H%M%S")


# Settings for developer's station
if(Sys.getenv("DEBUG_MODE") %in% c("", "TRUE", "T", "1")) {
  
  lapply(dir(file.path("r_modules", module_name, "R"), 
             full.names = T), 
         source)
  
  output_dir <- file.path(Sys.getenv("HOME"), 
                          module_name,
                          startMoment)
  
  faosws::GetTestEnvironment(
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = token)
  
  
} 

# Settings for working environment
if(Sys.getenv("DEBUG_MODE") %in% c("FALSE", "F", "0")) {
  
  output_dir <- file.path(Sys.getenv("R_SWS_SHARE_PATH"),
                          module_name,
                          startMoment)
  
  # If rProf variable was set by module user
  if(!is.null(swsContext.computationParams$rProf))
    rProf <- as.logical(swsContext.computationParams$rProf)
  
  
  # LIST OF REPORTERS
  # If user has inputed country codes in R plugin execution window
  if(!is.null(swsContext.computationParams$reporters))
    reporters <- swsContext.computationParams$reporters %>%
    stringr::str_extract_all('([0-9]+)') %>%
    unlist() %>%
    as.integer()
  
  # If user hasn't inputed country codes we take countries from 
  # swsContext.datasets
  if(is.null(swsContext.computationParams$reporters))
    reporters <- swsContext.datasets[[1]]@dimensions[[reportVar]]@keys
  
}


# Creating directory for execution profiling
dir.create(output_dir, showWarnings = F, recursive = T) 
# Start execution profiling
if(rProf) Rprof(file.path(output_dir, "Rprof.txt"))


# TODO Should we put here some checkings on input parameters?
partners <- swsContext.datasets[[1]]@dimensions[[partnerVar]]@keys
years    <- swsContext.datasets[[1]]@dimensions[[yearVar]]@keys
items    <- swsContext.datasets[[1]]@dimensions[[itemVar]]@keys
elements <- swsContext.datasets[[1]]@dimensions[[eleVar]]@keys

data <- getComtradeData(reporter = reporters,
                        partner  = partners,
                        year     = years, 
                        item     = items,
                        element  = elements %>%
  humanizeComtrade()

if(any(!is.element(unique(data$unit), c("kg", "US$")))) stop("Other than kg and US$ units present!")



data <- data %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate_(.dots = list(price = ~cost / weight)) #TODO Can we work we other than weight quantities?

# Detecting rules' match

# Adam's rule #1
# Removing self-trade
data <- data %>%
  mutate_(rule1 = ~reporter == partner)

# Rule #2. Completion through mirroring. Implemented in Mirroring module.

# Rule #3. Completion through unit values: missing quantities but values exist

data <- data %>%
  mutate_(rule2 = ~((price == 0 & weight != 0) | (price != 0 & weight == 0)))

# %>% 
#   group_by_(~reporter, ~partner, ~dir, ~back, ~item, ~hs) %>%
#   do(broom_augment(lm(price ~ year, data = .))) %>% # NSE here!
#   ungroup() 

# outers <- data %>%
#   arrange_(~desc(abs(.std.resid))) %>%
#   select_(~reporter, ~partner, ~dir, ~back, ~item, ~hs, ~price, ~year, ~.std.resid) %>%
#   top_n(20, .std.resid) %>%
#   distinct()
# 
# write.table(outers, file.path(output_dir, "outers.csv"), sep = ";", row.names = F, col.names = T)

if(rProf) Rprof(NULL)
