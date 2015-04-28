#' Main prevalidation R-module script

# Should module work through limited range of cases
fastText <- TRUE

# suppressMessages({
#library(faosws)
# library(data.table)
library(dplyr)
library(stringr)
# })

module_name <- "trade_prevalidation"
startMoment <- format(Sys.time(), "%Y%m%d%H%M%S")

if(Sys.getenv("DEBUG_MODE") %in% c("", "TRUE", "T", "1")) {
  
  lapply(dir(file.path("r_modules", module_name, "R"), 
             full.names = T), 
         source)
  
  output_dir <- file.path(Sys.getenv("HOME"), 
                          module_name,
                          startMoment)
  
  faosws::GetTestEnvironment(
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = "11ac4873-4747-43cd-b942-711d0ebfe844")
  
} else {
  output_dir <- file.path(Sys.getenv("R_SWS_SHARE_PATH"),
                          module_name,
                          startMoment)
}

dir.create(output_dir, showWarnings = F, recursive = T) 


