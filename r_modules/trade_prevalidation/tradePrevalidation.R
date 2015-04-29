#' Main prevalidation R-module script


rProf <- TRUE
reporters <- c(643, 381)

# suppressMessages({
#library(faosws)
# library(data.table)
suppressPackageStartupMessages(library(dplyr))
# library(stringr)
# library(ggplot2)
# })

module_name <- "trade_prevalidation"
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
    token = "11ac4873-4747-43cd-b942-711d0ebfe844")
  
  
} 

# Settings for working environment
if(Sys.getenv("DEBUG_MODE") %in% c("FALSE", "F", "0")) {
  
  output_dir <- file.path(Sys.getenv("R_SWS_SHARE_PATH"),
                          module_name,
                          startMoment)
  
  # If rProf variable was set by module user
  if(!is.null(swsContext.computationParams$rProf))
    rProf <- as.logical(swsContext.computationParams$rProf)
  
  # If user has inputed country codes
  if(!is.null(swsContext.computationParams$reporters))
    reporters <- swsContext.computationParams$reporters %>%
    stringr::str_extract_all('([0-9]+)') %>%
    unlist() %>%
    as.integer()
  
  # If user hasn't inputed country codes we take full list of
  # countries
  if(is.null(swsContext.computationParams$reporters))
    reporters <- getAllReportersRaw() %>%
    normalizeAreas() %>%
    filter_(.dots = list(~type == "country")) %>%
    select_(~code)  %>% unlist()
  
}

dir.create(output_dir, showWarnings = F, recursive = T) 

if(rProf) Rprof(file.path(output_dir, "Rprof.txt"))

data <- getComtradeData(reporter = reporters,
                        partner = getAllPartnersRaw() %>%
                          normalizeAreas() %>%
                          filter_(.dots = list(~type == "country")) %>%
                          select_(~code)  %>% unlist(),
                        year = 1990:2014, 
                        item = getAllItems() %>%
                          filter_(.dots = list(~stringr::str_detect(code, "^10") &
                                                 stringr::str_length(code) == 6)) %>%
                          select_(~code) %>% unlist(),
                        element = selectElems(direction == "in" &
                                                unitgroup %in% c("weight", 
                                                                 "cost", 
                                                                 "price", 
                                                                 "volume"))) %>%
  humanizeComtrade()

if(any(!is.element(unique(data$unit), c("kg", "US$")))) stop("Other than kg and US$ units present!")



data <- data %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate_(.dots = list(price = ~cost / weight)) %>% 
  group_by_(~reporter, ~partner, ~dir, ~back, ~item, ~hs) %>%
  do(broom_augment(lm(price ~ year, data = .))) %>% # NSE here!
  ungroup() 

outers <- data %>%
  arrange_(~desc(abs(.std.resid))) %>%
  select_(~reporter, ~partner, ~dir, ~back, ~item, ~hs, ~price, ~year, ~.std.resid) %>%
  top_n(20, .std.resid) %>%
  distinct()

write.table(outers, file.path(output_dir, "outers.csv"), sep = ";", row.names = F, col.names = T)

if(rProf) Rprof(NULL)
