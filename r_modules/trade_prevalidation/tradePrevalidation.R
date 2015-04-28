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

data <- getComtradeData(reporter = c(getCountryCode("USA\\(1981-")$code,
                                     getCountryCode("Germany\\(1")$code),
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



outers <- data %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate_(.dots = list(price = ~cost / weight)) %>% 
  group_by_(~reporter, ~partner, ~dir, ~back, ~item, ~hs) %>%
  do(broom_augment(lm(price ~ year, data = .))) %>% # NSE here!
  ungroup() %>%
  arrange_(~desc(abs(.std.resid))) %>%
  select_(~reporter, ~partner, ~dir, ~back, ~item, ~hs, ~price, ~year, ~.std.resid) %>%
  top_n(20, .std.resid) %>%
  select_(~-.std.resid) %>%
  distinct()

write.table(outers, file.path(output_dir, "outers.csv"), sep = ";", row.names = F, col.names = T)