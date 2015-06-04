#' Main prevalidation R-module script


rProf <- TRUE
compareWithFCL <- TRUE
nOfOutl <- 20
nOfYears <- 3

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))

token       <- "218d0df9-bc99-4a6a-994f-82f583b5f063"
module_name <- "trade_prevalidation"
domain      <- "trade"
dataset     <- "ct_raw_tf"
reportVar   <- "reportingCountryM49"
partnerVar  <- "partnerCountryM49"
itemVar     <- "measuredItemHS"
eleVar      <- "measuredElementTrade"
yearVar     <- "timePointYears"

startMoment <- format(Sys.time(), "%Y%m%d%H%M")


# Settings for developer's station
if(Sys.getenv("DEBUG_MODE") %in% c("", "TRUE", "T", "1")) {
  
  if(length(lapply(dir(file.path("r_modules", module_name, "R"), 
             full.names = T), 
         source)) == 0) stop("Files for sourcing not found")
  
  output_dir <- file.path(Sys.getenv("HOME"), 
                          module_name,
                          paste0(Sys.getenv("USER"), startMoment))
  
  faosws::GetTestEnvironment(
    baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
    token = token)
  
  reporters <- swsContext.datasets[[1]]@dimensions[[reportVar]]@keys
  
} 

# Settings for working environment
if(Sys.getenv("DEBUG_MODE") %in% c("FALSE", "F", "0")) {
  

  output_dir <- file.path(Sys.getenv("R_SWS_SHARE_PATH"),
                          module_name,
                          paste0(Sys.getenv("USER"), startMoment))
  
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
                        element  = elements) %>%
  humanizeComtrade()

if(any(!is.element(unique(data$unit), c("kg", "US$")))) stop("Other than kg and US$ units present!")



data <- data %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate_(.dots = list(price = ~cost / weight)) # %>% #TODO Can we work with other than weight quantities?
#   group_by_(~year, ~dir, ~back, ~item, ~hs) %>%
  #mutate_(.dots = list(price_item_mo = ~kdeModeBoot(price))) %>%
#   ungroup()

# Detecting rules' match

# Adam's rule #1
# Removing self-trade
# Implemented in mirroring, but still...
data <- data %>%
  mutate_(rule1 = ~reporter == partner)

# Rule #2. Completion through mirroring. Implemented in Mirroring module.

# Rule #3. Completion through unit values: missing quantities but values exist

data <- data %>%
  mutate_(rule3 = ~(cost != 0 & weight == 0))

# Rule #3. Completion through unit values: missing values but quantities exist

data <- data %>%
  mutate_(rule3_1 = ~(weight != 0 & cost == 0))

# Rule #5.
#   # Filter near matching trade flows



# LM outliers


data <- data %>% 
  left_join(data %>% 
              group_by_(~reporter, ~partner, ~dir, ~back, ~item, ~hs) %>%
              do(broom_augment(lm(price ~ year, data = .))) %>% # NSE here!
              ungroup() %>% 
              select(-price, -.fitted, -.resid), # Price is at both sides
            by =  c("reporter", "partner", "year", "dir", "back", "item", "hs"))
  
flowsWithOutls <- data %>%
  mutate_(outlrating = ~min_rank(desc(abs(.std.resid)))) %>%
  group_by_(~reporter, ~partner, ~dir, ~back, ~item, ~hs) %>%
  mutate_(tocheck = ~any(outlrating <= nOfOutl)) %>%
  ungroup() %>%
  filter_(~tocheck) %>%
  mutate_(isoutl = ~outlrating <= nOfOutl)

plyr::d_ply(flowsWithOutls, c("reporter", "partner", "dir", "back", "item", "hs"), 
            function(d) {
              ggplot(d, aes(year, price)) + 
                geom_smooth(method = "lm", se = F) +
                geom_point(aes(colour = isoutl)) + 
                ggtitle(paste(d$reporter[1], 
                              ifelse(d$dir[1] == "in", "<-", ifelse(d$dir[1] == "out", "->", "???")),
                              d$partner[1],
                              ifelse(d$back[1], "Retrade:yes", "Retrade:no"), "\n",
                              d$item[1],
                              sep = " ")) +
                scale_x_continuous("", breaks = seq(from = min(d$year), to = max(d$year))) + 
                scale_y_continuous("", labels = scales::dollar) +
                scale_color_manual(values = c("black", "red"), guide=FALSE)
              ggsave(file.path(output_dir, paste(paste(d$reporter[1], 
                                                 d$partner[1],
                                                 d$dir[1],
                                                 d$back[1],
                                                 d$hs[1],
                                                 sep = "_"), "png", sep = ".")),
                    height = 4, width = 10, dpi = 100)
            })



# outers <- data %>%
#   arrange_(~desc(abs(.std.resid))) %>%
#   select_(~reporter, ~partner, ~dir, ~back, ~item, ~hs, ~price, ~year, ~.std.resid) %>%
#   top_n(20, .std.resid) %>%
#   distinct()
# 
# write.table(outers, file.path(output_dir, "outers.csv"), sep = ";", row.names = F, col.names = T)

if(rProf) Rprof(NULL)
