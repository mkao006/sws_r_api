library(fclhs)
library(stringr)
library(tsoutliers)


# Workaround with dplyr's mask on base's filter()
#... Doesn't work

tso <- function(...) {
  if("package:dplyr" %in% search()) {
    assign("filter", function(...) stats::filter(...),  envir = .GlobalEnv)
  }
  r <- try(tsoutliers::tso(...))
  if(exists("filter", mode = "function", envir = .GlobalEnv,
            inherits = F)) rm(filter)
  if(inherits(r, "try-error")) stop("Error! (See above)")
  r
}

data(faoareanames)



.ojdbcclasspath <- "~/dstrbs/ojdbc14.jar"

items <- fclhs::hsfclmap %>%
  select(fcl, hs) %>%
  group_by(fcl) %>%
  mutate(n_of_hs = n()) %>%
  ungroup() %>%
  left_join(fclhs::hsfclmap %>%
              select(fcl, hs) %>%
              group_by(hs) %>%
              mutate(n_of_fcl = n()) %>%
              ungroup(),
            by = c('fcl', 'hs')) %>%
  filter(n_of_fcl == 1, n_of_hs == 1) %>%
  select(hs) %>%
  unlist() %>%
  unname()


items <- items[3:5]
years <- 1990:2012
reporters <- getCountryCode("USA\\(1981-\\)")$code
partners <- getCountryCode("Canada")$code
elements <- selectElems(unitgroup %in% c("weight", 
                                         "cost", 
                                         "price")) #, 
                                        # "volume",
                                        # "head"))

data <- getComtradeData(reporter = reporters,
                        partner  = partners,
                        year     = years, 
                        item     = items,
                        element  = elements) %>%
  humanizeComtrade()


data %>%
  group_by(reporter, partner, dir, back, item, hs, group, unit) %>%
  
ts <- zoo::zoo(data[1:19]$value, order.by = as.Date(as.character(data[1:19]$year), format = "%Y"))

ts <- ts(data[1:19]$value)


tsNotReg <- function(data, years) {
  # Fill missed years by NA
  yearsall <- seq(from = min(years), to = max(years))
  
  # If data is data.table
  data <- as.data.frame(data)
  data <- cbind(data, years)
  data <- dplyr::left_join(data.frame(years = yearsall),
                   data, by = "years")
  ts(data$data, start = min(data$years))
}




tsNotReg(data[1:19]$value, data[1:19]$year)



