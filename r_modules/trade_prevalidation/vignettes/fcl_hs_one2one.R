## Check differencies between TF_SOURCE and SWS ComTrade

library(fclhs)
library(stringr)

data(faoareanames)

.ojdbcclasspath <- "~/dstrbs/ojdbc14.jar"

faoareanames %>%
  filter(str_detect(name, "United States of"))


faoareanames %>%
  filter(str_detect(name, "Canada"))

gettfvalid(reporter = 231, 
           year = 2009, 
           partner = 33,
           fcl = 1062)


data %>%
  filter(hs == "040700", year == 2009) %>%
  select(-back, -group)


data(package = "fclhs")


head(fclhs::hsfclmap)

fcllinks <- fclhs::hsfclmap %>%
  select(fcl, hs) %>%
  group_by(fcl) %>%
  mutate(n_of_hs = n()) %>%
  ungroup()


hslinks <- fclhs::hsfclmap %>%
  select(fcl, hs) %>%
  group_by(hs) %>%
  mutate(n_of_fcl = n()) %>%
  ungroup()

one2one <- fcllinks %>%
  left_join(hslinks, by = c('fcl', 'hs')) %>%
  filter(n_of_fcl == 1, n_of_hs == 1) %>%
  select(fcl, hs) %>%
  mutate(fcl_desc = descfcl(fcl),
         hs_desc  = getHSDesc(hs)$description)


XLConnect::writeWorksheetToFile("~/Desktop/FCL_HS_onetoone.xlsx", as.data.frame(one2one), "one2one")
