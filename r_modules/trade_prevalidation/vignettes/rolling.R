library(fclhs)


.ojdbcclasspath = file.path(Sys.getenv("HOME"), "dstrbs", "ojdbc14.jar")



### Wheat gluten

fclitem <- 15
hs  <- c(100110, 100190)
years <- 1990:2012

us <- getCountryCode("USA\\(1981")$code
ke <- getCountryCode("Kenya")$code

## Data from SWS Comtrade

partners <- faosws::GetCodeList(domain = domain,
            dataset = dataset,
            dimension = partnerVar)[type == "country", code]


ct <- getComtradeData(reporter = c(us, ke),
                        partner  = partners,
                        year     = years, 
                        item     = hs,
                        element  = selectElems(unit %in% c("kg", "US$"), backflow == F)) %>%
  mutate_(reporter = ~getCountryName(reportingCountryM49),
          partner  = ~getCountryName(partnerCountryM49),
          dir      = ~getElementCodeDesc(measuredElementTrade)$direction,
          back     = ~getElementCodeDesc(measuredElementTrade)$backflow,
          unit     = ~getElementCodeDesc(measuredElementTrade)$unit,
          group    = ~getElementCodeDesc(measuredElementTrade)$unitgroup,
          element  = ~getElementCodeDesc(measuredElementTrade)$description,
          year     = ~as.numeric(timePointYears)) %>%
  select_(reporter = ~reportingCountryM49,
          partner  = ~partnerCountryM49, 
          ~year, 
          ~dir, 
          ~back, 
          hs = ~measuredItemHS,
          value = ~Value, 
          ~unit, 
          ~group) %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate_(.dots = list(reporter = ~faoarea(reporter),
                       partner = ~faoarea(partner),
                       fcl = ~hs2fcl(hs))) %>%
  select_(.dots = list(~-hs)) %>%
  group_by_(~reporter, ~partner, ~year, ~dir, ~back, ~fcl) %>%
  summarize_(.dots = list(cost  = ~sum(cost),
                          weight = ~sum(weight))) %>%
  mutate_(.dots = list(pricect = ~cost / weight)) %>%
  select_(.dots = list(~-cost, ~-weight))
  


### Prepare for joining

## Data from faostat

faostat <- fclhs::gettfvalid(reporter = fclhs::faoarea(c(ke, us)), 
                             year = years, 
                             fcl = fclitem) %>%
  mutate(dir = factor(ifelse(flow == 1, "in", ifelse(flow == 2, "out", NA))),
         back = F,
         pricefao = value / quantity) %>%
  select(-flow, -quantity, -value)
  

## Joining

wheat <- ct %>%
  left_join(faostat, by =  c("reporter", "partner", "year", "dir", "back", "fcl")) %>%
  group_by(reporter, partner, dir, back, fcl) %>%
  mutate(movingct = caTools::runmean(pricect, k = 3, alg = "C", endrule = "mean"),
         faodiff  = abs(pricefao - pricect)) %>%
  ungroup()  %>%
  left_join(ct %>% 
              group_by_(~reporter, ~partner, ~dir, ~back, ~fcl) %>%
              do(broom_augment(lm(pricect ~ year, data = .))) %>% # NSE here!
              ungroup() %>% 
              select(-pricect, -.fitted, -.resid), # Price is at both sides
            by =  c("reporter", "partner", "year", "dir", "back", "fcl"))


  

library(ggplot2)

wheat %>%
  reshape2::melt(measure.vars = c("pricect", "pricefao"),
                                  variable.name = "source") %>%
  filter(reporter == 114, dir == "in") %>%
  ggplot(aes(as.factor(year), value, color = source)) + geom_point() +
  facet_wrap(~ partner + dir, scales = "free_y") +
  scale_y_continuous("Unit value", labels = scales::dollar)


wheat %>%
#   group_by(reporter, partner, dir, back, fcl) %>% 
#   mutate(n = n())  %>% 
#   filter(n > 1) %>%
  #   ungroup %>%
  plyr::d_ply(c("reporter", "partner", "dir", "back", "fcl"), 
              function(d) {
                ggplot(d, aes(year, pricect)) + 
                  geom_smooth(method = "lm", se = F) +
                  geom_point(aes(colour = ifelse(is.nan(.std.resid), 0, .std.resid),
                                 size   = ifelse(is.nan(faodiff) | is.na(faodiff), 0, faodiff))) + 
                  scale_color_continuous("Std resid") + 
                  scale_size_continuous("Distance from FAOSTAT")
                #                 coord_cartesian(xlim = c(-5000, 5000
                
                
                ggsave(file.path(output_dir, paste(paste(d$reporter[1], 
                                                         d$partner[1],
                                                         d$dir[1],
                                                         d$back[1],
                                                         d$fcl[1],
                                                         sep = "_"), "png", sep = ".")),
                       height = 4, width = 10, dpi = 100)
              })


f1 <- ct %>%
  left_join(faostat, by =  c("reporter", "partner", "year", "dir", "back", "fcl")) %>%
  group_by(reporter, partner, dir, back, fcl) %>%
  mutate(movingct = caTools::runmean(pricect, k = 3, alg = "C", endrule = "mean")) %>%
  ungroup()  %>%
  left_join(ct %>% 
              group_by_(~reporter, ~partner, ~dir, ~back, ~fcl) %>%
              do(broom_augment(lm(pricect ~ year, data = .))) %>% # NSE here!
              ungroup() %>% 
              select(-pricect, -.resid), # Price is at both sides
            by =  c("reporter", "partner", "year", "dir", "back", "fcl")) %>%
  select(-.hat, -.cooksd, -.std.resid) %>%
  filter(year >= 2009,
         !is.na(partner),
         dir == "out") %>%
  mutate(year = as.Date(paste0(year, "-01-01")),
         partner = sapply(partner, function(x) {
           iso <- as.character(fclhs::faoareanames$iso2[fclhs::faoareanames$fao == x])
           if(length(iso) != 1) iso <- paste0("FAO", x)
           iso
         }, USE.NAMES = F),
         reporter = sapply(reporter, function(x) {
           iso <- as.character(fclhs::faoareanames$name[fclhs::faoareanames$fao == x])
           if(length(iso) != 1) iso <- paste0("FAO", x)
           iso
         }, USE.NAMES = F)
  ) %>% 
  reshape2::melt(measure.vars = c("pricefao", "movingct", ".fitted")) %>%
  ggplot() + 
  geom_segment(aes(x = year - lubridate::days(30),
                   xend = year + lubridate::days(30),
                   y = pricect, yend = pricect)) +
  geom_point(aes(year, value, color = variable, shape = variable), alpha = .8) + 
  facet_grid(partner ~ reporter, scales = "free_y") +
  scale_x_date("", labels = scales::date_format("%Y"), breaks = scales::date_breaks("year")) +
  scale_color_discrete("Type of UV estimation",
                       breaks = c("pricefao", "movingct", ".fitted"),
                       labels = c("FAOSTAT", "Rolling mean", "Linear model")) +
  scale_shape_discrete("Type of UV estimation",
                       breaks = c("pricefao", "movingct", ".fitted"),
                       labels = c("FAOSTAT", "Rolling mean", "Linear model")) +
  scale_y_continuous("Unit value", label = function(x) round(x, 1)) + 
  theme(legend.position="top", axis.title.y = element_text(hjust = 1)) + 
  ggtitle("Different UV-estimations. Wheat export. 2009-2011")

ggsave(file.path(output_dir, "bigpic.png"), f1, width = 6, height = 130, dpi = 100, limitsize = F)

 