years <- 2010:2012
hs  <- c(100110, 100190)

reporters <- faosws::GetCodeList(domain = domain,
                       dataset = dataset,
                       dimension = reportVar)[type == "country", code]

partners <- faosws::GetCodeList(domain = domain,
                                 dataset = dataset,
                                 dimension = partnerVar)[type == "country", code]



ct <- getComtradeData(reporter = reporters,
                      partner  = partners,
                      year     = years, 
                      item     = hs,
                      element  = selectElems(unit %in% c("kg", "US$"), backflow == F)) %>%
  humanizeComtrade()


# Calculate UV
ct <- ct %>%
  select_(.dots = list(~-unit)) %>% # We drop unit but we should to check unit consistensy before
  reshape2::dcast(... ~ group) %>%
  mutate(selftrade = reporter == partner,
         rule3 = weight == 0 & cost > 0,
         rule3_1 = weight > 0 & cost == 0,
         uv = ifelse(rule3 | rule3_1, NA, cost / weight)) 



### Choosing mean type

ct <- ct %>%
  group_by(dir, item, hs, year) %>%
  mutate(uv_global1 = mean(uv, na.rm = T) - uv,
         uv_global2 = sum(cost[!(rule3 | rule3_1)]) / sum(weight[!(rule3 | rule3_1)]) - uv,
         uv_global_me1 = median(uv, na.rm = T) - uv,
         uv_global_me2 = median(cost[!(rule3 | rule3_1)]) / median(weight[!(rule3 | rule3_1)]) - uv) %>%
  ungroup()
  
ggplot(ct, aes(uv, uv_global1)) + geom_point() + facet_wrap(~item)

# measure.vars = c('uv_global1', 'uv_global2'), 

ct %>%
  select(-hs, -weight, -cost, -uv) %>%
  reshape2::melt(measure.vars = c("uv_global1", "uv_global2", "uv_global_me1", "uv_global_me2"),
                 value.name = 'uv', variable.name = "uv_type") %>% 
  ggplot(aes(abs(uv), fill = uv_type)) + geom_density(alpha = .3) + facet_wrap(item ~ year, scales = "free") +
  coord_cartesian(xlim = c(0, 10))
  
ct %>%
  select(-hs, -weight, -cost, -uv) %>%
  reshape2::melt(measure.vars = c("uv_global1", "uv_global2", "uv_global_me1", "uv_global_me2"),
                 value.name = 'uv', variable.name = "uv_type") %>% 
  group_by(uv_type) %>%
  summarize(x = mean(abs(uv), na.rm = T),
            sd = sd(abs(uv), na.rm = T))


# We select uv_global_me1


magnorder <- function(x, x0, maxor = 4, na.rm = F) {
  
  df <- data.frame(x = x, x0 = x0)
  
  apply(df, 1, function(r) {
    
    for(i in maxor:1) {
      if(is.na(r[1])) break()
      if(r[1] > r[2] * 10 ^ (i - 1)) break()
    }
    if(is.na(r[1])) return(NA)
    i 
    
  })
}


ct <- ct %>%
  group_by(dir, item, hs, year) %>%
  mutate(uv_global = median(uv, na.rm = T)) %>%
  group_by(dir, item, hs, year, reporter) %>%
  mutate(uv_reporter = median(uv, na.rm = T)) %>%
  ungroup() %>%
  mutate(magnor = magnorder(uv, uv_global))
  

ct <- ct %>%
  group_by(year, item) %>%
  mutate(uv_partner = mirroredVariable(reporter, partner, dir, uv)) %>%
  ungroup()


ct %>%
  filter(magnor == 1,
         dir == "out",
         item == "Durum wheat") %>%
  reshape2::melt(measure.vars = c("uv_global", "uv_reporter", "uv_partner"),
                 variable.name = "uv_type") %>%
  mutate(value = abs(uv - value)) %>%
#   top_n(20, value) %>% select(-year, -dir, -item, -hs)
  group_by(year, dir, item, uv_type) %>%
  summarize(x = median(value, na.rm = T),
            sd = sd(value, na.rm = T)) %>%
  ggplot(aes(uv_type, x)) + geom_bar(stat = "identity") + 
  facet_wrap( ~ year, scales = "free") +
  scale_x_discrete("Type of UV replacement", 
                   breaks = c("uv_global", "uv_reporter", "uv_partner"),
                   labels = c("Global", "Reporter", "Mirroring")) +
  scale_y_continuous("Median distance from true UV") + 
  ggtitle("Method based on reporter's average shows more stable results \nDurum wheat, all reporters and partners")


ct %>%
  filter(magnor == 1,
         dir == "out",
         item == "Durum wheat") %>%
  reshape2::melt(measure.vars = c("uv_global", "uv_reporter", "uv_partner"),
                 variable.name = "uv_type") %>%
  mutate(value = abs(uv - value)) %>%
  ggplot(aes(uv_type, value)) + geom_boxplot() +
  facet_wrap( ~ year, scales = "free") +
  scale_x_discrete("Type of UV replacement", 
                   breaks = c("uv_global", "uv_reporter", "uv_partner"),
                   labels = c("Global", "Reporter", "Mirroring")) +
  scale_y_continuous("Distance from true UV") + 
  ggtitle("Method based on reporter's average shows more stable results \nExport of durum wheat, all reporters and partners") + 
  coord_cartesian(ylim = c(0, .4))
