
wheat[1:13,] %>%
  plyr::d_ply(c("reporter", "partner", "dir", "back", "fcl"), 
              function(d) {
                ggplot(d, aes(year, pricect)) + 
                  geom_smooth(method = "lm", se = F) +
                  geom_point(aes(colour = ifelse(is.nan(.std.resid), 0, .std.resid),
                                 size   = faodiff)) 
                  
                  ggsave(file.path(output_dir, paste(paste(d$reporter[1], 
                                                           d$partner[1],
                                                           d$dir[1],
                                                           d$back[1],
                                                           d$fcl[1],
                                                           sep = "_"), "png", sep = ".")),
                         height = 4, width = 10, dpi = 100)
              })
