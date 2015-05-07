keyCtl = DatasetKey(domain = "eurostat", dataset = "raw_apro_mt_lscatl",
                    dimensions = list(
                        Dimension(name = "eurostatRawAgriprod",
                                  keys = GetCodeList("eurostat", "raw_apro_mt_lscatl",
                                                     "eurostatRawAgriprod")$code),
                        Dimension(name = "eurostatRawMonth",
                                  keys = "M12"),
                        Dimension(name = "eurostatRawUnit",
                                  keys = GetCodeList("eurostat", "raw_apro_mt_lscatl",
                                                     "eurostatRawUnit")$code),
                        Dimension(name = "eurostatRawGeo",
                                  keys = GetCodeList("eurostat", "raw_apro_mt_lscatl",
                                                     "eurostatRawGeo")$code),
                        Dimension(name = "timePointYears",
                                  keys = as.character(1951:2015))
                 ))
cattleData = GetData(keyCtl)

keyShp = DatasetKey(domain = "eurostat", dataset = "raw_apro_mt_lssheep",
                    dimensions = list(
                        Dimension(name = "eurostatRawAgriprod",
                                  keys = GetCodeList("eurostat", "raw_apro_mt_lssheep",
                                                     "eurostatRawAgriprod")$code),
                        Dimension(name = "eurostatRawMonth",
                                  keys = "M12"),
                        Dimension(name = "eurostatRawUnit",
                                  keys = GetCodeList("eurostat", "raw_apro_mt_lssheep",
                                                     "eurostatRawUnit")$code),
                        Dimension(name = "eurostatRawGeo",
                                  keys = GetCodeList("eurostat", "raw_apro_mt_lssheep",
                                                     "eurostatRawGeo")$code),
                        Dimension(name = "timePointYears",
                                  keys = as.character(1951:2015))
                 ))
sheepData = GetData(keyShp)

identical(cattleData, sheepData)
