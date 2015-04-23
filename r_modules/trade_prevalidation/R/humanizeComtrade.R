humanizeComtrade <- function(data) {
  data %>%
    mutate_(reporter = ~getCountryName(reportingCountryM49),
            partner  = ~getCountryName(partnerCountryM49),
            item     = ~getHSDesc(measuredItemHS)$description,
            dir      = ~getElementCodeDesc(measuredElementTrade)$direction,
            back     = ~getElementCodeDesc(measuredElementTrade)$backflow,
            unit     = ~getElementCodeDesc(measuredElementTrade)$unit,
            group    = ~getElementCodeDesc(measuredElementTrade)$unitgroup,
            element  = ~getElementCodeDesc(measuredElementTrade)$description,
            year     = ~as.numeric(timePointYears)) %>%
    select_(~reporter,
            ~partner, 
            ~year, 
            ~dir, 
            ~back, 
            hs = ~measuredItemHS,
            value = ~Value, 
            ~unit, 
            ~group, 
            ~item)
  
}