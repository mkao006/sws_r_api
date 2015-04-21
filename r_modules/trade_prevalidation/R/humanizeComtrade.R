humanizeComtrade <- function(data) {
  data %>%
    mutate_(reporter = ~getCountryName(reportingCountryM49),
            partner  = ~getCountryName(partnerCountryM49),
            item     = ~getHSDesc(measuredItemHS)$description,
            dir      = ~getElementCodeDesc(measuredElementTrade)$direction,
            back     = ~getElementCodeDesc(measuredElementTrade)$backflow,
            unit     = ~getElementCodeDesc(measuredElementTrade)$unit,
            group    = ~getElementCodeDesc(measuredElementTrade)$unitgroup,
            element  = ~getElementCodeDesc(measuredElementTrade)$description) %>%
    select_(~reporter,
            ~partner, 
            year = ~timePointYears, 
            ~dir, 
            ~back, 
            hs = ~measuredElementTrade,
            value = ~Value, 
            ~unit, 
            ~group, 
            ~item)
  
}