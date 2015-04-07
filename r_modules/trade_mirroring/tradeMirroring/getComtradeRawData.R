getComtradeRawData = function(measuredItemHSCode){
    dimensions =
        list(Dimension(name = "reportingCountryM49",
                       keys = as.character(allReportingCountryCode)),
             Dimension(name = "partnerCountryM49",
                       keys = as.character(allPartnerCountryCode)),
             Dimension(name = "measuredItemHS",
                       keys = as.character(measuredItemHSCode)),
             ## Dimension(name = "measuredElementTrade",
             ##           keys = as.character(with(elementTable,
             ##               unique(na.omit(c(import, reimport,
             ##                                export, reexport)))))),
             Dimension(name = "measuredElementTrade",
                       keys = as.character(c("5600", "5621", "5630", "5612",
                           "5623", "5900", "5921", "5930", "5912", "5923"))),
             Dimension(name = "timePointYears",
                       keys = as.character(selectedYear)))

    newKey = DatasetKey(domain = "trade", dataset = "ct_raw_tf",
        dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "reportingCountryM49", ascending = TRUE),
        Pivoting(code = "partnerCountryM49", ascending = TRUE),
        Pivoting(code = "measuredItemHS", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    comtradeRaw = GetData(key = newKey, normalized = FALSE, pivoting = newPivot)
    comtradeRaw[,timePointYears := as.numeric(timePointYears)]
    setkeyv(comtradeRaw, cols = c("reportingCountryM49", "partnerCountryM49",
                             "measuredItemHS", "timePointYears"))
    ## NOTE (Michael): The meta data shows which procedure was done to
    ##                 obtain the value. This is replaced by flag

    comtradeRaw[, `:=`(c(grep(valuePrefix, colnames(comtradeRaw), value = TRUE)),
                       lapply(c(grep(valuePrefix, colnames(comtradeRaw),
                                     value = TRUE)),
                              FUN = function(x) as.numeric(.SD[[x]])))]
    comtradeRaw
}
