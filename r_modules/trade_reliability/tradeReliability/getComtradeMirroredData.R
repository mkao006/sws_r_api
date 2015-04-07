getComtradeMirroredData = function(dataContext){
        dimensions =
            list(Dimension(name = "reportingCountryM49",
                           keys = as.character(allReportingCountryCode)),
                 Dimension(name = "partnerCountryM49",
                           keys = as.character(allPartnerCountryCode)),
                 Dimension(name = "measuredItemHS",
                           keys = allItem),
                 Dimension(name = "measuredElementTrade",
                           keys = c("5600", "5900")),
                 Dimension(name = "timePointYears",
                           keys = dataContext@dimensions$timePointYears@keys))

    newKey =
        DatasetKey(domain = "trade",
                   dataset = "completed_tf",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "reportingCountryM49", ascending = TRUE),
        Pivoting(code = "partnerCountryM49", ascending = TRUE),
        Pivoting(code = "measuredItemHS", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    mirroredData = GetData(key = newKey, pivoting = newPivot)
    mirroredData
}
