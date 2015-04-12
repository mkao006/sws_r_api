##' Get Comtrade Mirrored Data
##' 
##' @param reportingCountries A character vector of the country codes for all
##' reporting countries which should be pulled.
##' @param partnerCountries A character vector of the country codes for all
##' partner countries which should be pulled.
##' @param item A character vector of all the HS commodity codes which should
##' be pulled.
##' 
##' @return A data.table object containing trading data for all input
##' reporting countries, partner countries, items, and years.
##' 

getComtradeMirroredData = function(reportingCountries, partnerCountries, items, years){
        dimensions =
            list(Dimension(name = "reportingCountryM49",
                           keys = as.character(allReportingCountryCode)),
                 Dimension(name = "partnerCountryM49",
                           keys = as.character(allPartnerCountryCode)),
                 Dimension(name = "measuredItemHS",
                           keys = items),
                 Dimension(name = "measuredElementTrade",
                           keys = c("5600", "5900")),
                 Dimension(name = "timePointYears",
                           keys = years))

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
