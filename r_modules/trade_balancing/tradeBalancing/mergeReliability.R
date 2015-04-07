mergeReliability = function(data, reliability){
    reliabilityCopy =
        copy(reliability[, list(geographicAreaM49, timePointYears, Value)])
    dataCopy = copy(data)
    
    setnames(reliabilityCopy,
             old = c(standardCountryVar, "Value"),
             new = c(reportingCountryVar, "reportingReliability"))
    dataWithReportingReliability =
        merge(dataCopy, reliabilityCopy, by = c(reportingCountryVar, yearVar),
              all.x = TRUE)

    setnames(reliabilityCopy,
             old = c(reportingCountryVar, "reportingReliability"),
             new = c(partnerCountryVar, "partnerReliability"))
    reliabilityFull =
        merge(dataWithReportingReliability, reliabilityCopy,
              by = c(partnerCountryVar, yearVar),
              all.x = TRUE)

    ## Countries that does not have reliability are less reliable than
    ## countries that has reported every incorrectly.
    reliabilityFull[is.na(reportingReliability),
                    `:=`(c("reportingReliability"), -10)]
    reliabilityFull[is.na(partnerReliability),
                    `:=`(c("partnerReliability"), -10)]
    reliabilityFull
}
