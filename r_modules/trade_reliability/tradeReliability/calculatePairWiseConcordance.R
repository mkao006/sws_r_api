calculatePairWiseConcordance = function(data, reportingCountry, partnerCountry,
    year, mirroredFlag, tolerance){

    tmp = data[!(data[[flagPrefix]] %in% mirroredFlag) &
               !(data[[paste0("reverse_", flagPrefix)]] %in% mirroredFlag), ]
    concordance =
        tmp[,list(concordance =
                       sum(abs(computeRatio(.SD[[valuePrefix]] -
                                            .SD[[paste0("reverse_", valuePrefix)]],
                                            .SD[[valuePrefix]])) <= 0)/.N),
            by = c(reportingCountry, partnerCountry, year)]
    concordance
}
