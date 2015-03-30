##' Get World Bank Climate Data
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

getWorldBankClimateData = function(){
    ## allCountries =
    ##     GetCodeList(domain = "WorldBank",
    ##                 dataset = "wb_ecogrw",
    ##                 dimension = "geographicAreaM49")[type == "country", code]
    
    newKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_climate",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = getAllCountries()),
                           Dimension(name = "wbIndicator",
                                     keys = c("SWS.FAO.PREC", "SWS.FAO.TEMP")),
                           Dimension(name = "timePointYears",
                                     keys = getAllYears())
                       )
                   )

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "wbIndicator", ascending = TRUE)        
    )

    climateData = GetData(key = newKey, pivoting = newPivot, normalized = FALSE)
    climateData[, timePointYears := as.numeric(timePointYears)]
    climateData
}