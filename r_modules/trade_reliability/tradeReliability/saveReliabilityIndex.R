saveReliabilityIndex = function(reliability){

    ## HACK (Michael): The reliability is loaded with standard M49
    ##                 country codes, but the data from complete data
    ##                 is in Comtrade M49. We will only subset
    ##                 countries which are in the target dataset.

    countryList =
        GetCodeList(domain = "trade",
                    dataset = "reliability_index",
                    dimension = "geographicAreaM49")[, code]
    
    SaveData(domain = "trade",
                dataset = "reliability_index",
                data = reliability[geographicAreaM49 %in% countryList, ],
                normalized = FALSE)
}
