suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(data.table)
    library(magrittr)
    library(reshape2)
})

verbose = FALSE

if(verbose){
    startingTime = Sys.time()
    currentTime = startingTime
}


## Year should be a paramameter selected.
selectedYear = "2010"


## Setting up variables
areaVar = "geographicAreaM49"
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value"
flagPrefix = "flagTrade"
reverseTradePrefix = "reverse_"


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "1e5c87fe-320f-4faa-9485-fde92f5b8fef"
        )
    verbose = TRUE
}


## Get all reporting country codes
allReportingCountryCode =
    GetCodeList(domain = "trade",
                dataset = "ct_raw_tf",
                dimension = reportingCountryVar)[type == "country", code]

## Get all partner country codes
allPartnerCountryCode =
    GetCodeList(domain = "trade",
                dataset = "ct_raw_tf",
                dimension = partnerCountryVar)[type == "country", code]


## Get All possible items
allItem =
    GetCodeList(domain = "trade",
                dataset = "ct_raw_tf",
                dimension = itemVar)[, code]


elementTable =
    data.frame(type = c("quantity", "value", "unit_value"),
               import = c("5600", "5621", "5630"),
               reimport = c("5612", "5622", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5922", NA))

## Get Comtrade mirrored data
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



## Function to map the trade reported by the bilateral partner
mergeReverseTrade = function(data){
    origin = copy(data)
    reverse = copy(data)
    setkeyv(origin, c(reportingCountryVar, partnerCountryVar, elementVar,
                      itemVar, yearVar))
    setkeyv(reverse, c(reportingCountryVar, partnerCountryVar, elementVar,
                      itemVar, yearVar))
    
    setnames(reverse,
             old = c(reportingCountryVar, partnerCountryVar, valuePrefix,
                 flagPrefix),
             new = c(partnerCountryVar, reportingCountryVar,
                 paste0("reverse_", valuePrefix), paste0("reverse_", flagPrefix)))

    ## Create reverse mapping
    map1 = elementTable[, c("import", "export")]
    colnames(map1) = c("origin", "to")
    map2 = elementTable[, c("export", "import")]
    colnames(map2) = c("origin", "to")
    reversionTable = rbind(map1, map2)

    reverse[, `:=`(c(elementVar),
                   reversionTable[match(measuredElementTrade,
                                        reversionTable$origin), "to"])]
    
    originWithReverse =
        merge(origin, reverse,
              by = c(reportingCountryVar, partnerCountryVar, elementVar,
                  itemVar, yearVar))
    originWithReverse
}


## Function to calculate the reliability index
calculateReliability = function(data, mirroredFlag = "m", tolerance = 0.05){
    tmp = data[!(data[[flagPrefix]] %in% mirroredFlag) &
               !(data[[paste0("reverse_", flagPrefix)]] %in% mirroredFlag), ]
    
    reliability =
        tmp[,sum((.SD[[valuePrefix]] - .SD[[paste0("reverse_", valuePrefix)]])/
                 .SD[[valuePrefix]] <= tolerance)/.N,
             by = c(reportingCountryVar, yearVar)]
    setnames(reliability, old = c(reportingCountryVar, "V1"),
             new = c(areaVar, "Value_measuredElement_RELIDX"))
    reliability[, `:=`(c("flagObservationStatus_measuredElement_RELIDX",
                         "flagMethod_measuredElement_RELIDX"),
                       list("E", "e"))]
    reliability
}


## Save the reliability index back
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


## Compute reliability index

getComtradeMirroredData(swsContext.datasets[[1]]) %>%
    mergeReverseTrade(data = .) %>%
    calculateReliability(data = ., tolerance = 0) %>%
    saveReliabilityIndex
