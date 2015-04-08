suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(faoswsFlag)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(igraph)
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
    files = dir(path = "./tradeReliability", pattern = "\\.R$", recursive = TRUE,
        full.names = TRUE)
    lapply(files, FUN = function(x) source(x))
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
               reimport = c("5612", "5623", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5923", NA),
               stringsAsFactors = FALSE)


## Calculate Trade reliability
reliabilityIndex = 
    getComtradeMirroredData(reportingCountries = allReportingCountryCode,
                            partnerCountries = allPartnerCountryCode,
                            items = allItem,
                            dataContext = swsContext.datasets[[1]]) %>%
    mergeReverseTrade(data = ., elementTable = elementTable) %>%
    calculatePairWiseConcordance(data = .,
                                 reportingCountry = reportingCountryVar,
                                 partnerCountry = partnerCountry,
                                 year = yearVar,
                                 mirroredFlag = "m") %>%
    calculateReliability(data = .,
                         reportingCountry = reportingCountryVar,
                         partnerCountry = partnerCountry,
                         year = yearVar,
                         concordance = "concordance") %>%
    setnames(x = ., old = "reliability", new = "Value_measuredElement_RELIDX") %>%
    .[, `:=`(c("flagObservationStatus_measuredElement_RELIDX",
               "flagMethod_measuredElement_RELIDX"),
             list("E", "e"))] %>%
    saveReliabilityIndex(reliability = .)



## Tests
## ---------------------------------------------------------------------

## tradeRawData =
##     getComtradeMirroredData(swsContext.datasets[[1]]) %>%
##     mergeReverseTrade(data = .)


## tradeConcordance = 
##     tradeRawData %>%
##     calculatePairWiseConcordance(data = .,
##                                  reportingCountry = "reportingCountryM49",
##                                  partnerCountry = "partnerCountryM49",
##                                  year = "timePointYears",
##                                  mirroredFlag = "m",
##                                  tolerance = 0)

## pdf(file = "graph.pdf", width = 30, height = 30)
## tradeReliability =
##     tradeConcordance %>%
##     calculateReliability(data = .,
##                          reportingCountry = "reportingCountryM49",
##                          partnerCountry = "partnerCountryM49",
##                          year = "timePointYears",
##                          concordance = "concordance",
##                          plot = TRUE)
## graphics.off()


## countryList =
##     GetCodeList("trade", "completed_tf",
##                 "reportingCountryM49")[type == "country", list(code, description)]
## setnames(countryList, "code", "geographicAreaM49")
## check = merge(tradeReliability, countryList, by = "geographicAreaM49",
##     all.x = TRUE)
## check = check[order(reliability), ]
## print(check, nrow = 229)
