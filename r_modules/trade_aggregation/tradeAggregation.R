suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
})
verbose = FALSE


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "d1eeead4-ba76-4a9a-9ba8-cc6904a5aa3b"
        )
    verbose = TRUE
    files = dir(path = "./tradeAggregation", pattern = "\\.R$", recursive = TRUE,
        full.names = TRUE)
    lapply(files, FUN = function(x) source(x))
}

## Setting up variables
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
standardCountryVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"
reverseTradePrefix = "reverse_"



## Get all reporting country codes
allReportingCountryCode =
    GetCodeList(domain = "trade",
                dataset = "completed_tf",
                dimension = reportingCountryVar)[type == "country", code]

## Get all partner country codes
allPartnerCountryCode =
    GetCodeList(domain = "trade",
                dataset = "completed_tf",
                dimension = partnerCountryVar)[type == "country", code]

## Aggregate the data
aggregate =
    try({
        ## Get the raw data
        if(verbose){
            cat("Extracting raw data\n")
            currentTime = Sys.time()
        }
        mirroredData = getComtradeMirroredData(swsContext.datasets[[1]])

        ## Aggregate the data
        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
            cat("Performing Trade Flow Aggregation\n")
        }
        aggregatedData =
            copy(mirroredData) %>%
            aggregateTradeFlow(mirroredData = .,
                                 aggregateFlag = "") %>%
            {
                ## Map UNSD comtrade M49 country codes to standard M49
                ## country codes
                if(verbose){
                    endTime = Sys.time()
                    timeUsed = endTime - currentTime
                    cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
                    currentTime = endTime
                    cat("Map UNSD Comtrade M49 to Standard M49\n")
                }

                comtradeToStandardM49Mapping <<- getComtradeStandard49Mapping()
                comtradeM49ToStandardM49(comtradeData = .,
                                         comtradeM49Name = "geographicAreaM49",
                                         standardM49Name = "geographicAreaM49",
                                         translationData =
                                             comtradeToStandardM49Mapping,
                                         translationComtradeM49Name =
                                             "comtrade_code",
                                         translationStandardM49Name =
                                             "translation_code",
                                         aggregateKey =
                                             c(itemVar, yearVar),
                                         aggregateValueCol =
                                             grep(valuePrefix, colnames(.),
                                                  value = TRUE))
            }

        ## Save the data back to the data base
        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
            cat("Saving Data back\n")
        }


        ## Save aggregated data back
        aggregatedData %>%
            saveAggregatedUnbalanceData(aggregatedData = .)

        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
        }
    })

if(inherits(aggregate, "try-error")){
    print(paste0("Trade Flow Aggregation Module Failed\n", aggregate[1]))
} else {
    print("Trade Flow Aggregation Module Executed Successfully")
}
