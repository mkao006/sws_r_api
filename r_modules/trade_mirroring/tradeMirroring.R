## NOTE (Michael): Need to request flag for raw data, the flag are missing.

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


faoswsTradeFlagTable =
    data.table(flagObservationStatus = c("", "m", "b"),
               flagObservationWeights = c(1, 0.5, 0.25))

## Setting up variables
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"
reverseTradePrefix = "reverse_"


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "4419f4bb-132f-40a2-b699-2d03ba709f34"
        )
    files = dir(path = "./tradeMirroring", pattern = "\\.R$", recursive = TRUE,
        full.names = TRUE)
    lapply(files, FUN = function(x) source(x))
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


## Get relevant element codes and set names
##
## NOTE (Michael): Lets work with set elements first, then expand them
##                 when we have the formula table.

allElementTable =
    GetCodeList(domain = "trade",
                dataset =  "ct_raw_tf",
                dimension = "measuredElementTrade")
## elementCode = c("5600", "5612", "5621", "5623", "5630", "5900", "5912", "5921",
##     "5923", "5930")
## elementCodeName = c("importQuantity", "reimportQuantity", "importValue",
##     "reimportValue", "importUnitValue", "exportQuantity", "reexportQuantity",
##     "exportValue", "reexportValue", "exportUnitValue")

## NOTE (Michael): This table is for cereal only, need Nick to provide
##                 the formula table.
elementTable =
    data.frame(type = c("quantity", "value", "unit_value"),
               import = c("5600", "5621", "5630"),
               reimport = c("5612", "5623", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5923", NA),
               stringsAsFactors = FALSE)

## NOTE (Michael): I think the assignment of the names and variables
##                 in the global environment is the reason of error on
##                 the server.
assignElementName = function(elementTable){
    meltedElementTable = na.omit(melt(elementTable, id.vars = "type"))
    elementName = with(meltedElementTable, paste(variable, type, sep = "_"))
    elementCode = paste0(valuePrefix, elementVar, "_", meltedElementTable$value)
    mapply(FUN = function(name, colname){
        assign(x = name, value = colname, envir = .GlobalEnv)
        assign(x = name, value = colname, envir = .GlobalEnv)        
        assign(x = paste0(reverseTradePrefix, name),
               value = paste0(reverseTradePrefix, colname),
               envir = .GlobalEnv)        
    }, name = elementName, colname = elementCode)
}

assignElementName(elementTable)


selectedItems = swsContext.datasets[[1]]@dimensions$measuredItemHS@keys
for(i in selectedItems){
    cat("Perform mirroring for HS item:", i, "\n")
    mirrorProcess = try({
            if(verbose){
                cat("Extracting raw data\n")
                currentTime = Sys.time()
            }

            rawData =
                getComtradeRawData(measuredItemHSCode = i) %>%
                removeSelfTrade(data = ., reportingCountry = reportingCountryVar,
                            partnerCountry = partnerCountryVar) %>%
                removeInconsistentQuantityValue(data = .,
                                                quantity = import_quantity,
                                                value = import_value) %>%
                removeInconsistentQuantityValue(data = ., quantity =
                                                    export_quantity,
                                                value = export_value) %>% 
                addRetradeToTrade(data = .,
                                  importQuantity = import_quantity,
                                  reimportQuantity = reimport_quantity,
                                  exportQuantity = export_quantity,
                                  reexportQuantity = reexport_quantity,
                                  importValue = import_value,
                                  reimportValue = reimport_value,
                                  exportValue = export_value,
                                  reexportValue = reexport_value)
            
            if(verbose){
                endTime = Sys.time()
                timeUsed = endTime - currentTime
                cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
                currentTime = endTime
                cat("Performing mirroring\n")
            }
        
            ## Mirrored the data and calculate unit value
            ##
            ## TODO (Michael): Need to change the flag back to "mr" after
            ##                 adding it as a valid flag.

            mirroredData =
                rawData %>%
                    mirrorTrade(data = .,
                                reportingCountry = reportingCountryVar,
                                partnerCountry = partnerCountryVar,
                                reverseTradePrefix = reverseTradePrefix,
                                valueColumns =
                                    grep(valuePrefix, colnames(.), value = TRUE),
                                flagColumns =
                                    grep(flagPrefix, colnames(.), value = TRUE),
                                mirrorFlag = "m") %>%
                 calculateUnitValue(data = .,
                                    importUnitValue = import_unit_value,
                                    importUnitValueFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             import_unit_value),
                                    importTradeValue = import_value,
                                    importTradeValueFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             import_value),
                                    importTradeQuantity = import_quantity,
                                    importTradeQuantityFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             import_quantity),
                                    exportUnitValue = export_unit_value,
                                    exportUnitValueFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             export_unit_value),
                                    exportTradeValue = export_value,
                                    exportTradeValueFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             export_value),
                                    exportTradeQuantity = export_quantity,
                                    exportTradeQuantityFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             export_quantity),
                                    flagTable = faoswsTradeFlagTable)

            if(verbose){
                endTime = Sys.time()
                timeUsed = endTime - currentTime
                cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
                currentTime = endTime
                cat("Saving Data back\n")
            }

            ## Save mirrored data back
            mirroredData %>%
                subsetRequiredData(data = .) %>%
                saveMirroredTradeData(requiredData = .)

            if(verbose){
                endTime = Sys.time()
                timeUsed = endTime - currentTime
                cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
                currentTime = endTime
            }
        })
    
    if(inherits(mirrorProcess, "try-error")){
        print("Mirror Module Failed")
    } else {
        print("Mirror Module Executed Successfully")
    }
    
}
