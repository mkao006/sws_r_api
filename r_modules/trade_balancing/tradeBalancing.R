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




## Setting up variables
standardCountryVar = "geographicAreaM49"
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
        token = "b6b96ffd-bfeb-48e8-a622-b9c39233aa26"
        )
    R_SWS_SHARE_PATH = getwd()
    verbose = TRUE
    files = dir(path = "./tradeBalancing", pattern = "\\.R$", recursive = TRUE,
        full.names = TRUE)
    lapply(files, FUN = function(x) source(x))    
} else {
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/kao"
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

tradeFlowFlagTable =
    data.table(flagObservationStatus = c("", "m"),
               flagObservationWeights = c(1, 0.5))

## Get relevant element codes and set names
##
## NOTE (Michael): Lets work with set elements first, then expand them
##                 when we have the formula table.

allElementTable =
    GetCodeList(domain = "trade",
                dataset =  "ct_raw_tf",
                dimension = "measuredElementTrade")

## NOTE (Michael): This table is for cereal only, need Nick to provide
##                 the formula table.
elementTable =
    data.frame(type = c("quantity", "value", "unit_value"),
               import = c("5600", "5621", "5630"),
               reimport = c("5612", "5623", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5923", NA),
               stringsAsFactors = FALSE)


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



## HACK (Michael): This is a hack function to map HS to CPC. The std
##                 data will be saved back as HS then converted to
##                 CPC.
getHStoCPCMapping = function(){
    mapping = GetTableData(schemaName = "ess", tableName = "hs_2_cpc")
    setnames(mapping,
             old = c("hs", "cpc"),
             new = c("measuredItemHS", "measuredItemCPC"))
    mapping
}

mapHStoCPCTradeStd = function(tradeStdData, tradeStdVar, mapping){
    tradeStdMap = merge(tradeStdData, mapping, by = "measuredItemHS",
        allow.cartesian = TRUE)
    tradeStdMapped =
        tradeStdMap[, list(quantity_standard_deviation =
                               sqrt(sum((split * conversion_factor)^2 *
                                            .SD[[tradeStdVar]]^2)),
                          flagObservationStatus = "E",
                          flagMethod = "e"),
                    by = c("geographicAreaM49", "measuredItemCPC",
                        "measuredElementTrade", "timePointYears")]
    ## setnames(tradeStdMapped, old = "V1", new = "quantity_standard_deviation")
    tradeStdMapped
}

getComtradeStandard49Mapping = function(){
    mapping =
        GetTableData(schemaName = "ess", tableName = "comtrade_m49_map")
    codeColumn = c("comtrade_code", "standard_code", "translation_code")
    mapping[, `:=`(c(codeColumn),
                   lapply(codeColumn,
                          FUN = function(x){
                              as.character(mapping[[x]])
                          }))]
    mapping
}


## Function to translate the comtrade specific M49 codes to the
## standard UNSD M49 country codes.
comtradeM49ToStandardM49 = function(comtradeData, comtradeM49Name, standardM49Name,
    translationData, translationComtradeM49Name, translationStandardM49Name,
    aggregateKey, aggregateValueCol, aggregateFun){
    if(NROW(comtradeData) > 0){
        
        ## Merge comtrade data with translation data
        setnames(comtradeData,
                 old = comtradeM49Name,
                 new = translationComtradeM49Name)
        translate = merge(comtradeData,
            translationData[, c(translationComtradeM49Name,
                                translationStandardM49Name), with = FALSE],
            by = c(translationComtradeM49Name))
        translate[, `:=`(c(translationComtradeM49Name), NULL)]

        ## NOTE (Michael): If mapping is missing, then aggregate to the
        ##                 code for "world".
        translate[is.na(translate[[translationStandardM49Name]]),
                  `:=`(c(translationStandardM49Name), "0")]

        translated =
            translate[, `:=`(c(aggregateValueCol),
                             lapply(aggregateValueCol,
                                    FUN = function(x) aggregateFun(.SD[[x]]))),
                      by = c(unique(c(translationStandardM49Name, aggregateKey)))]
        translated[, `:=`(c("flagObservationStatus", "flagMethod"),
                          list("E", "e"))]
        setnames(translated,
                 old = translationStandardM49Name,
                 new = standardM49Name)
        return(translated)
    } else {
        return(comtradeData)
    }
}

## End of HACK


## Procedure for trade balancing and calculate trade standard deviation.
allItems = swsContext.datasets[[1]]@dimensions$measuredItemHS@keys
selectedYear = swsContext.datasets[[1]]@dimensions$timePointYears@keys

subContext = swsContext.datasets[[1]]
for(i in allItems){
    cat("Perform Balancing for HS item:", i, "\n")
    subContext@dimensions$measuredItemHS@keys = i

    mirroredData = getComtradeMirroredData(dataContext = subContext)
    if(NROW(mirroredData) == 0)
        next
    
    mirroredData %>%
        mergeReverseTrade(data = .) %>%
        {
            reliability <<- getReliabilityIndex(swsContext.datasets[[1]])
            mergeReliability(data = ., reliability = reliability)
        } %>%
        balanceTrade(data = .) %>%
        {
            ## HACK (Michael): The section on mapping HS to CPC
            ##                 and also the country code is not
            ##                 required, it should be removed
            ##                 later when the database is set up
            ##                 correctly.
            hsToCPCMapping <<- getHStoCPCMapping()
            comtradeToStandardM49Mapping <<- getComtradeStandard49Mapping()
            finalStdData <<-
                mapHStoCPCTradeStd(tradeStdData = .$stdData,
                                   tradeStdVar = "Value",
                                   mapping = hsToCPCMapping) %>%
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
                                         c("measuredItemCPC", yearVar),
                                     aggregateValueCol =
                                         grep(valuePrefix, colnames(.),
                                              value = TRUE),
                                     aggregateFun = function(x) sqrt(sum(x^2))) %>%
             setcolorder(.,
                         neworder = c("geographicAreaM49",
                             "measuredItemCPC",
                             "measuredElementTrade", "timePointYears",
                             "quantity_standard_deviation",
                             "flagObservationStatus", "flagMethod")) %>%
             setnames(.,
                      old = "quantity_standard_deviation",
                      new = "Value")
            if(NROW(finalStdData) > 0)
                saveTradeStandardDeviation(stdData = finalStdData)
            selectBalancedTargetData(data = .$balanceData)
        } %>%
        saveBalancedData(data = .)
}
