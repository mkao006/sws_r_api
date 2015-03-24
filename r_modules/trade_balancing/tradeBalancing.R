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
        token = "1e5c87fe-320f-4faa-9485-fde92f5b8fef"
        )
    R_SWS_SHARE_PATH = getwd()
    verbose = TRUE
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
               reimport = c("5612", "5622", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5922", NA))


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

## Function to get mirrored data in normalized form
getComtradeMirroredData = function(dataContext){
    dimensions =
        list(Dimension(name = "reportingCountryM49",
                       keys = as.character(allReportingCountryCode)),
             Dimension(name = "partnerCountryM49",
                       keys = as.character(allPartnerCountryCode)),
             Dimension(name = "measuredItemHS",
                       keys = dataContext@dimensions$measuredItem@keys),
             Dimension(name = "measuredElementTrade",
                       keys = dataContext@dimensions$measuredElementTrade@keys),
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


mergeReverseTrade = function(data){

    origin = copy(data)
    reverse = copy(data)
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


getReliabilityIndex = function(dataContext){
    countries =
        GetCodeList(domain = "trade",
                    dataset = "reliability_index",
                    dimension = "geographicAreaM49")[type == "country", code]
    
    dimensions =
        list(Dimension(name = "geographicAreaM49",
                       keys = countries),
             Dimension(name = "measuredElement",
                       keys = "RELIDX"),
             Dimension(name = "timePointYears",
                       keys = dataContext@dimensions$timePointYears@keys))

    newKey =
        DatasetKey(domain = "trade",
                   dataset = "reliability_index",
                   dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
    )

    reliabilityData = GetData(key = newKey, pivoting = newPivot)
    reliabilityData
}

mergeReliability = function(data, reliability){
    reliabilityCopy = copy(reliability)
    dataCopy = copy(data)
    
    setnames(reliabilityCopy,
             old = c(standardCountryVar, "Value"),
             new = c(reportingCountryVar, "reportingReliability"))
    dataWithReportingReliability =
        merge(dataCopy, reliabilityCopy, by = c(reportingCountryVar, yearVar))

    setnames(reliabilityCopy,
             old = c(reportingCountryVar, "reportingReliability"),
             new = c(partnerCountryVar, "partnerReliability"))
    reliabilityFull =
        merge(dataWithReportingReliability, reliabilityCopy,
              by = c(partnerCountryVar, yearVar))

    ## Countries that does not have reliability are less reliable than
    ## countries that has reported every incorrectly.
    reliabilityFull[is.na(reportingReliability), `:=`(c("reportingReliability", -10))]
    reliabilityFull[is.na(partnerReliability), `:=`(c("partnerReliability", -10))]
    reliabilityFull
}


balanceTrade = function(data){
    balanceData = copy(data)

    balanceData[, reliableValue :=
                 ifelse(reportingReliability >= partnerReliability,
                        .SD[[valuePrefix]],
                        .SD[[paste0("reverse_", valuePrefix)]])]
    balanceData[, reliabilityFlag :=
                    aggregateObservationFlag(balanceData[[flagPrefix]],
                                             balanceData[[paste0("reverse_",
                                                                 flagPrefix)]],
                                             flagTable = tradeFlowFlagTable)]
                    

    stdData =
        balanceData[, sqrt(sum((.SD[[valuePrefix]] - reliableValue)^2)/.N),
                 by = c(reportingCountryVar, elementVar, itemVar, yearVar)]
    setnames(stdData,
             old = c(reportingCountryVar, "V1"),
             new = c(standardCountryVar, "Value"))
    stdData[, measuredElementTrade := paste0("SD", measuredElementTrade)]
    stdData[, `:=`(c("flagObservationStatus", "flagMethod"),
                   list("E", "e"))]
    list(balanceData = balanceData, stdData = stdData)
}


saveTradeStandardDeviation = function(stdData){
    SaveData(domain = "trade",
                dataset = "stddev_quantity",
                data = stdData)
}



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
    aggregateKey, aggregateValueCol){
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
                                    FUN = function(x) sum(.SD[[x]]))),
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

selectSaveSelection = function(data){    
    saveSelection =
        data[, c(names(swsContext.datasets[[1]]@dimensions),
                 "reliableValue", "reliabilityFlag"), with = FALSE]
    setnames(saveSelection,
             old = c("reliableValue", "reliabilityFlag"),
             new = c(valuePrefix, flagPrefix))
    saveSelection
}

saveBalancedData = function(data){
    SaveData(domain = "trade",
                dataset = "ct_published_tf",
                data = data)
}
    

## NOTE (Michael): Should do this by item
allItems = swsContext.datasets[[1]]@dimensions$measuredItemHS@keys
subContext = swsContext.datasets[[1]]
allItems = "1001"
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
            ## NOTE (Michael): The section on mapping HS to CPC
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
                                              value = TRUE)) %>%
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
            selectSaveSelection(data = .$balanceData)
        } %>%
        saveBalancedData(data = .)
}


