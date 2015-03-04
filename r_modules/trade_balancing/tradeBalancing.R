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
    data.table(flagObservationStatus = c("", "/"),
               flagObservationWeights = c(1, 0.5))

## Get relevant element codes and set names
##
## NOTE (Michael): Lets work with set elements first, then expand them
##                 when we have the formula table.

allElementTable =
    GetCodeList(domain = "trade",
                dataset =  "ct_raw_tf",
                dimension = "measuredElementTrade")
## elementCode = c("5600", "5612", "5621", "5622", "5630", "5900", "5912", "5921",
##     "5922", "5930")
## elementCodeName = c("importQuantity", "reimportQuantity", "importValue",
##     "reimportValue", "importUnitValue", "exportQuantity", "reexportQuantity",
##     "exportValue", "reexportValue", "exportUnitValue")

## NOTE (Michael): This table is for cereal only, need Nick to provide
##                 the formula table.
elementTable =
    data.frame(type = c("quantity", "value", "unit_value"),
               import = c("5600", "5621", "5630"),
               reimport = c("5612", "5622", NA),
               export = c("5900", "5921", "5930"),
               reexport = c("5912", "5922", NA))

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


getReliabilityIndex = function(){}

mergeReliability = function(data, reliability){
    reliabilityCopy = copy(reliability)
    dataCopy = copy(data)
    
    setnames(reliabilityCopy,
             old = c(areaVar, "reliability"),
             new = c(reportingCountryVar, "reportingReliability"))
    dataWithReportingReliability =
        merge(dataCopy, reliabilityCopy, by = c(reportingCountryVar, yearVar))

    setnames(reliabilityCopy,
             old = c(reportingCountryVar, "reportingReliability"),
             new = c(partnerCountryVar, "partnerReliability"))
    reliabilityFull =
        merge(dataWithReportingReliability, reliabilityCopy,
              by = c(partnerCountryVar, yearVar))
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
                                             balanceData[[paste0("reverse_", flagPrefix)]],
                                             flagTable = tradeFlowFlagTable)]
                    

    stdData =
        balanceData[, sqrt(sum((.SD[[valuePrefix]] - reliableValue)^2)/.N),
                 by = c(reportingCountryVar, elementVar, itemVar, yearVar)]
    setnames(stdData,
             old = c(reportingCountryVar, "V1"),
             new = c(areaVar, "standard_deviation"))
    list(balanceData = balanceData, stdData = stdData)
}


saveTradeStandardDeviation = function(stdData){
    SaveDataNew(domain = "trade",
                dataset = "tradestd",
                data = stdData)
}

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
    SaveDataNew(domain = "trade",
                dataset = "ct_published_tf",
                data = data)
}
    

    
mirroredData = getComtradeMirroredData(dataContext = swsContext.datasets[[1]])

mirroredData %>%
    mergeReverseTrade(data = .) %>%
        {
            reliability <<- getReliabilityIndex()
            mergeReliability(data = ., reliability = reliability)
        } %>%
    balanceTrade(data = .) %>%
        {
            ## saveTradeStandardDeviation(data = .$stdData)
            selectSaveSelection(data = .$balanceData)
        } %>%
    saveBalancedData(data = .)

