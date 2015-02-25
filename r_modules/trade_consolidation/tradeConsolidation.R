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

    mirroredData = GetData(key = newKey, normalized = FALSE, pivoting = newPivot)
    mirroredData
}



consolidateTradeFlow = function(mirroredData, key, consolidateFlag = "c"){
    setnames(mirroredData, reportingCountryVar, standardCountryVar)
    if(missing(key))
        key = c(standardCountryVar, itemVar, yearVar)

    valueColumns = grep(valuePrefix, colnames(mirroredData), value = TRUE)
    flagColumns = grep(flagPrefix, colnames(mirroredData), value = TRUE)
    consolidatedData =
        mirroredData[, lapply(valueColumns,
                              FUN = function(x) sum(as.numeric(.SD[[x]]))),
                     by = key]
    setnames(consolidatedData,
             old = paste0("V", 1:length(valueColumns)),
             new = valueColumns)
    consolidatedData[, `:=`(c(flagColumns), consolidateFlag)]
    setkeyv(consolidatedData, key)
    consolidatedData
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
        setnames(translated,
                 old = translationStandardM49Name,
                 new = standardM49Name)
        return(translated)
    } else {
        return(comtradeData)
    }
}

saveConsolidatedData = function(consolidatedData){
    if(is.null(key(consolidatedData)))
        setkeyv(x = consolidatedData,
                cols = c("geographicAreaM49", "measuredItemHS", "timePointYears"))

    valueColumns = grep(valuePrefix, colnames(consolidatedData), value = TRUE)
    flagColumns = grep(flagPrefix, colnames(consolidatedData), value = TRUE)
    pairColumns = vector("character", length = length(valueColumns) * 2)
    pairColumns[as.logical(1:length(pairColumns) %% 2)] = valueColumns
    pairColumns[!as.logical(1:length(pairColumns) %% 2)] = flagColumns
    setcolorder(consolidatedData, c(key(consolidatedData), pairColumns))
    consolidatedData[, timePointYears := as.character(timePointYears)]
    if(NROW(consolidatedData) > 0)
        SaveData(domain = "trade", dataset = "total_trade",
                 data = consolidatedData, normalized = FALSE)
}

## Get the completely mirrored data



## Consolidate the data

consolidate =
    try({
        ## Get the raw data
        if(verbose){
            cat("Extracting raw data\n")
            currentTime = Sys.time()
        }
        mirroredData = getComtradeMirroredData(swsContext.datasets[[1]])

        ## Consolidate the data
        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
            cat("Performing Consolidation\n")
        }
        consolidatedData =
            copy(mirroredData) %>%
            consolidateTradeFlow(mirroredData = .,
                                 consolidateFlag = "") %>%
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


        ## Save consolidated data back
        consolidatedData %>%
            saveConsolidatedData(consolidatedData = .)

        if(verbose){
            endTime = Sys.time()
            timeUsed = endTime - currentTime
            cat("\t Time used:", timeUsed, attr(timeUsed, "units") , "\n")
            currentTime = endTime
        }
    })

if(inherits(consolidate, "try-error")){
    print(paste0("Consolidation Module Failed\n", consolidate[1]))
} else {
    print("Consolidation Module Executed Successfully")
}
