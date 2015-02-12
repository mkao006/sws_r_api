## NOTE (Michael): Just need to read data from the completed trade
##                 flow and then aggregate and write back to total
##                 trade.


suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
})

## Year should be a paramameter selected.
selectedYear = "2010"


## Setting up variables
reportingCountryVar = "reportingCountryM49"
partnerCountryVar = "partnerCountryM49"
yearVar = "timePointYears"
itemVar = "measuredItemHS"
elementVar = "measuredElementTrade"
valuePrefix = "Value_"
flagPrefix = "flagTrade_"
reverseTradePrefix = "reverse_"


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

## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
        )
}


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


getComtradeMirroredData = function(){}

## TODO (Michael): Need to check how we should consolidate the flag,
##                 or if necessary.
consolidateTradeFlow = function(balancedData, importQuantity, exportQuantity,
    importValue, exportValue, reportingCountryVar, itemVar, yearVar,
    consolidatedFlag = "c"){
    consolidatedData =
        balancedData[, list(importQuantity = sum(get(importQuantity)),
                            exportQuantity = sum(get(exportQuantity)),
                            importValue = sum(get(importValue)),
                            exportValue = sum(get(exportValue))),
                     by = c(reportingCountryVar, itemVar, yearVar)]
    ## Assign flags
    consolidatedData[, `:=`(
        sapply(c(importQuantity, exportQuantity, importValue, exportValue),
               FUN = function(x) gsub(valuePrefix, flagPrefix, x)),
        consolidatedFlag)]
    setnames(consolidatedData,
             old = c(reportingCountryVar, "importQuantity", "exportQuantity",
                 "importValue", "exportValue"),
             new = c("geographicAreaM49", importQuantity, exportQuantity,
                 importValue, exportValue))
    consolidatedData
}


saveConsolidatedData = function(consolidatedData){
    valueColumns = c(import_quantity, import_value, export_quantity, export_value)
    flagColumns = sapply(valueColumns,
        FUN = function(x) gsub(valuePrefix, flagPrefix, x))
    pairColumns = vector("character", length = length(valueColumns) * 2)
    pairColumns[as.logical(1:length(pairColumns) %% 2)] = valueColumns
    pairColumns[!as.logical(1:length(pairColumns) %% 2)] = flagColumns
    consolidatedData =
        consolidatedData[,c("geographicAreaM49", itemVar, yearVar, pairColumns),
                         with = FALSE]
    consolidatedData[, timePointYears := as.character(timePointYears)]
    if(NROW(consolidatedData) > 0)
        SaveData(domain = "trade", dataset = "total_trade",
                 data = consolidatedData, normalized = FALSE)
}


selectedItems = swsContext.datasets[[1]]@dimensions$measuredItemHS@keys
comtradeToStandardM49Mapping = getComtradeStandard49Mapping()
## i = "1001"
for(i in selectedItems){
    try({
        ## Consolidate the data
        mirroredData = getComtradeMirroredData()

        
        ## NOTE (Michael): This is to be discussed which data set should
        ##                 be consolidated and ultimate feed into the Food
        ##                 Balance Sheet.
        consolidatedData = 
            mirroredData %>%
            consolidateTradeFlow(balancedData = .,
                                 importQuantity = import_quantity,
                                 exportQuantity = export_quantity,
                                 importValue = import_value,
                                 exportValue = export_value,
                                 reportingCountryVar = reportingCountryVar,
                                 itemVar = itemVar,
                                 yearVar = yearVar,
                                 consolidatedFlag = "") %>%
           comtradeM49ToStandardM49(comtradeData = .,
                                    comtradeM49Name = "geographicAreaM49",
                                    standardM49Name = "geographicAreaM49",
                                    translationData = comtradeToStandardM49Mapping,
                                    translationComtradeM49Name =
                                        "comtrade_code",
                                    translationStandardM49Name =
                                        "translation_code",
                                    aggregateKey = c(itemVar, yearVar),
                                    aggregateValueCol =
                                        grep(valuePrefix, colnames(.),
                                             value = TRUE))
        
        
        
        ## Save consolidated data back
        consolidatedData %>%
            saveConsolidatedData(consolidatedData = .)
    }
        )
}
