suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(caret)
})

## Year should be a paramameter selected.
selectedYear = "2010"


## Setting up variables
standardCountryVar = "geographicAreaM49"
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
        token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
        )
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


## Function to get current selection of trade Data
getSelectedTotalTradeData = function(){}


## Get all the total trade data if to update the model.
getAllTotalTradeData = function(){}

