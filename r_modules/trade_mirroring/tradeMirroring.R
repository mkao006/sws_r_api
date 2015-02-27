## NOTE (Michael): Need to request flag for raw data, the flag are missing.

suppressMessages({
    library(faosws)
    library(faoswsUtil)
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
        token = "09aeec5c-2cf5-46f7-90e7-f8b4bdc6dc93"
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


getComtradeRawData = function(measuredItemHSCode){
    dimensions =
        list(Dimension(name = "reportingCountryM49",
                       keys = as.character(allReportingCountryCode)),
             Dimension(name = "partnerCountryM49",
                       keys = as.character(allPartnerCountryCode)),
             Dimension(name = "measuredItemHS",
                       keys = as.character(measuredItemHSCode)),
             ## Dimension(name = "measuredElementTrade",
             ##           keys = as.character(with(elementTable,
             ##               unique(na.omit(c(import, reimport,
             ##                                export, reexport)))))),
             Dimension(name = "measuredElementTrade",
                       keys = as.character(c("5600", "5621", "5630", "5612",
                           "5622", "5900", "5921", "5930", "5912", "5922"))),
             Dimension(name = "timePointYears",
                       keys = as.character(selectedYear)))

    newKey = DatasetKey(domain = "trade", dataset = "ct_raw_tf",
        dimensions = dimensions)

    newPivot = c(
        Pivoting(code = "reportingCountryM49", ascending = TRUE),
        Pivoting(code = "partnerCountryM49", ascending = TRUE),
        Pivoting(code = "measuredItemHS", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    comtradeRaw = GetData(key = newKey, normalized = FALSE, pivoting = newPivot)
    comtradeRaw[,timePointYears := as.numeric(timePointYears)]
    setkeyv(comtradeRaw, cols = c("reportingCountryM49", "partnerCountryM49",
                             "measuredItemHS", "timePointYears"))
    ## NOTE (Michael): The meta data shows which procedure was done to
    ##                 obtain the value. This is replaced by flag

    comtradeRaw[, `:=`(c(grep(valuePrefix, colnames(comtradeRaw), value = TRUE)),
                       lapply(c(grep(valuePrefix, colnames(comtradeRaw),
                                     value = TRUE)),
                              FUN = function(x) as.numeric(.SD[[x]])))]
    comtradeRaw
}

addRetradeToTrade = function(data, importQuantity, reimportQuantity,
    exportQuantity, reexportQuantity, importValue, reimportValue,
    exportValue, reexportValue){
    missingCol =
        setdiff(c(importQuantity, reimportQuantity, exportQuantity,
                  reexportQuantity, importValue, reimportValue, exportValue,
                  reexportValue),
                colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    data[, `:=`(c(importQuantity, exportQuantity, importValue, exportValue),
                list(ifelse(apply(is.na(.SD[, c(importQuantity, reimportQuantity),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(importQuantity, reimportQuantity),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(exportQuantity, reexportQuantity),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(exportQuantity, reexportQuantity),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(importValue, reimportValue),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(importValue, reimportValue),
                                        with = FALSE], na.rm = TRUE)),
                     ifelse(apply(is.na(.SD[, c(exportValue, reexportValue),
                                            with = FALSE]), 1, all), NA,
                            rowSums(.SD[, c(exportValue, reexportValue),
                                        with = FALSE], na.rm = TRUE))))]
    ## NOTE (Michael): Make sure the type are numeric
    valueColumns = grep(valuePrefix, colnames(data), value = TRUE)
    data[, `:=`(c(valueColumns),
                lapply(valueColumns, FUN = function(x) as.numeric(.SD[[x]])))]
    
    data[, `:=`(c(reimportQuantity, reexportQuantity,
                  reimportValue, reexportValue,
                  gsub(valuePrefix, flagPrefix, reimportQuantity),
                  gsub(valuePrefix, flagPrefix, reexportQuantity),
                  gsub(valuePrefix, flagPrefix, reimportValue),
                  gsub(valuePrefix, flagPrefix, reexportValue)), NULL)]
    ## NOTE (Michael): What do we do with the retrades in the data base?
    data
}


removeSelfTrade = function(data, reportingCountry, partnerCountry){
    noSelfTrade = data[which(data[[reportingCountry]] != data[[partnerCountry]]), ]
    noSelfTrade
}


removeInconsistentQuantityValue = function(data, quantity, value){
    data[data[[quantity]] == 0 & data[[value]] != 0,
         `:=`(c(quantity), NA)]
    data[data[[quantity]] != 0 & data[[value]] == 0,
         `:=`(c(value), NA)]
    data
}


mirrorTrade = function(data, reportingCountry, partnerCountry, reverseTradePrefix,
                       valueColumns, flagColumns, mirrorFlag = "/"){
    base = copy(data)
    tmp = data[, !flagColumns, with = FALSE]

    ## create reversion of the reporting and partner country, and
    ## create the reverse trade flow for mirroring process.
    reverseReportingName = paste0(reverseTradePrefix, reportingCountry)
    reversePartnerName = paste0(reverseTradePrefix, partnerCountry)
    setnames(tmp, old = c(reportingCountry, partnerCountry),
             new = c(reverseReportingName, reversePartnerName))    
    base[, `:=`(c(reverseReportingName, reversePartnerName),
                list(.SD[[partnerCountry]], .SD[[reportingCountry]]))]
    setnames(tmp, old = valueColumns,
             new = paste0(reverseTradePrefix, valueColumns))

    ## Merge the two dataset to obtain the full global trade flow
    mirroredTrade = merge(base, tmp,
        by = intersect(colnames(base), colnames(tmp)), all = TRUE)
    ## TODO (Michael): Values should be filled here to ensure the base
    ##                 is symmetrical for validation.
    mirroredTrade[is.na(mirroredTrade[[reportingCountry]]),
                `:=`(c(reportingCountry), .SD[[reversePartnerName]])]
    mirroredTrade[is.na(mirroredTrade[[partnerCountry]]),
                `:=`(c(partnerCountry), .SD[[reverseReportingName]])]

    ## Mirror the value based on the element table
    ##
    ## TODO (Michael): Assign flag when mirroring
    mirroredTrade[is.na(mirroredTrade[[import_quantity]]) &
                  !is.na(mirroredTrade[[reverse_export_quantity]]),
                  `:=`(c(import_quantity,
                         gsub(valuePrefix, flagPrefix, import_quantity)),
                       list(.SD[[reverse_export_quantity]], mirrorFlag))]
    mirroredTrade[is.na(mirroredTrade[[import_value]]) &
                  !is.na(mirroredTrade[[reverse_export_value]]),
                  `:=`(c(import_value,
                         gsub(valuePrefix, flagPrefix, import_value)),
                       list(.SD[[reverse_export_value]], mirrorFlag))]

    mirroredTrade[is.na(mirroredTrade[[export_quantity]]) &
                  !is.na(mirroredTrade[[reverse_import_quantity]]),
                  `:=`(c(export_quantity,
                         gsub(valuePrefix, flagPrefix, export_quantity)),
                       list(.SD[[reverse_import_quantity]], mirrorFlag))]
    mirroredTrade[is.na(mirroredTrade[[export_value]]) &
                  !is.na(mirroredTrade[[reverse_import_value]]),
                  `:=`(c(export_value,
                         gsub(valuePrefix, flagPrefix, export_value)),
                       list(.SD[[reverse_import_value]], mirrorFlag))]

    
    ## NOTE (Michael): Flags are not needed for the reverse trade
    mirroredTrade[is.na(mirroredTrade[[reverse_import_quantity]]) &
                  !is.na(mirroredTrade[[export_quantity]]),
                  `:=`(c(reverse_import_quantity), list(.SD[[export_quantity]]))]
    mirroredTrade[is.na(mirroredTrade[[reverse_import_value]]) &
                  !is.na(mirroredTrade[[export_value]]),
                  `:=`(c(reverse_import_value), list(.SD[[export_value]]))]

    
    mirroredTrade[is.na(mirroredTrade[[reverse_export_quantity]]) &
                  !is.na(mirroredTrade[[import_quantity]]),
                  `:=`(c(reverse_export_quantity), list(.SD[[import_quantity]]))]
    mirroredTrade[is.na(mirroredTrade[[reverse_export_value]]) &
                  !is.na(mirroredTrade[[import_value]]),
                  `:=`(c(reverse_export_value), list(.SD[[import_value]]))]
    mirroredTrade
}


calculateUnitValue = function(data, importUnitValue, importUnitValueFlag,
    importTradeValue, importTradeQuantity, exportUnitValue, exportUnitValueFlag,
    exportTradeValue, exportTradeQuantity, calculatedFlag = "c"){

    missingCol = setdiff(c(importUnitValue, importTradeValue, importTradeQuantity,
        exportUnitValue, exportUnitValue, exportTradeQuantity), colnames(data))
    if(length(missingCol) > 0)
        data[, `:=`(c(missingCol), as.numeric(NA))]
    
    data[!is.na(data[[importTradeValue]]) & !is.na(data[[importTradeQuantity]]),
         `:=`(c(importUnitValue, importUnitValueFlag),
                list(computeRatio(.SD[[importTradeValue]],
                                  .SD[[importTradeQuantity]]),
                     calculatedFlag))]

    data[!is.na(data[[exportTradeValue]]) & !is.na(data[[exportTradeQuantity]]),
         `:=`(c(exportUnitValue, exportUnitValueFlag),
                list(computeRatio(.SD[[exportTradeValue]],
                                  .SD[[exportTradeQuantity]]),
                     calculatedFlag))]
    
    data
}

subsetRequiredData = function(data){
    valueColumns = c(import_quantity, import_value, export_quantity, export_value,
                     import_unit_value, export_unit_value)
    flagColumns = sapply(valueColumns,
        FUN = function(x) gsub(valuePrefix, flagPrefix, x))
    colIndex = which(colnames(data) %in%
        c(reportingCountryVar, partnerCountryVar, itemVar, yearVar,
          valueColumns, flagColumns))
    mirroredData = data[, colIndex, with = FALSE]
    setcolorder(mirroredData,
                neworder = c(reportingCountryVar, partnerCountryVar,
                    colnames(mirroredData)[!colnames(mirroredData) %in%
                                           c(reportingCountryVar,
                                             partnerCountryVar)]))
    mirroredData[, timePointYears := as.character(timePointYears)]
    mirroredData
}

saveMirroredTradeData = function(requiredData){

    ## NOTE (Michael): We save back to complete trade flow as the
    ##                 mirror completes all the trade flow possibly
    ##                 observed.
    if(NROW(mirroredData) > 0)
        SaveData(domain = "trade", dataset = "completed_tf",
                 data = requiredData, normalized = FALSE)
}


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
                                mirrorFlag = "") %>%
                calculateUnitValue(data = .,
                                   importUnitValue = import_unit_value,
                                   importUnitValueFlag =
                                       gsub(valuePrefix, flagPrefix,
                                            import_unit_value),
                                   importTradeValue = import_value,
                                   importTradeQuantity = import_quantity,
                                   exportUnitValue = export_unit_value,
                                   exportUnitValueFlag =
                                       gsub(valuePrefix, flagPrefix,
                                            export_unit_value),
                                   exportTradeValue = export_value,
                                   exportTradeQuantity = export_quantity,
                                   calculatedFlag = "") %>%
             ## Calculate the unit value for reverse trade
                 calculateUnitValue(data = .,
                                    importUnitValue = reverse_import_unit_value,
                                    importUnitValueFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             reverse_import_unit_value),
                                    importTradeValue = reverse_import_value,
                                    importTradeQuantity = reverse_import_quantity,
                                    exportUnitValue = reverse_export_unit_value,
                                    exportUnitValueFlag =
                                        gsub(valuePrefix, flagPrefix,
                                             reverse_export_unit_value),
                                    exportTradeValue = reverse_export_value,
                                    exportTradeQuantity = reverse_export_quantity,
                                    calculatedFlag = "")

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

            ## mirroredData %>%
            ##     subsetRequiredData(data = .) %>%
            ##         write.csv(., file = paste0("item_", i, ".csv"), na = "",
            ##                   row.names = FALSE)
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
