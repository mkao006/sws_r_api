## NOTE (Michael): Need to request flag for raw data, the flag are missing.

## Split this module in 3?

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


## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
        )
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


comtradeToStandardM49Mapping = getComtradeStandard49Mapping()

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



validationByMirrorValue = function(importUnitValue, exportUnitValue,
    reverseImportUnitValue, reverseExportUnitValue, importUnitValueFlag,
    exportUnitValueFlag, ratioBoundary, log = TRUE, plot = FALSE,
    validatedFlag = "v"){

    
    if(log){
        newImportUnitValue = log(importUnitValue)
        newExportUnitValue = log(exportUnitValue)
        newReverseImportUnitValue = log(reverseImportUnitValue)
        newReverseExportUnitValue = log(reverseExportUnitValue)
    } else {
        newImportUnitValue = importUnitValue
        newExportUnitValue = exportUnitValue
        newReverseImportUnitValue = reverseImportUnitValue
        newReverseExportUnitValue = reverseExportUnitValue
    }
    newImportUnitValueFlag = importUnitValueFlag
    newExportUnitValueFlag = exportUnitValueFlag

    bilateralImport = c(newImportUnitValue, newReverseImportUnitValue)
    bilateralExport = c(newReverseExportUnitValue, newExportUnitValue)

    ## Checking whether there are any paired unitvalue available
    n.pairs = length(na.omit(rowSums(cbind(bilateralImport, bilateralExport))))
    if(n.pairs > 0){
        pr = prcomp(~bilateralExport + bilateralImport)
        slope = pr$rotation[2, 1] / pr$rotation[1, 1]
        intercept = pr$center[2] - slope*pr$center[1]

        
        valueRatioImportUnitValue =
            (newReverseExportUnitValue - intercept)/newImportUnitValue
        valueRatioExportUnitValue =
            (newExportUnitValue - intercept)/newReverseImportUnitValue

        badRatioImportUnitValue =
            which(valueRatioImportUnitValue > slope * ratioBoundary |
                      valueRatioImportUnitValue < slope/ratioBoundary)

        badRatioExportUnitValue =
            which(valueRatioExportUnitValue > slope * ratioBoundary |
                      valueRatioExportUnitValue < slope/ratioBoundary)
        
        svec = c(1, slope)
        importUnitValueBasedOnExpectedRatio  =
            ((matrix(c(newImportUnitValue, newReverseExportUnitValue - intercept),
                     nc = 2) %*% svec)/c(svec %*% svec)) %*% matrix(svec, nc = 2)
        exportUnitValueBasedOnExpectedRatio  =
            ((matrix(c(newReverseImportUnitValue, newExportUnitValue - intercept),
                     nc = 2) %*% svec)/c(svec %*% svec)) %*% matrix(svec, nc = 2)    

        newImportUnitValue[badRatioImportUnitValue] =
            importUnitValueBasedOnExpectedRatio[badRatioImportUnitValue, 1]
        newReverseExportUnitValue[badRatioImportUnitValue] =
            importUnitValueBasedOnExpectedRatio[badRatioImportUnitValue, 2] + intercept


        newReverseImportUnitValue[badRatioExportUnitValue] =
            exportUnitValueBasedOnExpectedRatio[badRatioExportUnitValue, 1]
        newExportUnitValue[badRatioExportUnitValue] =
            exportUnitValueBasedOnExpectedRatio[badRatioExportUnitValue, 2] + intercept
        
        newImportUnitValueFlag[badRatioImportUnitValue] = validatedFlag
        newExportUnitValueFlag[badRatioExportUnitValue] = validatedFlag

        if(plot){
            plot(bilateralImport, bilateralExport, pch = 19, col = "red")
            abline(a = intercept, b = slope)
            abline(a = intercept, b = slope * ratioBoundary, col = "red")
            abline(a = intercept, b = slope/ratioBoundary, col = "red")        
            points(c(newImportUnitValue, newReverseImportUnitValue),
                   c(newReverseExportUnitValue, newExportUnitValue),
                   col = "blue", pch = 19)
        }

        if(log){
            newImportUnitValue = exp(newImportUnitValue)
            newExportUnitValue = exp(newExportUnitValue)
            newReverseImportUnitValue = exp(newReverseImportUnitValue)
            newReverseExportUnitValue = exp(newReverseExportUnitValue)
        }
        
        list(newImportUnitValue, newImportUnitValueFlag,
             newExportUnitValue, newExportUnitValueFlag,
             newReverseExportUnitValue, newReverseImportUnitValue)
    } else {

        ## If no pair exist for mirror validation, then just return
        ## original value.
        warning("Mirror Validation was not possible, due to zero matching pairs")
        list(importUnitValue, importUnitValueFlag,
             exportUnitValue, exportUnitValueFlag,
             reverseExportUnitValue, reverseImportUnitValue)
    }
}

validationByRange = function(value, flag, validatedFlag = "v", log = TRUE){
    if(log){
        newValue = log(value)
    } else {
        newValue = value
    }
    q = quantile(value, probs = c(0.25, 0.75), na.rm = TRUE)
    min = q[1] - 1.5 * diff(q)
    max = q[2] + 1.5 * diff(q)
    badValue = which(value > max | value < min)
    newValue[badValue] = median(newValue[-badValue], na.rm = TRUE)
    if(log)
        newValue = exp(newValue)
    newFlag = flag
    newFlag[badValue] = validatedFlag
    list(newValue, newFlag)
}


validation = function(data, importUnitValue, exportUnitValue,
    reverseImportUnitValue, reverseExportUnitValue, importUnitValueFlag,
    exportUnitValueFlag, validatedFlag = "v",
    ratioBoundary = 3, log = TRUE, plot = FALSE){
    valid = copy(data)

    valid[, `:=`(c(importUnitValue, importUnitValueFlag,
                    exportUnitValue, exportUnitValueFlag,
                    reverseExportUnitValue, reverseImportUnitValue),
                  validationByMirrorValue(importUnitValue = .SD[[importUnitValue]],
                                          exportUnitValue = .SD[[exportUnitValue]],
                                          reverseImportUnitValue =
                                              .SD[[reverseImportUnitValue]],
                                          reverseExportUnitValue =
                                              .SD[[reverseExportUnitValue]],
                                          importUnitValueFlag =
                                              .SD[[importUnitValueFlag]],
                                          exportUnitValueFlag =
                                              .SD[[exportUnitValueFlag]],
                                          ratioBoundary = ratioBoundary,
                                          log = log,
                                          plot = plot))]
    ## NOTE(Michael): Validation by range is not currently applied. It
    ## should be only applied to primary commodities such as wheat
    ## grain. Livestocks should not be validated by range as well.
    ##
    ## valid[, `:=`(c(importUnitValue, importUnitValueFlag),
    ##              validationByRange(value = .SD[[importUnitValue]],
    ##                                flag = .SD[[importUnitValueFlag]],
    ##                                log = log))]
    ## valid[, `:=`(c(exportUnitValue, exportUnitValueFlag),
    ##              validationByRange(value = .SD[[exportUnitValue]],
    ##                                flag = .SD[[exportUnitValueFlag]],
    ##                                log = log))]
    valid
}


imputeUnitValue = function(data, unitValue, unitValueFlag, mirrorUnitValue,
    imputationFlag = "i"){
    imputed = copy(data)
    imputed[is.na(imputed[[mirrorUnitValue]]),
            `:=`(c(mirrorUnitValue),
                 median(imputed[[mirrorUnitValue]], na.rm = TRUE))]
    imputed[is.na(imputed[[unitValue]]),
            `:=`(c(unitValue, unitValueFlag),
                 list(median(imputed[[unitValue]], na.rm = TRUE), imputationFlag))]
    imputed
}

updateTradeQuantity = function(data, unitValue, value, quantity, unitValueFlag,
    quantityFlag,  calculatedFlag = "c"){
    updatedTradeQuantity = copy(data)

    ## Update trade quantity if unit value is not calculated.
    updatedTradeQuantity[!is.na(updatedTradeQuantity[[value]]) & 
                         !is.na(updatedTradeQuantity[[unitValue]]) &
                         is.na(updatedTradeQuantity[[quantity]]) &
                         updatedTradeQuantity[[unitValueFlag]] != calculatedFlag,
                         `:=`(c(quantity, quantityFlag),
                              list(computeRatio(.SD[[value]], .SD[[unitValue]]),
                                   .SD[[unitValueFlag]]))]
    updatedTradeQuantity    
}

## TODO (Michael): This should be based only on the reported
##                 quantities, not mirrored.
calculateReliability = function(data, importQuantity, exportQuantity,
    reverseImportQuantity, reverseExportQuantity, reportingCountry, partnerCountry,
    pctTolerance){

    ## TODO (Michael): Discard worst discrepancy
    reliability =
        data[, (sum(abs(.SD[[importQuantity]] - .SD[[reverseExportQuantity]])/
                        .SD[[importQuantity]] <= pctTolerance, na.rm = TRUE) +
               (sum(abs(.SD[[exportQuantity]] - .SD[[reverseImportQuantity]])/
                        .SD[[exportQuantity]] <= pctTolerance, na.rm = TRUE)))/
                            (2 * .N),
             by = reportingCountry]
    setnames(reliability, old = "V1", new = "reportingReliability")
    reportingReliability = merge(data, reliability, by = reportingCountry,
                                 all.x = TRUE)
    setnames(reliability, old = c(reportingCountry, "reportingReliability"),
             new = c(partnerCountry, "partnerReliability"))
    finalReliability = merge(reportingReliability, reliability,
        by = partnerCountry, all.x = TRUE)
    finalReliability
}



balanceTradeQuantity = function(data, importQuantity, exportQuantity,
    reverseImportQuantity, reverseExportQuantity, reportingReliability,
    partnerReliability, pctTolerance){
    balanced = copy(data)
    balanced[reportingReliability < partnerReliability &
             (abs(balanced[[importQuantity]] - balanced[[reverseExportQuantity]])/
                  balanced[[importQuantity]] > pctTolerance),
             `:=`(c(importQuantity), .SD[[reverseExportQuantity]])]
    balanced[reportingReliability < partnerReliability &
             (abs(balanced[[exportQuantity]] - balanced[[reverseImportQuantity]])/
                  balanced[[exportQuantity]]> pctTolerance),
             `:=`(c(exportQuantity), .SD[[reverseImportQuantity]])]

    ## NOTE (Michael): If the reliability are tied, then we take the 
    balanced[reportingReliability == partnerReliability &
             (abs(balanced[[importQuantity]] - balanced[[reverseExportQuantity]])/
                  balanced[[importQuantity]] > pctTolerance),
             `:=`(c(importQuantity), .SD[[reverseExportQuantity]])]
    
    balanced
}


updateTradeValue = function(data, unitValue, value, valueFlag, quantity,
    calculatedFlag = "c"){

    updatedTradeValue = copy(data)

    updatedTradeValue[, tmp := .SD[[quantity]] * .SD[[unitValue]]]
    updatedTradeValue[tmp != updatedTradeValue[[value]],
                      `:=`(c(value, valueFlag), list(tmp, calculatedFlag))]
    updatedTradeValue[, tmp := NULL]
    updatedTradeValue
}

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


## TODO (Michael): Need to change the dataset name
saveMirroredTradeData = function(data){
    valueColumns = c(import_quantity, import_value, export_quantity, export_value)
    flagColumns = sapply(valueColumns,
        FUN = function(x) gsub(valuePrefix, flagPrefix, x))
    colIndex = which(colnames(data) %in%
        c(reportingCountryVar, partnerCountryVar, itemVar, yearVar,
          valueColumns, flagColumns))
    mirroredData = data[, colIndex, with = FALSE]
    setcolorder(mirroredData,
                neworder = c(reportingCountryVar, partnerCountryVar,
                    colnames(mirroredData)[!colnames(mirroredData) %in%
                                               c(reportingCountryVar, partnerCountryVar)]))
    mirroredData[, timePointYears := as.character(timePointYears)]
    ## NOTE (Michael): We save back to complete trade flow as the
    ##                 mirror completes all the trade flow possibly
    ##                 observed.
    if(NROW(mirroredData) > 0)
        SaveData(domain = "trade", dataset = "completed_tf",
                 data = mirroredData, normalized = FALSE)
        ## SaveData(domain = "trade", dataset = "ct_raw_tf",
        ##          data = mirroredData, normalized = FALSE)
}


saveValidatedTradeData = function(data){
    valueColumns = c(import_quantity, import_value, export_quantity, export_value)
    flagColumns = sapply(valueColumns,
        FUN = function(x) gsub(valuePrefix, flagPrefix, x))
    colIndex = which(colnames(data) %in%
        c(reportingCountryVar, partnerCountryVar, itemVar, yearVar,
          valueColumns, flagColumns))
    validatedData = data[, colIndex, with = FALSE]
    validatedData[, timePointYears := as.character(timePointYears)]
    if(NROW(validatedData) > 0)
        SaveData(domain = "trade", dataset = "consolidated_tf",
                 data = validatedData, normalized = FALSE)
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
## i = "1001"
for(i in selectedItems){
    ## print(i)

    rawData =
        getComtradeRawData(measuredItemHSCode = i) %>%
        removeSelfTrade(data = ., reportingCountry = reportingCountryVar,
                        partnerCountry = partnerCountryVar) %>%
        removeInconsistentQuantityValue(data = ., quantity = import_quantity,
                                        value = import_value) %>%
        removeInconsistentQuantityValue(data = ., quantity = export_quantity,
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
                    valueColumns = grep(valuePrefix, colnames(.), value = TRUE),
                    flagColumns = grep(flagPrefix, colnames(.), value = TRUE),
                    mirrorFlag = "") %>%
        calculateUnitValue(data = .,
                           importUnitValue = import_unit_value,
                           importUnitValueFlag =
                               gsub(valuePrefix, flagPrefix, import_unit_value),
                           importTradeValue = import_value,
                           importTradeQuantity = import_quantity,
                           exportUnitValue = export_unit_value,
                           exportUnitValueFlag =
                               gsub(valuePrefix, flagPrefix, export_unit_value),
                           exportTradeValue = export_value,
                           exportTradeQuantity = export_quantity,
                           calculatedFlag = "") %>%    
        calculateUnitValue(data = .,
                           importUnitValue = reverse_import_unit_value,
                           importUnitValueFlag =
                               gsub(valuePrefix, flagPrefix, reverse_import_unit_value),
                           importTradeValue = reverse_import_value,
                           importTradeQuantity = reverse_import_quantity,
                           exportUnitValue = reverse_export_unit_value,
                           exportUnitValueFlag =
                               gsub(valuePrefix, flagPrefix, reverse_export_unit_value),
                           exportTradeValue = reverse_export_value,
                           exportTradeQuantity = reverse_export_quantity,
                           calculatedFlag = "")

    ## ## Save mirrored data back
    mirroredData %>%
        saveMirroredTradeData(data = .)

    ## Consolidate the data
    ##
    ## NOTE (Michael): This is to be discussed which data set should
    ##                 be consolidated and ultimate feed into the Food
    ##                 Balance Sheet.
    ## consolidatedData = 
    ##     mirroredData %>%
    ##     consolidateTradeFlow(balancedData = .,
    ##                          importQuantity = import_quantity,
    ##                          exportQuantity = export_quantity,
    ##                          importValue = import_value,
    ##                          exportValue = export_value,
    ##                          reportingCountryVar = reportingCountryVar,
    ##                          itemVar = itemVar,
    ##                          yearVar = yearVar,
    ##                          consolidatedFlag = "") %>%
    ##    comtradeM49ToStandardM49(comtradeData = .,
    ##                              comtradeM49Name = "geographicAreaM49",
    ##                              standardM49Name = "geographicAreaM49",
    ##                              translationData = comtradeToStandardM49Mapping,
    ##                              translationComtradeM49Name = "comtrade_code",
    ##                              translationStandardM49Name = "translation_code",
    ##                              aggregateKey = c(itemVar, yearVar),
    ##                              aggregateValueCol =
    ##                                  grep(valuePrefix, colnames(.), value = TRUE))

    
    
    ## ## Save consolidated data back
    ## consolidatedData %>%
    ##     saveConsolidatedData(consolidatedData = .)

    ## ## Validated the data
    ## ## TODO (Michael): Need to change the validated flag back to "v" after
    ## ##                 adding it as a valid flag.
    ## validatedData =
    ##     mirroredData %>%
    ##     validation(data = .,
    ##                importUnitValue = import_unit_value,
    ##                exportUnitValue = export_unit_value,
    ##                reverseImportUnitValue = reverse_import_unit_value,
    ##                reverseExportUnitValue = reverse_export_unit_value,
    ##                importUnitValueFlag =
    ##                    gsub(valuePrefix, flagPrefix, import_unit_value),
    ##                exportUnitValueFlag =
    ##                    gsub(valuePrefix, flagPrefix, export_unit_value),
    ##                ratioBoundary = 1.1, log = TRUE, plot = FALSE,
    ##                validatedFlag = "") %>%
    ##     imputeUnitValue(data = .,
    ##                     unitValue = import_unit_value,
    ##                     unitValueFlag = gsub(valuePrefix, flagPrefix, import_unit_value),
    ##                     mirrorUnitValue = reverse_export_unit_value,
    ##                     imputationFlag = "") %>%
    ##     imputeUnitValue(data = .,
    ##                     unitValue = export_unit_value,
    ##                     unitValueFlag = gsub(valuePrefix, flagPrefix, export_unit_value),
    ##                     mirrorUnitValue = reverse_import_unit_value,
    ##                     imputationFlag = "")

    
    ## ## Balance the data
    ## balancedData =
    ##     validatedData %>%
    ##     updateTradeQuantity(data = .,
    ##                         unitValue = import_unit_value,
    ##                         unitValueFlag =
    ##                             gsub(valuePrefix, flagPrefix, import_unit_value),
    ##                         value = import_value,
    ##                         quantity = import_quantity,
    ##                         quantityFlag =
    ##                             gsub(valuePrefix, flagPrefix, import_quantity),
    ##                         calculatedFlag = "") %>%
    ##     updateTradeQuantity(data = .,
    ##                         unitValue = export_unit_value,
    ##                         unitValueFlag =
    ##                             gsub(valuePrefix, flagPrefix, export_unit_value),
    ##                         value = export_value,
    ##                         quantity = export_quantity,
    ##                         quantityFlag =
    ##                             gsub(valuePrefix, flagPrefix, export_quantity),
    ##                         calculatedFlag = "") %>%
    ##     updateTradeQuantity(data = .,
    ##                         unitValue = reverse_import_unit_value,
    ##                         unitValueFlag =
    ##                             gsub(valuePrefix, flagPrefix, reverse_import_unit_value),
    ##                         value = reverse_import_value,
    ##                         quantity = reverse_import_quantity,
    ##                         quantityFlag =
    ##                             gsub(valuePrefix, flagPrefix, reverse_import_quantity),
    ##                         calculatedFlag = "") %>%
    ##     updateTradeQuantity(data = .,
    ##                         unitValue = reverse_export_unit_value,
    ##                         unitValueFlag =
    ##                             gsub(valuePrefix, flagPrefix, reverse_export_unit_value),
    ##                         value = reverse_export_value,
    ##                         quantity = reverse_export_quantity,
    ##                         quantityFlag =
    ##                             gsub(valuePrefix, flagPrefix, reverse_export_quantity),
    ##                         calculatedFlag = "") %>%
    ##     calculateReliability(data = .,
    ##                          importQuantity = import_quantity,
    ##                          exportQuantity = export_quantity,
    ##                          reverseImportQuantity = reverse_import_quantity,
    ##                          reverseExportQuantity = reverse_export_quantity,
    ##                          reportingCountry = reportingCountryVar,
    ##                          partnerCountry = partnerCountryVar,
    ##                          pctTolerance = 0.1) %>% 
    ##     balanceTradeQuantity(data = .,
    ##                          importQuantity = import_quantity,
    ##                          exportQuantity = export_quantity,
    ##                          reverseImportQuantity = reverse_import_quantity,
    ##                          reverseExportQuantity = reverse_export_quantity,
    ##                          reportingReliability = reportingCountryVar,
    ##                          partnerReliability = partnerCountryVar,
    ##                          pctTolerance = 0.1) %>%
    ##     updateTradeValue(data = .,
    ##                      unitValue = import_unit_value,
    ##                      value = import_value,
    ##                      valueFlag = gsub(valuePrefix, flagPrefix, import_value),
    ##                      quantity = import_quantity,
    ##                      calculatedFlag = "") %>%
    ##     updateTradeValue(data = .,
    ##                      unitValue = export_unit_value,
    ##                      value = export_value,
    ##                      valueFlag = gsub(valuePrefix, flagPrefix, export_value),
    ##                      quantity = export_quantity,
    ##                      calculatedFlag = "")
    
    ## ## Save the data back to the database
    ## balancedData %>%
    ##     saveValidatedTradeData(data = .)
    
}


## ## This is just a test
## test = GetData(swsContext.datasets[[1]])
## SaveData(domain = "trade", dataset = "ct_raw_tf", data = test)
