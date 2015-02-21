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

    newKey = DatasetKey(domain = "trade", dataset = "completed_tf",
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



selectedItems = swsContext.datasets[[1]]@dimensions$measuredItemHS@keys
for(i in selectedItems){

    ## Validated the data
    ## TODO (Michael): Need to change the validated flag back to "v" after
    ##                 adding it as a valid flag.
    validatedData =
        getComtradeMirroredData(swsContext.datasets[[1]]) %>%
        ## NOTE (Michael): Actually need to create the same mirror here.
        validation(data = .,
                   importUnitValue = import_unit_value,
                   exportUnitValue = export_unit_value,
                   reverseImportUnitValue = reverse_import_unit_value,
                   reverseExportUnitValue = reverse_export_unit_value,
                   importUnitValueFlag =
                       gsub(valuePrefix, flagPrefix, import_unit_value),
                   exportUnitValueFlag =
                       gsub(valuePrefix, flagPrefix, export_unit_value),
                   ratioBoundary = 1.1, log = TRUE, plot = FALSE,
                   validatedFlag = "") %>%
        imputeUnitValue(data = .,
                        unitValue = import_unit_value,
                        unitValueFlag = gsub(valuePrefix, flagPrefix, import_unit_value),
                        mirrorUnitValue = reverse_export_unit_value,
                        imputationFlag = "") %>%
        imputeUnitValue(data = .,
                        unitValue = export_unit_value,
                        unitValueFlag = gsub(valuePrefix, flagPrefix, export_unit_value),
                        mirrorUnitValue = reverse_import_unit_value,
                        imputationFlag = "")

    
    ## Balance the data
    balancedData =
        validatedData %>%
        updateTradeQuantity(data = .,
                            unitValue = import_unit_value,
                            unitValueFlag =
                                gsub(valuePrefix, flagPrefix, import_unit_value),
                            value = import_value,
                            quantity = import_quantity,
                            quantityFlag =
                                gsub(valuePrefix, flagPrefix, import_quantity),
                            calculatedFlag = "") %>%
        updateTradeQuantity(data = .,
                            unitValue = export_unit_value,
                            unitValueFlag =
                                gsub(valuePrefix, flagPrefix, export_unit_value),
                            value = export_value,
                            quantity = export_quantity,
                            quantityFlag =
                                gsub(valuePrefix, flagPrefix, export_quantity),
                            calculatedFlag = "") %>%
        updateTradeQuantity(data = .,
                            unitValue = reverse_import_unit_value,
                            unitValueFlag =
                                gsub(valuePrefix, flagPrefix, reverse_import_unit_value),
                            value = reverse_import_value,
                            quantity = reverse_import_quantity,
                            quantityFlag =
                                gsub(valuePrefix, flagPrefix, reverse_import_quantity),
                            calculatedFlag = "") %>%
        updateTradeQuantity(data = .,
                            unitValue = reverse_export_unit_value,
                            unitValueFlag =
                                gsub(valuePrefix, flagPrefix, reverse_export_unit_value),
                            value = reverse_export_value,
                            quantity = reverse_export_quantity,
                            quantityFlag =
                                gsub(valuePrefix, flagPrefix, reverse_export_quantity),
                            calculatedFlag = "") %>%
        calculateReliability(data = .,
                             importQuantity = import_quantity,
                             exportQuantity = export_quantity,
                             reverseImportQuantity = reverse_import_quantity,
                             reverseExportQuantity = reverse_export_quantity,
                             reportingCountry = reportingCountryVar,
                             partnerCountry = partnerCountryVar,
                             pctTolerance = 0.1) %>% 
        balanceTradeQuantity(data = .,
                             importQuantity = import_quantity,
                             exportQuantity = export_quantity,
                             reverseImportQuantity = reverse_import_quantity,
                             reverseExportQuantity = reverse_export_quantity,
                             reportingReliability = reportingCountryVar,
                             partnerReliability = partnerCountryVar,
                             pctTolerance = 0.1) %>%
        updateTradeValue(data = .,
                         unitValue = import_unit_value,
                         value = import_value,
                         valueFlag = gsub(valuePrefix, flagPrefix, import_value),
                         quantity = import_quantity,
                         calculatedFlag = "") %>%
        updateTradeValue(data = .,
                         unitValue = export_unit_value,
                         value = export_value,
                         valueFlag = gsub(valuePrefix, flagPrefix, export_value),
                         quantity = export_quantity,
                         calculatedFlag = "")
    
    ## Save the data back to the database
    balancedData %>%
        saveValidatedTradeData(data = .)
    
}

