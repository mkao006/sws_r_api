## load the library
library(faosws)
library(data.table)
library(faoswsUtil)
library(faoswsFlag)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")
noDataMessage = paste("Algorithm ran successfully, but no data was available",
                      "for the selected dimensions from the Eurostat tables.")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Get SWS Parameters
    GetTestEnvironment(
        ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## token = "a2dd0e14-1cdc-4486-bc4b-1f65d9ecad01"
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "02ebe8ec-6509-477a-97b2-bf32f7a9afe2"
    )
    files = dir("~/Documents/Github/sws_r_api/r_modules/eurostat_harvester/R",
                full.names = TRUE)
    sapply(files, source)
    
} else {
    cat("Working on SWS...\n")
}

# dir = "~/GitHub/sws_r_api/r_modules/eurostat_harvester/"
animalMap = GetTableData(schemaName = "ess",
                         tableName = "eurostat_cpc_animals")
## Filter out any duplicate maps, as they shouldn't be here and will cause
## problems with removeNon1to1.
animalMap = unique(animalMap)
animalMap = removeNon1to1(data = animalMap, key1Colname = "animals",
                          key2Colname = "cpc")
cropMap = GetTableData(schemaName = "ess",
                       tableName = "eurostat_cpc_crop")
cropMap = unique(cropMap)
cropMap = remove1toMany(data = cropMap, key1Colname = "crop",
                        key2Colname = "cpc")
meatMap = GetTableData(schemaName = "ess",
                       tableName = "eurostat_cpc_meat")
meatMap = unique(meatMap)
meatMap = removeNon1to1(data = meatMap, key1Colname = "meat",
                        key2Colname = "cpc")

## Convert Commodity Codes
commodityCPC = data.table(
    cpc = as.character(swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys)
    )
animalItems = merge(commodityCPC, animalMap, by = "cpc")
animalItems = animalItems[, c("cpc", "animals"), with = FALSE]
setnames(animalItems, c("measuredItemCPC", "EurostatCode"))
cropItems = merge(commodityCPC, cropMap, by = "cpc")
cropItems = cropItems[, c("cpc", "crop"), with = FALSE]
setnames(cropItems, c("measuredItemCPC", "EurostatCode"))
meatItems = merge(commodityCPC, meatMap, by = "cpc")
meatItems = meatItems[, c("cpc", "meat"), with = FALSE]
setnames(meatItems, c("measuredItemCPC", "EurostatCode"))
if(max(nrow(animalItems), nrow(meatItems), nrow(cropItems)) == 0)
    stop(noDataMessage, " (No rows match with eurostat items)")

## Convert Country Codes
countryMap = GetTableData(schemaName = "ess", tableName = "eurostat_m49")
m49Areas = swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
eurostatAreas = countryMap[m49 %in% m49Areas, eurostat]
# eurostatAreas = eurostatAreas[!eurostatAreas %in%
#                                   c("GB", "RE", "MQ", "GR", "GP", "GF")]
if(length(eurostatAreas) == 0)
    stop(noDataMessage, " (No rows match with eurostat areas)")

## Convert Element Codes
elementMap = GetTableData(schemaName = "ess", tableName = "eurostat_element")
if(any(elementMap$factor != 1000))
    stop("Current code assumes all factors are 1000, but this seems to not ", 
         "be the case.  The code needs to be updated to handle other factors.")
fbsElements = swsContext.datasets[[1]]@dimensions$measuredElement@keys
eurostatElements = elementMap[element %in% fbsElements, ]
if(nrow(eurostatElements) == 0)
    stop(noDataMessage, " (No rows match with eurostat elements)")

## Convert Time Codes
eurostatMonths = "M12"
eurostatYears = swsContext.datasets[[1]]@dimensions$timePointYears@keys

## Unit Codes (for animals only)
eurostatUnits = c("THS_HD", "THS_T")

## Pull relevant Eurostat data
newFAOData = NULL
if(nrow(animalItems) > 0){
    keyPig = DatasetKey(domain = "eurostat", dataset = "raw_apro_mt_lspig",
                         dimensions = list(
                             Dimension(name = "eurostatRawAgriprod",
                                       keys = animalItems$EurostatCode),
                             Dimension(name = "eurostatRawMonth",
                                       keys = eurostatMonths),
                             Dimension(name = "eurostatRawUnit",
                                       keys = eurostatUnits),
                             Dimension(name = "eurostatRawGeo",
                                       keys = eurostatAreas),
                             ## HACK!  Eurostat time codes are one year off
                             ## of the SWS years, but only for animals.  So,
                             ## we have to adjust here and in the extracted
                             ## data.
                             Dimension(name = "timePointYears",
                                       keys = as.character(as.numeric(eurostatYears) - 1))
                     ))
    ## If any keys have a length of 0, then we'll get an error with
    ## GetDataFlexible. To avoid that, check here first and only pull the data
    ## if all keys exist.
    keyLength = lapply(keyPig@dimensions, function(x) length(x@keys))
    if(all(keyLength > 0)){
        keyGoat = keyPig
        keyGoat@dataset = "raw_apro_mt_lsgoat"
        keySheep = keyPig
        keySheep@dataset = "raw_apro_mt_lssheep"
        keyCattle = keyPig
        keyCattle@dataset = "raw_apro_mt_lscatl"
        pigData = GetDataFlexible(keyPig)
        goatData = GetDataFlexible(keyGoat)
        sheepData = GetDataFlexible(keySheep)
        cattleData = GetDataFlexible(keyCattle)
        #stop("Is this still true?  Something's wrong if it is...")
        #identical(sheepData, cattleData)
        newTempData = rbind(newFAOData, pigData, goatData,
                            sheepData, cattleData)
        if(nrow(newTempData) > 0){ # Only continue if data is available
            newTempData[, eurostatRawMonth := NULL]
            ## HACK!  Convert year values back to original to match SWS
            convertCode(data = newTempData, mappingTable = animalMap,
                        keyData = "eurostatRawAgriprod",
                        newKeyName = "measuredItemCPC",
                        newKeyMap = "cpc", oldKeyMap = "animals")
            convertCode(data = newTempData, mappingTable = countryMap,
                        keyData = "eurostatRawGeo",
                        newKeyName = "geographicAreaM49",
                        newKeyMap = "m49", oldKeyMap = "eurostat")
            convertCode(data = newTempData,
                        mappingTable = elementMap[dataset == "apro_mt_lscatl",],
                        keyData = "eurostatRawUnit",
                        newKeyName = "measuredElement",
                        newKeyMap = "element", oldKeyMap = "unit")
            ## HACK!  Eurostat reports at end of year, so adjust year up one
            newTempData[, timePointYears :=
                            as.character(as.numeric(timePointYears) + 1)]
            newFAOData = rbindlist(list(newFAOData, newTempData))
        }
    }
}
if(nrow(cropItems) > 0){
    cropElements = eurostatElements[!is.na(strcupro), strcupro]
    keyCrop = DatasetKey(domain = "eurostat", dataset = "raw_apro_cpp_crop",
                         dimensions = list(
                             Dimension(name = "eurostatRawCroppro",
                                       keys = cropItems$EurostatCode),
                             Dimension(name = "eurostatRawStrucpro",
                                       keys = cropElements),
                             Dimension(name = "eurostatRawGeo",
                                       keys = eurostatAreas),
                             Dimension(name = "timePointYears",
                                       keys = eurostatYears)
                     ))
    ## If any keys have a length of 0, then we'll get an error with
    ## GetDataFlexible. To avoid that, check here first and only pull the data
    ## if all keys exist.
    keyLength = lapply(keyCrop@dimensions, function(x) length(x@keys))
    if(all(keyLength > 0)){
        cropData = GetDataFlexible(keyCrop)
        if(nrow(cropData) > 0){ # Only continue if data is available
            convertCode(data = cropData, mappingTable = cropMap,
                        keyData = "eurostatRawCroppro",
                        newKeyName = "measuredItemCPC",
                        newKeyMap = "cpc", oldKeyMap = "crop")
            convertCode(data = cropData, mappingTable = countryMap,
                        keyData = "eurostatRawGeo",
                        newKeyName = "geographicAreaM49",
                        newKeyMap = "m49", oldKeyMap = "eurostat")
            convertCode(data = cropData,
                        mappingTable = elementMap[dataset == "raw_apro_cpp_crop",],
                        keyData = "eurostatRawStrucpro",
                        newKeyName = "measuredElement",
                        newKeyMap = "element", oldKeyMap = "strcupro")
            newFAOData = rbind(newFAOData, cropData)
        }
    }
}
if(nrow(meatItems) > 0){
    meatElements = eurostatElements[!is.na(meatitem), meatitem]
    keyMeat = DatasetKey(domain = "eurostat", dataset = "raw_apro_mt_pann",
                         dimensions = list(
                             Dimension(name = "eurostatRawAgriprod",
                                       keys = meatItems$EurostatCode),
                             Dimension(name = "eurostatRawMeatItem",
                                       keys = meatElements),
                             Dimension(name = "eurostatRawUnit",
                                       keys = eurostatUnits),
                             Dimension(name = "eurostatRawGeo",
                                       keys = eurostatAreas),
                             Dimension(name = "timePointYears",
                                       keys = eurostatYears)
                     ))
    ## If any keys have a length of 0, then we'll get an error with GetDataFlexible.
    ## To avoid that, check here first and only pull the data if all keys
    ## exist.
    keyLength = lapply(keyMeat@dimensions, function(x) length(x@keys))
    if(all(keyLength > 0)){
        meatData = GetDataFlexible(keyMeat)
        if(nrow(meatData) > 0){ # Only continue if data is available
            meatData[, eurostatRawMeatItem := NULL]
            convertCode(data = meatData, mappingTable = meatMap,
                        keyData = "eurostatRawAgriprod",
                        newKeyName = "measuredItemCPC",
                        newKeyMap = "cpc", oldKeyMap = "meat")
            convertCode(data = meatData, mappingTable = countryMap,
                        keyData = "eurostatRawGeo",
                        newKeyName = "geographicAreaM49",
                        newKeyMap = "m49", oldKeyMap = "eurostat")
            convertCode(data = meatData,
                        mappingTable = elementMap[dataset == "raw_apro_mt_pann", ],
                        keyData = "eurostatRawUnit",
                        newKeyName = "measuredElement",
                        newKeyMap = "element", oldKeyMap = "unit")
            newFAOData = rbind(newFAOData, meatData)
        }
    }
}

if(is.null(newFAOData)){
    outMessage = "No Eurostat data available for these keys!"
} else {
    ## Convert Flags
    flagMappingTable = GetTableData(schemaName = "ess",
                                    tableName = "eurostat_flag_sws")
    setnames(flagMappingTable, c("eurostat_flag", "status",
                                 "method", "description"),
             c("flagRawEurostat", "flagObservationStatus",
               "flagMethod", "Metadata"))
    flagMappingTable[is.na(flagObservationStatus), flagObservationStatus := ""]
    newFAOData = merge(newFAOData, flagMappingTable, by = "flagRawEurostat",
                       all.x = TRUE)
    if(any(is.na(newFAOData$flagObservationStatus)))
        stop("Invalid Eurostat flag!  Please update the flag conversion table.")
    ## Some Eurostat to CPC maps are Many to 1.  So, we need to
    ## aggregate those cases into unique elements:
    newFAOData = newFAOData[, combineRows(Value, flagObservationStatus,
                                          flagMethod, Metadata, flagRawEurostat),
                            by = c("timePointYears", "measuredItemCPC",
                                   "geographicAreaM49", "measuredElement")]
    newFAOData[, flagRawEurostat := NULL]
    
    ## Convert Values (Conversion Factors)
    newFAOData[, Value := Value * 1000]
    
    ## Merge with original data to determine which values to overwrite
    keys = names(swsContext.datasets[[1]]@dimensions)
    oldFAOData = GetDataFlexible(swsContext.datasets[[1]])
    if(nrow(oldFAOData) > 0){
        ## Convert keys to character for the join
        for(key in keys){
            oldFAOData[, c(key) := as.character(get(key))]
            newFAOData[, c(key) := as.character(get(key))]
        }
        newFAOData = merge(newFAOData, oldFAOData, by = keys, all.x = TRUE)
        ## Don't overwrite non-missing (old) values with missing (new) values
        newFAOData = newFAOData[flagObservationStatus.x != "M" |
                                flagObservationStatus.y == "M", ]
        newFAOData[, c("Value.y", "flagObservationStatus.y", "flagMethod.y") :=
                        list(NULL)]
        setnames(newFAOData, c("Value.x", "flagObservationStatus.x", "flagMethod.x"),
                 c("Value", "flagObservationStatus", "flagMethod"))
    }
    metadata = newFAOData[, c(keys), with = FALSE]
    metadata[, Metadata := "GENERAL"]
    metadata[, Metadata_Language := "en"]
    metadata[, Metadata_Group := 1]
    metadata[, Metadata_Element := "COMMENT"]
    metadata[, Metadata_Value := newFAOData[, Metadata]]
    ## Also write to metadata to indicate it was pulled from Eurostat
    metadataCopy = copy(metadata)
    metadataCopy[, Metadata := "SOURCE"]
    metadataCopy[, Metadata_Language := "en"]
    metadataCopy[, Metadata_Group := 1]
    metadataCopy[, Metadata_Element := "ORGANIZATION"]
    metadataCopy[, Metadata_Value := "Eurostat"]
    metadata = metadata[Metadata_Value != "/", ]
    
    SaveData(domain = "agriculture", dataset = "agriculture",
             data = newFAOData[, c(keys, "Value", "flagObservationStatus",
                                   "flagMethod"), with = FALSE],
             metadata = rbind(metadata, metadataCopy))
    
    outMessage = "Module completed!"
}

outMessage