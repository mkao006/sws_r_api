## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Get SWS Parameters
    GetTestEnvironment(
        ## baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## token = "a2dd0e14-1cdc-4486-bc4b-1f65d9ecad01"
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "4e57a0e6-39ae-447e-a7f7-f4cf9d5524d0"
    )
    
} else {
    cat("Working on SWS...\n")
}

warning("UPDATE THIS WITH DATABASE TABLES!!!")
dir = "~/Documents/Github/sws_r_api/r_modules/eurostat_harvester/"
animalMap = fread(input = paste0(dir, "Mapping_ESTAT-CPC_animals.csv"))
animalMap[, Item := as.character(Item)]
cropMap = fread(input = paste0(dir, "Mapping_ESTAT-FCL_crops.csv"))
cropMap[, Item := as.character(Item)] # Are these necessary?
meatMap = fread(input = paste0(dir, "Mapping_ESTAT-CPC_meat.csv"))
meatMap[, Item := as.character(Item)]

## Convert Commodity Codes (crops aren't CPC in SWS, but animals and meat are)
commodityCPC = data.table(
    Item = as.character(swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys)
    )
fcl2cpc = GetTableData(schemaName = "ess", tableName = "fcl_2_cpc")
cropMap[, Item := formatC(as.numeric(Item), width = 4, flag = "0",
                          format = "d")]
cropMap = data.table(merge.data.frame(cropMap, fcl2cpc,
                                      by.x = "Item", by.y = "fcl"))
cropMap[, Item := cpc]
cropMap[, cpc := NULL]

animalItems = merge(commodityCPC, animalMap, by = "Item")
animalItems = animalItems[, c("Item", "animals"), with = FALSE]
setnames(animalItems, c("measuredItemCPC", "EurostatCode"))
cropItems = merge(commodityCPC, cropMap, by = "Item")
cropItems = cropItems[, c("Item", "crop_pro"), with = FALSE]
setnames(cropItems, c("measuredItemCPC", "EurostatCode"))
meatItems = merge(commodityCPC, meatMap, by = "Item")
meatItems = meatItems[, c("Item", "meat"), with = FALSE]
setnames(meatItems, c("measuredItemCPC", "EurostatCode"))
if(max(nrow(eurostatItems), nrow(meatItems), nrow(cropItems)) == 0)
    stop("No rows match with eurostat items")

## Convert Country Codes
countryMap = GetTableData(schemaName = "ess", tableName = "eurostat_m49")
m49Areas = swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys
eurostatAreas = countryMap[m49 %in% m49Areas, eurostat]
eurostatAreas = eurostatAreas[!eurostatAreas %in%
                                  c("GB", "RE", "MQ", "GR", "GP", "GF")]
if(length(eurostatAreas) == 0)
    stop("No rows match with eurostat areas")

## Convert Element Codes
## Waiting on table to be loaded:
## elementMap = GetTableData(schemaName = "ess", tableName = "eurostat_element")
elementMap = data.table(strucpro = c(NA, NA, "AR", "PR", NA, NA, NA, NA),
                        meatitem = c(NA, NA, "SL", "SL", NA, NA, NA, NA),
                        unit = c("THS_HD", "THS_T", NA, NA, rep("THS_HD", 4)),
                        element = c(5330, 5315, 5312, 5510, rep(5511, 4)),
                        factor = 1000)
fbsElements = swsContext.datasets[[1]]@dimensions$measuredElement@keys
eurostatElements = elementMap[element %in% fbsElements, ]
if(nrow(eurostatElements) == 0)
    stop("No rows match with eurostat elements")

## Convert Time Codes
eurostatMonths = "M12"
eurostatYears = swsContext.datasets[[1]]@dimensions$timePointYears@keys

## Unit Codes (for animals only)
eurostatUnits = c("THS_HD", "THS_T")

## Pull relevant Eurostat data
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
                             Dimension(name = "timePointYears",
                                       keys = eurostatYears)
                     ))
    keyGoat = keyPig
    keyGoat@dataset = "raw_apro_mt_lsgoat"
    keySheep = keyPig
    keySheep@dataset = "raw_apro_mt_lssheep"
    keyCattle = keyPig
    keyCattle@dataset = "raw_apro_mt_lscatl"
    pigData = GetData(keyPig)
    goatData = GetData(keyGoat)
    sheepData = GetData(keySheep)
    cattleData = GetData(keyCattle)
    stop("Is this still true?  Something's wrong if it is...")
    identical(sheepData, cattleData)
}
if(nrow(cropItems) > 0){
    cropElements = eurostatElements[!is.na(strucpro), strucpro]
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
    cropData = GetData(keyCrop)
}
if(nrow(meatItems) > 0){
}

## Convert Flags

## Convert Values (Conversion Factors)

"Module completed!"