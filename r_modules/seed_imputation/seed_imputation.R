## Steps:
##
## (1) First get the area harvested/sown data using the R API (GetData)
##
## (2) Then get the seed rate data after loading them in to the data
##     base. (GetTableData).
##
## NOTE (Michael): The codes need to be converted to CPC from FCL.
##
##
## (3) Multiply the seed rate with the area harvested/sown in the
##     following year to get the seed used in the current year.
##
## (4) Save the data back.

library(data.table)
library(faosws)
library(faoswsFlag)
library(faoswsUtil)
library(magrittr)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
areaSownElementCode = "5212"
areaHarvestedElementCode = "5312"
seedElementCode = "5525"
valuePrefix = "Value_measuredElement_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"


## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "ad7f16e3-d447-48ec-9d62-089f63bbc137"
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
}

## Function for obtaining the area harvested/sown data
getAreaData = function(dataContext, areaSownElementCode, areaHarvestedElementCode,
    seedElementCode){
    
    ## Setups
    requiredElements = c(areaSownElementCode, areaHarvestedElementCode,
        seedElementCode)


    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )

    ## Check the code for area
    slot(slot(dataContext, "dimensions")$measuredElement, "keys") =
        requiredElements
    
    ## Query the data
    query = GetData(
        key = dataContext,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )

    ## Find and fill in missing elements
    missingElements =
        requiredElements[which(!paste0(valuePrefix, requiredElements) %in%
                               colnames(query))]
    lapply(missingElements,
           FUN = function(x){
               query[, `:=`(paste0(x, c(valuePrefix, flagObsPrefix,
                                        flagMethodPrefix)),
                            list(as.numeric(NA), as.character(NA),
                                 as.character(NA)))]
           }
           )
    
    ## Make zero (M) as NA (M)
    lapply(requiredElements,
           FUN = function(x){
               remove0M(data = query,
                        value = paste0(valuePrefix, x),
                        flag = paste0(flagObsPrefix, x))
           })
    query[, timePointYears := as.numeric(timePointYears)]
    setkeyv(query, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    ## list(query = query,
    ##      prefixTuples = prefixTuples)
    query
}

## Get the area harvested/sown data
## areaData = getAreaData(swsContext.datasets[[1]])

## Function to compute the area sown for calculation of seed, this
## function. 
imputeAreaSown = function(data, valueAreaSown = "Value_measuredElement_5212",
    valueAreaHarvested = "Value_measuredElement_5312",
    flagObsAreaSown = "flagObservationStatus_measuredElement_5212",
    flagObsAreaHarvested = "flagObservationStatus_measuredElement_5312",
    imputedFlag = "i"){
    if(all(is.na(data[[valueAreaSown]]))){
        data[[valueAreaSown]] = data[[valueAreaHarvested]]
        data[[flagObsAreaSown]] = data[[flagObsAreaHarvested]]
    } else {
        ratio = mean(data[[valueAreaSown]]/data[[valueAreaHarvested]],
            na.rm = TRUE)
        replaceIndex =
            is.na(data[[valueAreaSown]]) &
            !is.na(data[[valueAreaHarvested]])
        data[[valueAreaSown]][replaceIndex] =
            data[[valueAreaHarvested]] * ratio
        data[[flagObsAreaSown]][replaceIndex] =
            data[[flagObsAreaHarvested]]
    }
    data
}


## Function to load the country specific rate data
getCountrySpecificSeedRate = function(){
    countrySpecificRate.dt =
        GetTableData(schemaName = "ess", tableName = "specific_seed_rate")

    ## NOTE (Michael): We assume all other data collection method are official
    countrySpecificRate.dt[flag != "E", flag := ""]

    setnames(countrySpecificRate.dt,
             old = c("area", "item", "value", "flag"),
             new = c("geographicAreaM49", "measuredItemCPC", "Value_seedRate",
                 "flagObservationStatus_seedRate"))
    countrySpecificRate.dt[, measuredItemCPC := as.character(measuredItemCPC)]
    countrySpecificRate.dt[, geographicAreaM49 := as.character(geographicAreaM49)]
    setkeyv(countrySpecificRate.dt, c("geographicAreaM49", "measuredItemCPC"))
    countrySpecificRate.dt
}

fillCountrySpecificSeedRate = function(data,
    countrySpecificData = getCountrySpecificSeedRate()){
    ## Fill in the country Specific rates          
    data[countrySpecificData,
         `:=`(c("Value_seedRate", "flagObservationStatus_seedRate"),
              list(i.Value_seedRate, i.flagObservationStatus_seedRate)),
         allow.cartesian = TRUE]

}

getCountryGeneralSeedRate = function(){
    seedGeneral.dt =
        GetTableData(schemaName = "ess", tableName = "default_seed_rate")
    ## NOTE (Michael): We again assume the rates are official here
    seedGeneral.dt[, flagObservationStatus_seedRate := ""]
    setnames(seedGeneral.dt,
             old = c("item", "value"),
             new = c("measuredItemCPC", "Value_seedRate"))
    setkeyv(seedGeneral.dt, "measuredItemCPC")
    seedGeneral.dt
}


fillGeneralSeedRate = function(data,
    generalSeedData = getCountryGeneralSeedRate()){
    ## fill in the general rates
    okey = key(data)
    setkeyv(data, key(generalSeedData))
    data[is.na(Value_seedRate), ][generalSeedData,
                                  `:=`(c("Value_seedRate"),
                                       list(i.Value_seedRate)),
                                  allow.cartesian = TRUE]
    setkeyv(data, okey)
}


## Function to impute the seed.
imputeSeed = function(areaSown, seedRate, missingFlag = "M", imputedFlag = "i"){
    seed = c(c(areaSown, NA) * c(NA, seedRate)/1000)[-1]
    seedFlag = rep(missingFlag, length(areaSown))
    seedFlag[!is.na(seedFlag)] = imputedFlag
    list(seed, seedFlag)
}

imputeSeed = function(data,
    seedValue = "Value_measuredElement_5525",
    seedMethodFlag = "flagMethod_measuredElement_5525",
    seedObsFlag = "flagObservationStatus_measuredElement_5525",
    areaSownValue = "Value_measuredElement_5212",
    areaSownObsFlag = "flagObservationStatus_measuredElement_5212",
    seedRateValue = "Value_seedRate",
    seedRateFlag = "flagObservationStatus_seedRate",
    missingFlag = "M", imputedFlag = "i",
    byKey = key(data)[1:2]){

    each = function(seedValue = seedValue, seedMethodFlag = seedMethodFlag,
        seedObsFlag = seedObsFlag, areaSownValue = areaSownValue,
        areaSownObsFlag, seedRateValue = seedRateValue,
        seedRateFlag = seedRateFlag, imputedFlag = imputedFlag){
        newSeedValue = c(c(areaSownValue, NA) * c(NA, seedRateValue)/1000)[-1]
        replaceIndex = is.na(seedValue) & !is.na(newSeedValue)
        seedValue[replaceIndex] = newSeedValue
        seedMethodFlag[replaceIndex] = imputedFlag
        seedObsFlag[replaceIndex] =
            aggregateObservationFlag(areaSownObsFlag, seedRateFlag)
        list(seedValue, seedMethodFlag, seedObsFlag)        
    }
    data[, `:=`(c(seedValue, seedMethodFlag, seedObsFlag),
                each(seedValue = get(seedValue),
                     seedMethodFlag = get(seedMethodFlag),
                     seedObsFlag = get(seedObsFlag),
                     areaSownValue = get(areaSownValue),
                     areaSownObsFlag = get(areaSownObsFlag),
                     seedRateValue = get(seedRateValue),
                     seedRateFlag = get(seedRateFlag),
                     imputedFlag = imputedFlag)), by = byKey]
    data[, `:=`(c(seedRateValue, seedRateFlag), NULL)]
}
                    
    
SaveSeedData = function(data){
    ## Save the data back
    SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
             dataset = slot(swsContext.datasets[[1]], "dataset"),
             data = data,
             normalized = FALSE)
}


## Run the whole seed module
getAreaData(swsContext.datasets[[1]],
            areaSownElementCode = areaSownElementCode,
            areaHarvestedElementCode = areaHarvestedElementCode,
            seedElementCode = seedElementCode) %>%
                imputeAreaSown %>%
                    fillCountrySpecificSeedRate %>%
                        fillGeneralSeedRate %>%
                            imputeSeed %>%
                                SaveSeedData












## NOTE (Michael): Need to check whether all the seed rate data has
##                 been included.
##
## NOTE (Michael): Need to check with Adam where the data came from?
##
## NOTE (Michael): Need to convert the seed.rate in the AllSeed
##                 data.frame to numeric.
##
## NOTE (Michael): It looks like the seed data is the country and year
##                 specific rates, while the AllSeed is the default
##                 rates for each commodity.
##
## NOTE (Michael): The multiplication should be done to the next year,
##                 not the current year.
##
## NOTE (Michael): What are the nutrition value for?
##
## NOTE (Michael): All the codes are still in FCL
##
## NOTE (Michael): Remove the hard coded china, and also 351 is an aggregate.
##
## NOTE (Michael): Write the seed module at the item level.

