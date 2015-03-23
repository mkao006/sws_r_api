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
library(igraph)
library(lme4)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"
areaSownElementCode = "5025"
areaHarvestedElementCode = "5312"
seedElementCode = "5525"
valuePrefix = "Value_measuredElement_"
flagObsPrefix = "flagObservationStatus_measuredElement_"
flagMethodPrefix = "flagMethod_measuredElement_"


## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "9a7edc96-ff8e-4428-a1ca-60f383096be6"
        )
}

## Function to obtain all CPC item 
getAllItemCPC = function(){
    itemEdgeList =
        adjacent2edge(
            GetCodeTree(domain = "agriculture",
                        dataset = "agriculture",
                        dimension = itemVar)
        )
    itemEdgeGraph = graph.data.frame(itemEdgeList)
    itemDist = shortest.paths(itemEdgeGraph, v = "0", mode = "out")
    fbsItemCodes = colnames(itemDist)[is.finite(itemDist)]
    fbsItemCodes
}

getAllCountries = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "geographicAreaM49")[type == "country", code]
}

getAllYears = function(){
    GetCodeList(domain = "agriculture",
                dataset = "agriculture",
                dimension = "timePointYears")[description != "wildcard", code]
}
    

getOfficialSeedData = function(){
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = getAllCountries()),
            Dimension(name = elementVar,
                      keys = seedElementCode),
            Dimension(name = itemVar,
                      keys = getAllItemCPC()),
            Dimension(name = yearVar,
                      keys = getAllYears())
        )
    )

    ## Pivot to vectorize yield computation
    seedPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    seedQuery = GetData(
        key = seedKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = seedPivot
    )

    ## Convert time to numeric
    seedQuery[, timePointYears := as.numeric(timePointYears)]
    seedQuery[seedQuery[[paste0(flagObsPrefix, seedElementCode)]] == "", ]
}

getAllAreaData = function(){
    ## Setups    
    areaKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = getAllCountries()),
            Dimension(name = elementVar,
                      keys = c(areaHarvestedElementCode, areaSownElementCode)),
            Dimension(name = itemVar,
                      keys = getAllItemCPC()),
            Dimension(name = yearVar,
                      keys = getAllYears())
        )
    )

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )
    
    ## Query the data
    query = GetData(
        key = areaKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )
    
    query[, timePointYears := as.numeric(timePointYears)]
    setkeyv(query, c("geographicAreaM49", "measuredItemCPC", "timePointYears"))
    query
}

getWorldBankClimateData = function(){
    ## allCountries =
    ##     GetCodeList(domain = "WorldBank",
    ##                 dataset = "wb_ecogrw",
    ##                 dimension = "geographicAreaM49")[type == "country", code]
    
    newKey =
        DatasetKey(domain = "WorldBank",
                   dataset = "wb_climate",
                   dimensions =
                       list(
                           Dimension(name = "geographicAreaM49",
                                     keys = getAllCountries()),
                           Dimension(name = "wbIndicator",
                                     keys = c("SWS.FAO.PREC", "SWS.FAO.TEMP")),
                           Dimension(name = "timePointYears",
                                     keys = getAllYears())
                       )
                   )

    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "wbIndicator", ascending = TRUE)        
    )

    climateData = GetData(key = newKey, pivoting = newPivot, normalized = FALSE)
    climateData[, timePointYears := as.numeric(timePointYears)]
    climateData
}

## Function for obtaining the area harvested/sown data
getAreaData = function(dataContext, areaSownElementCode, areaHarvestedElementCode,
    seedElementCode){
    
    ## Setups
    requiredElements = c(areaSownElementCode, areaHarvestedElementCode)


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
    query
}

## Get the area harvested/sown data

## Function to compute the area sown for calculation of seed, this
## function. 
imputeAreaSown = function(data, valueAreaSown = "Value_measuredElement_5025",
    valueAreaHarvested = "Value_measuredElement_5312",
    flagObsAreaSown = "flagObservationStatus_measuredElement_5025",
    flagObsAreaHarvested = "flagObservationStatus_measuredElement_5312",
    flagMethodAreaSown = "flagMethod_measuredElement_5025",
    imputedObsFlag = "I", imputedMethodFlag = "e"){
    if(all(is.na(data[[valueAreaSown]]))){
        data[[valueAreaSown]] = data[[valueAreaHarvested]]
        data[[flagObsAreaSown]] = data[[flagObsAreaHarvested]]
    } else {
        ratio =
            mean(computeRatio(data[[valueAreaSown]],
                              data[[valueAreaHarvested]]),
                 na.rm = TRUE)
        replaceIndex =
            is.na(data[[valueAreaSown]]) &
            !is.na(data[[valueAreaHarvested]])
        data[[valueAreaSown]][replaceIndex] =
            data[[valueAreaHarvested]][replaceIndex] * ratio
        ## NOTE (Michael): This is actually wrong, the flag should not
        ##                 be transferred.
        ##
        ## data[[flagObsAreaSown]][replaceIndex] =
        ##     data[[flagObsAreaHarvested]]
        data[[flagObsAreaSown]][replaceIndex] = imputedObsFlag
        data[[flagMethodAreaSown]][replaceIndex] = imputedMethodFlag
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
    merge(data, countrySpecificData, allow.cartesian = TRUE)
    ## Fill in the country Specific rates
    ## data[countrySpecificData,
    ##      `:=`(c("Value_seedRate", "flagObservationStatus_seedRate"),
    ##           list(i.Value_seedRate, i.flagObservationStatus_seedRate)),
    ##      allow.cartesian = TRUE]

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

mergeAllSeedData = function(seedData, ...){
    explanatoryData = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all.x = TRUE)
    },
           x = explanatoryData, init = seedData
           )
}

removeCarryForward = function(data, variable){
    data[, variance := var(.SD[[variable]], na.rm = TRUE),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data[, duplicateValue := duplicated(.SD[[variable]]),
         by = c("geographicAreaM49", "measuredItemCPC")]
    data = data[!(variance == 0 & duplicateValue), ]
    data[, `:=`(c("variance", "duplicateValue"), NULL)]
    data         
}


buildCPCHierarchy = function(data, cpcItemVar, levels = 3){
    data[, `:=`(c(paste0("cpcLvl", 1:levels)),
                lapply(1:levels, FUN = function(x)
                    factor(substr(data[[cpcItemVar]], 1, x))))]
}

imputeSeed = function(data,
    seedValue = "Value_measuredElement_5525",
    seedMethodFlag = "flagMethod_measuredElement_5525",
    seedObsFlag = "flagObservationStatus_measuredElement_5525",
    areaSownValue = "Value_measuredElement_5025",
    areaSownObsFlag = "flagObservationStatus_measuredElement_5025",
    seedRateValue = "Value_seedRate",
    seedRateFlag = "flagObservationStatus_seedRate",
    imputedFlag = "i",
    byKey = key(data)[1:2]){

    each = function(seedValue = seedValue, seedMethodFlag = seedMethodFlag,
        seedObsFlag = seedObsFlag, areaSownValue = areaSownValue,
        areaSownObsFlag, seedRateValue = seedRateValue,
        seedRateFlag = seedRateFlag, imputedFlag = imputedFlag){
        
        newSeedValue = c(c(areaSownValue, NA) * c(NA, seedRateValue)/1000)[-1]
        replaceIndex = is.na(seedValue) & !is.na(newSeedValue)
        seedValue[replaceIndex] = newSeedValue[replaceIndex]
        seedMethodFlag[replaceIndex] = imputedFlag
        seedObsFlag[replaceIndex] =
            aggregateObservationFlag(areaSownObsFlag[replaceIndex],
                                     seedRateFlag[replaceIndex])
        list(seedValue, seedMethodFlag, seedObsFlag)
        
    }
    data[, `:=`(c(seedValue, seedMethodFlag, seedObsFlag),
                each(seedValue = .SD[[seedValue]],
                     seedMethodFlag = .SD[[seedMethodFlag]],
                     seedObsFlag = .SD[[seedObsFlag]],
                     areaSownValue = .SD[[areaSownValue]],
                     areaSownObsFlag = .SD[[areaSownObsFlag]],
                     seedRateValue = .SD[[seedRateValue]],
                     seedRateFlag = .SD[[seedRateFlag]],
                     imputedFlag = imputedFlag)), by = byKey]
    data[, `:=`(c(seedRateValue, seedRateFlag), NULL)]
}
                    

## Obtain the valid year range of each country
getValidRange = function(dataContext){
    countryTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = areaVar)
    countryTable =
        countryTable[type == "country", ]
    ## countryTable[, startDate := NULLtoNA(startDate)]
    ## countryTable[, endDate := NULLtoNA(endDate)]
    countryTable[, startDate := as.numeric(substr(startDate, 1, 4))]
    countryTable[, endDate := as.numeric(substr(endDate, 1, 4))]
    countryTable[is.na(startDate), startDate := -Inf]
    countryTable[is.na(endDate), endDate := Inf]
    countryTable
}


## Function to remove imputed data which corresponds to invalid time
## range.
validTimeData = function(data, areaName = areaVar,
    yearName = yearVar){
    validRange = getValidRange()
    validSubset =
        paste0(with(validRange,
                    paste0("(", areaName, " == ", code,
                           " & ", yearName, " > ", startDate,
                           " & ", yearName, " < ", endDate, ")")),
               collapse = " | ")
    valid = data[eval(parse(text = validSubset)), ]
    valid    
}    
    
SaveSeedData = function(data){
    ## Save the data back
    SaveData(domain = "agriculture",
             dataset = "agriculture",
             data = data,
             normalized = FALSE)
}


seed =
    getOfficialSeedData() %>%
    removeCarryForward(data = ., variable = "Value_measuredElement_5525") %>%
    buildCPCHierarchy(data = ., cpcItemVar = itemVar, levels = 3)
area =
    getAllAreaData() %>%
    imputeAreaSown(data = .,
                   valueAreaSown = "Value_measuredElement_5025", 
                   valueAreaHarvested = "Value_measuredElement_5312",
                   flagObsAreaSown = "flagObservationStatus_measuredElement_5025", 
                   flagObsAreaHarvested =
                       "flagObservationStatus_measuredElement_5312", 
                   flagMethodAreaSown = "flagMethod_measuredElement_5025",
                   imputedObsFlag = "I", 
                   imputedMethodFlag = "e")
climate = getWorldBankClimateData()



seedModelData =
    mergeAllSeedData(seedData = seed, area, climate) %>%
    .[Value_measuredElement_5525 > 1 & Value_measuredElement_5025 > 1, ]

seedLmeModel = 
    lmer(log(Value_measuredElement_5525) ~ Value_wbIndicator_SWS.FAO.TEMP +
             timePointYears + 
         (log(Value_measuredElement_5025)|cpcLvl3/measuredItemCPC:geographicAreaM49),
         data = seedModelData)


getSelectedSeedData = function(dataContext){
    
    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )

    selectedSeed =
        GetData(key = dataContext, normalized = FALSE, pivoting = newPivot)
    selectedSeed[, timePointYears := as.numeric(timePointYears)]
    selectedSeed
}

selectedSeed =
    getSelectedSeedData(swsContext.datasets[[1]]) %>%
    removeCarryForward(data = ., variable = "Value_measuredElement_5525") %>%
    buildCPCHierarchy(data = ., cpcItemVar = itemVar, levels = 3) %>%
    mergeAllSeedData(seedData = ., area, climate) %>%
    .[Value_measuredElement_5525 > 1 & Value_measuredElement_5025 > 1, ]

selectedSeed[, predicted :=
                 exp(predict(seedLmeModel, selectedSeed, allow.new.levels = TRUE))]


with(selectedSeed, plot(Value_measuredElement_5525, predicted))

selectedSeed[Value_measuredElement_5525 >= 6e6 & predicted <= 4e6, ]
