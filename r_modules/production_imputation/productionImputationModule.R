## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(faoswsProductionImputation)
library(data.table)
library(FAOSTAT)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "f9886fc4-7107-455b-ab31-7fa0290481ab"
        )
}



## Function to get all country keys
##
## TODO (Michael): Need to get CIO to provide a proper functionality
##                 for this.

getAllCountryCode = function(){
    ## 1062 is geographical world
    keyTree =
        unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
                           dataset = swsContext.datasets[[1]]@dataset,
                           dimension = "geographicAreaM49",
                           roots = "1062")
               )    
    allCountryCode =
        unique(adjacent2edge(keyTree)$children)
    allCountryCode[allCountryCode %in% FAOcountryProfile$UN_CODE]
}


getImputationData = function(dataContext){
    ## Setups
    formulaTuples =
        data.table(
            output = "5510",
            input = "5312",
            productivity = "5421"
            )    

    ## setting the prefix, also should be accessed by the API
    prefixTuples =
        data.table(
            valuePrefix = "Value_measuredElement_",
            flagObsPrefix = "flagObservationStatus_measuredElement_",
            flagMethodPrefix = "flagMethod_measuredElement_"
            )
    allCountryCode = getAllCountryCode()
    
    ## Create the new expanded keys
    newKey = DatasetKey(
        domain = slot(swsContext.datasets[[1]], "domain"),
        dataset = slot(swsContext.datasets[[1]], "dataset"),
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountryCode),
            Dimension(name = "measuredElement",
                      keys = slot(slot(swsContext.datasets[[1]],
                          "dimensions")$measuredElement, "keys")),
            Dimension(name = "measuredItemCPC",
                      keys = slot(slot(swsContext.datasets[[1]],
                          "dimensions")$measuredItemCPC, "keys")),
            Dimension(name = "timePointYears",
                      keys = slot(slot(swsContext.datasets[[1]],
                          "dimensions")$timePointYears, "keys"))
            )
        )


    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code= "geographicAreaM49", ascending = TRUE),
        Pivoting(code= "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code= "measuredElement", ascending = TRUE)
        )

    ## Query the data
    query = GetData(
        ## key = swsContext.datasets[[1]],
        key = newKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )
    ## Convert time to numeric
    query[, timePointYears := as.numeric(timePointYears)]

    list(query = query,
         formulaTuples = formulaTuples,
         prefixTuples = prefixTuples)
}
    
## Function to turn lists of NULL to vector of NA
NULLtoNA = function(nullList){
    vector = rep(NA, length = length(nullList))
    validEntry = which(sapply(nullList, FUN = function(x) !is.null(x)))
    vector[validEntry] =
        unlist(nullList[validEntry])
    vector
}

## Obtain the valid year range of each country
getValidRange = function(){
    countryTable =
        GetCodeList("agriculture", "agriculture", "geographicAreaM49")
    countryTable =
        countryTable[code %in% FAOcountryProfile$UN_CODE]
    countryTable[, type := NULLtoNA(type)]
    countryTable[, startDate := NULLtoNA(startDate)]
    countryTable[, endDate := NULLtoNA(endDate)]
    countryTable[, startDate := as.numeric(substr(startDate, 1, 4))]
    countryTable[, endDate := as.numeric(substr(endDate, 1, 4))]
    countryTable[is.na(startDate), startDate := -Inf]
    countryTable[is.na(endDate), endDate := Inf]
    countryTable
}


## Function to remove imputed data which corresponds to invalid time
## range.
validImputedData = function(imputed, areaName = "geographicAreaM49",
    yearName = "timePointYears"){
    validRange = getValidRange()
    validSubset =
        paste0(with(validRange,
                    paste0("(", areaName, " == ", code,
                           " & ", yearName, " > ", startDate,
                           " & ", yearName, " < ", endDate, ")")),
               collapse = " | ")
    valid = imputed[eval(parse(text = validSubset)), ]
    valid    
}    


## Function to save data back
saveImputedData = function(dataContext, data){
    ## Should only the selected country be saved, or the whole set?
    SaveData(domain = slot(dataContext[[1]], "domain"),
             dataset = slot(dataContext[[1]], "dataset"),
             data = data, normalized = FALSE)
}


executeImputationModule = function(){
    library(faoswsProductionImputation)
    library(faoswsFlag)
    library(faoswsExtra)
    ## Maybe we can put the for loop here for the multiple elements.
    impute = try(
        {
            datasets = getImputationData(swsContext.datasets[[1]])
            with(datasets,
                 {
                     ## NOTE (Michael): The yield should have been calculated a priori to
                     ##                 the imputation modeul.

                     ## Set the names
                     productionValue = paste0(prefixTuples$valuePrefix, formulaTuples$output)
                     productionObservationFlag = paste0(prefixTuples$flagObsPrefix, formulaTuples$output)
                     productionMethodFlag = paste0(prefixTuples$flagMethodPrefix, formulaTuples$output)
                     areaHarvestedValue = paste0(prefixTuples$valuePrefix, formulaTuples$input)
                     areaHarvestedObservationFlag = paste0(prefixTuples$flagObsPrefix, formulaTuples$input)
                     areaHarvestedMethodFlag = paste0(prefixTuples$flagMethodPrefix, formulaTuples$input)
                     yieldValue = paste0(prefixTuples$valuePrefix, formulaTuples$productivity)
                     yieldObservationFlag = paste0(prefixTuples$flagObsPrefix, formulaTuples$productivity)
                     yieldMethodFlag = paste0(prefixTuples$flagMethodPrefix, formulaTuples$productivity)

                     
                     ## Recompute the yield
                     computeYield(productionValue = productionValue,
                                  productionObservationFlag = productionObservationFlag,
                                  areaHarvestedValue = areaHarvestedValue,
                                  areaHarvestedObservationFlag = areaHarvestedObservationFlag,
                                  yieldValue = yieldValue,
                                  yieldObservationFlag = yieldObservationFlag,
                                  yieldMethodFlag = yieldMethodFlag,
                                  newMethodFlag = "i", flagTable = faoswsFlagTable,
                                  data = query)
                     
                     ## Impute the dataset
                     imputed =
                         imputeProductionDomain(data = query,
                                                productionValue = productionValue,
                                                productionObservationFlag = productionObservationFlag,
                                                productionMethodFlag = productionMethodFlag,
                                                areaHarvestedValue = areaHarvestedValue,
                                                areaHarvestedObservationFlag = areaHarvestedObservationFlag,
                                                areaHarvestedMethodFlag = areaHarvestedMethodFlag,
                                                yieldValue = yieldValue,
                                                yieldObservationFlag = yieldObservationFlag,
                                                yieldMethodFlag = yieldMethodFlag,
                                                yearValue = "timePointYears",
                                                flagTable = faoswsFlagTable,
                                                removePriorImputation = TRUE,
                                                removeConflictValues = TRUE,
                                                imputedFlag = "E",
                                                imputationFlag = "I",
                                                newMethodFlag = "e",
                                                naFlag = "M",
                                                maxdf = 5,
                                                byKey = "geographicAreaM49",
                                                restrictWeights = TRUE,
                                                maximumWeights = 0.7)
                     valid = validImputedData(imputed)
                     ## Save back
                     saveImputedData(swsContext.datasets, valid)
                 }
                 )
        }
        )
    if(inherits(impute, "try-error")){
        print("Imputation Module Failed")
    } else {
        print("Imputation Module Executed Successfully")
    }
}


executeImputationModule()

