## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(faoswsProductionImputation)
library(data.table)
library(RPostgreSQL)
library(RJSONIO)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"

## set up for the test environment and parameters
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "916d93af-3000-4afb-9781-4fc74e77117d"
        )
}

## Function to get the yield formula triplets
getYieldFormula = function(itemCode){
    condition =
        paste0("WHERE cpc_code IN (",
               paste0(shQuote(as.character(itemCode)),
                      collapse = ", "), ")")
    yieldFormula =
        GetTableData(schemaName = "ess",
                     tableName = "item_yield_elements",
                     whereClause = condition)
    setnames(yieldFormula,
             old = c("cpc_code", "element_31", "element_41",
                 "element_51", "factor"),
             new = c(itemVar, "input", "productivity",
                 "output", "unitConversion")
             )
    yieldFormula
}



## Function to get all country keys
##
## TODO (Michael): Need to get CIO to provide a proper functionality
##                 for this.

getAllCountryCode = function(dataContext){
    countryTable =
        GetCodeList(domain = slot(dataContext, "domain"),
                    dataset = slot(dataContext, "dataset"),
                    dimension = areaVar)
    unique(countryTable[type == "country", code])
}


## set1 = GetCodeList("agriculture", "agriculture", "geographicAreaM49")
## set1Code = set1[type == "country", code]
## keyTree =
##     unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
##                        dataset = swsContext.datasets[[1]]@dataset,
##                        dimension = areaVar,
##                        roots = "1062")
##            )    
## allCountryCode =
##     unique(adjacent2edge(keyTree)$children)
## set2Code = unique(allCountryCode)


getImputationData = function(dataContext){
    ## Setups
    formulaTuples =
        getYieldFormula(slot(slot(dataContext,
                                  "dimensions")$measuredItemCPC, "keys"))

    ## setting the prefix, also should be accessed by the API
    prefixTuples =
        data.table(
            valuePrefix = "Value_measuredElement_",
            flagObsPrefix = "flagObservationStatus_measuredElement_",
            flagMethodPrefix = "flagMethod_measuredElement_"
            )
    allCountryCode = getAllCountryCode(dataContext)
    selectedYears =
        slot(slot(dataContext, "dimensions")$timePointYears,
             "keys")

    ## Set 15 years as the default required number of years for
    ## imputation
    if(length(selectedYears) < 15)
        selectedYears =
            as.character((max(as.numeric(selectedYears) - 14)):
                             max(as.numeric(selectedYears)))

    ## Create the new expanded keys
    newKey = DatasetKey(
        domain = slot(dataContext, "domain"),
        dataset = slot(dataContext, "dataset"),
        dimensions = list(
            Dimension(name = areaVar,
                      keys = allCountryCode),
            Dimension(name = elementVar,
                      keys = unique(unlist(formulaTuples[,
                          list(input, productivity, output)]))),
            Dimension(name = itemVar,
                      keys = slot(slot(dataContext,
                          "dimensions")$measuredItemCPC, "keys")),
            Dimension(name = yearVar,
                      keys = selectedYears)
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
    

## Obtain the valid year range of each country
getValidRange = function(dataContext){
    countryTable =
        GetCodeList(domain = slot(dataContext, "domain"),
                    dataset = slot(dataContext, "dataset"),
                    dimension = areaVar)
    countryTable =
        countryTable[type == "country", ]
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
validImputedData = function(imputed, areaName = areaVar,
    yearName = yearVar, dataContext){
    validRange = getValidRange(dataContext)
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
    SaveData(domain = slot(dataContext, "domain"),
             dataset = slot(dataContext, "dataset"),
             data = data, normalized = FALSE)
}


executeImputationModule = function(){
    library(faoswsProductionImputation)
    library(faoswsFlag)
    library(faoswsUtil)

    fullKey = swsContext.datasets[[1]]
    subKey = fullKey
    uniqueItem = fullKey@dimensions$measuredItemCPC@keys
    for(singleItem in uniqueItem){
        subKey@dimensions$measuredItemCPC@keys = singleItem
        print(paste0("Imputation for item: ", singleItem))
        
        impute = try({
            datasets = getImputationData(subKey)
            ## This is a temporary hack until the API issue is
            ## resolved
            datasets$query =
                as.data.table(lapply(datasets$query, FUN = NULLtoNA))
            with(datasets, {
                ## NOTE (Michael): The yield should have been
                ##                 calculated a priori to the
                ##                 imputation modeul.
                
                ## Set the names
                assign("productionValue",
                    paste0(prefixTuples$valuePrefix,
                           formulaTuples$output),
                       envir = .GlobalEnv)
                assign("productionObservationFlag",
                    paste0(prefixTuples$flagObsPrefix,
                           formulaTuples$output),
                       envir = .GlobalEnv)
                assign("productionMethodFlag",
                    paste0(prefixTuples$flagMethodPrefix,
                           formulaTuples$output),
                       envir = .GlobalEnv)
                assign("areaHarvestedValue",
                    paste0(prefixTuples$valuePrefix,
                           formulaTuples$input),
                       envir = .GlobalEnv)
                assign("areaHarvestedObservationFlag",
                    paste0(prefixTuples$flagObsPrefix,
                           formulaTuples$input),
                       envir = .GlobalEnv)
                assign("areaHarvestedMethodFlag",
                    paste0(prefixTuples$flagMethodPrefix,
                           formulaTuples$input),
                       envir = .GlobalEnv)
                assign("yieldValue",
                    paste0(prefixTuples$valuePrefix,
                           formulaTuples$productivity),
                       envir = .GlobalEnv)
                assign("yieldObservationFlag",
                    paste0(prefixTuples$flagObsPrefix,
                           formulaTuples$productivity),
                       envir = .GlobalEnv)
                assign("yieldMethodFlag",
                    paste0(prefixTuples$flagMethodPrefix,
                           formulaTuples$productivity),
                       envir = .GlobalEnv)

                ## Recompute the yield
                computeYield(productionValue = productionValue,
                             productionObservationFlag =
                                 productionObservationFlag,
                             areaHarvestedValue =
                                 areaHarvestedValue,
                             areaHarvestedObservationFlag =
                                 areaHarvestedObservationFlag,
                             yieldValue = yieldValue,
                             yieldObservationFlag =
                                 yieldObservationFlag,
                             yieldMethodFlag = yieldMethodFlag,
                             newMethodFlag = "i",
                             flagTable = faoswsFlagTable,
                             data = query,
                             unitConversion =
                                 formulaTuples$unitConversion)

                ## Impute the dataset
                yieldDefaultFormula =
                    paste0(yieldValue, " ~ -1 + (1 + bs(timePointYears, df = 2, degree = 1)|geographicAreaM49)")
                
                imputed = imputeProductionDomain(data = query,
                    productionValue = productionValue,
                    productionObservationFlag =
                        productionObservationFlag,
                    productionMethodFlag = productionMethodFlag,
                    areaHarvestedValue = areaHarvestedValue,
                    areaHarvestedObservationFlag =
                        areaHarvestedObservationFlag,
                    areaHarvestedMethodFlag =
                        areaHarvestedMethodFlag,
                    yieldValue = yieldValue,
                    yieldObservationFlag = yieldObservationFlag,
                    yieldMethodFlag = yieldMethodFlag,
                    yearValue = yearVar,
                    flagTable = faoswsFlagTable,
                    removePriorImputation = TRUE,
                    removeConflictValues = TRUE,
                    imputedFlag = "E",
                    imputationFlag = "I",
                    newMethodFlag = "e",
                    naFlag = "M",
                    maxdf = 5,
                    byKey = areaVar,
                    restrictWeights = TRUE,
                    maximumWeights = 0.7,
                    yieldFormula =
                        yieldDefaultFormula)

                ## Validate data
                valid = validImputedData(imputed = imputed,
                    dataContext = subKey)

                ## Save back
                saveImputedData(subKey, valid)
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
}

executeImputationModule()
