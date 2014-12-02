########################################################################
## Title: Yield Computation Module for SWS
## Date:2014-04-25
## Author: Michael. C. J. Kao
########################################################################

library(data.table)
library(faosws)
library(faoswsFlag)
library(faoswsUtil)
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
        ## token = "ad7f16e3-d447-48ec-9d62-089f63bbc137"
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
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
             new = c("measuredItemCPC", "input", "productivity",
                 "output", "unitConversion")
             )
    yieldFormula
}
    
## Function for obtaining the data and meta data.
getYieldData = function(dataContext){
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

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
        )
    slot(slot(dataContext, "dimensions")$measuredElement, "keys") =
        unique(unlist(formulaTuples[, list(input,
                                           productivity, output)]))

    ## Query the data
    query = GetData(
        key = dataContext,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )
    
    list(query = query,
         formulaTuples = formulaTuples,
         prefixTuples = prefixTuples)
}





## Function to compute the yield data
computeYieldData = function(data, formulaTuples, prefixTuples,
    newMethodFlag = "i", flagTable = faoswsFlagTable, unitConversion){
    computeYield(productionValue =
                     paste0(prefixTuples$valuePrefix,
                            formulaTuples$output),
                 productionObservationFlag =
                     paste0(prefixTuples$flagObsPrefix,
                            formulaTuples$output),
                 areaHarvestedValue =
                     paste0(prefixTuples$valuePrefix,
                            formulaTuples$input),
                 areaHarvestedObservationFlag =
                     paste0(prefixTuples$flagObsPrefix,
                            formulaTuples$input),
                 yieldValue =
                     paste0(prefixTuples$valuePrefix,
                            formulaTuples$productivity),
                 yieldObservationFlag =
                     paste0(prefixTuples$flagObsPrefix,
                            formulaTuples$productivity),
                 yieldMethodFlag =
                     paste0(prefixTuples$flagMethodPrefix,
                            formulaTuples$productivity),
                 newMethodFlag = newMethodFlag,
                 flagTable = flagTable, data = data,
                 unitConversion = unitConversion)
}    



    
## Function to save data back
saveYieldData = function(dataContext, data){
    SaveData(domain = slot(dataContext, "domain"),
             dataset = slot(dataContext, "dataset"),
             data = data, normalized = FALSE)
}

## Function to execute the whole yield module
executeYieldModule = function(){
    fullKey = swsContext.datasets[[1]]
    subKey = fullKey
    uniqueItem = fullKey@dimensions$measuredItemCPC@keys
    for(singleItem in uniqueItem){
        subKey@dimensions$measuredItemCPC@keys = singleItem
        ## print(paste0("Computing Yield for item: ", singleItem))
        compute = try({
            datasets = getYieldData(subKey)
            datasets$query =
                as.data.table(lapply(datasets$query, FUN = NULLtoNA))
            with(datasets,
                 {
                     computeYieldData(data = query,
                                      formulaTuples = formulaTuples,
                                      prefixTuples = prefixTuples,
                                      unitConversion =
                                          formulaTuples$unitConversion)
                     saveYieldData(dataContext = subKey,
                                   data = query)
                 }
                 )
        }
        )
        
        if(inherits(compute, "try-error")){
            print("Yield Module Failed")
        } else {
            print("Yield Module Executed Successfully")
        }
    }
}


executeYieldModule()
