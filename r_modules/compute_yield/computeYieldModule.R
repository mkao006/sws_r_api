########################################################################
## Title: Yield Computation Module for SWS
## Date:2014-04-25
## Author: Michael. C. J. Kao
########################################################################

library(data.table)
library(faosws)
library(faoswsFlag)
library(faoswsExtra)
library(RPostgreSQL)

connectionProfile =
    list(drv = PostgreSQL(),
         user = R_SWS_DATABASE_USER,
         password = R_SWS_DATABASE_USER_PASSWD,
         dbname = R_SWS_DATABASE_NAME,
         host = R_SWS_DATABASE_HOST,
         port = R_SWS_DATABASE_PORT)

## set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
}

## Function to get the yield formula triplets
getYieldFormula = function(itemCode, connectionProfile){
    con = do.call(what = "dbConnect", args = connectionProfile)
    query = paste0("SELECT * FROM ess.item_yield_elements WHERE cpc_code IN (", paste0("'", itemCode, "'", collapse = ", "), ")")
    yieldFormula = data.table(dbGetQuery(con, query))
    setnames(yieldFormula,
             old = c("cpc_code", "element_31", "element_41",
                 "element_51", "factor"),
             new = c("measuredItemCPC", "input", "productivity",
                 "output", "unitConversion")
             )
    dbDisconnect(con)
    yieldFormula
}



## Function for obtaining the data and meta data.
getYieldData = function(dataContext){
    ## setting the prefix, also should be accessed by the API
    prefixTuples =
        data.table(
            valuePrefix = "Value_measuredElement_",
            flagObsPrefix = "flagObservationStatus_measuredElement_",
            flagMethodPrefix = "flagMethod_measuredElement_"
            )

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
        )
    
    ## Query the data
    query = GetData(
        key = dataContext,
        flags = TRUE,
        normalized = FALSE,
        pivoting = newPivot
        )
    
    list(query = query,
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


list2vec = function(x){
    tmp = unlist(x)
    if(is.null(tmp))
        tmp = rep(NA, length(x))
    tmp
}

## Function to execute the whole yield module
executeYieldModule = function(){
    fullKey = swsContext.datasets[[1]]
    subKey = fullKey
    uniqueItem = fullKey@dimensions$measuredItemCPC@keys
    for(singleItem in uniqueItem){
        subKey@dimensions$measuredItemCPC@keys = singleItem
        yieldFormula = getYieldFormula(singleItem, connectionProfile)
        unitConversion = yieldFormula[, unitConversion]
        subKey@dimensions$measuredElement@keys =
            yieldFormula[, c(input, productivity, output)]
        
        compute = try(
            {
                datasets = getYieldData(subKey)
                with(datasets,
                     {
                         computeYieldData(data = query,
                                          formulaTuples = yieldFormula,
                                          prefixTuples = prefixTuples,
                                          unitConversion =
                                              unitConversion)
                         convertedData =
                             as.data.table(lapply(query, list2vec))
                         saveYieldData(dataContext = subKey,
                                       data = convertedData)
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
