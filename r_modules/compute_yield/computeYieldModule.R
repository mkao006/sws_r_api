########################################################################
## Title: Yield Computation Module for SWS
## Date:2014-04-25
## Author: Michael. C. J. Kao
########################################################################

library(data.table)
library(faosws)

## set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "5d9b8d4a-0989-4b50-869f-cd0bc566fd18"
        )
}


## Function for obtaining the data and meta data.
getYieldData = function(dataContext){
    ## Setting the formula triplet, this should be accessed by the API
    ## formulaTuples =
    ##     data.table(
    ##         output = c("5510", "5510", "5510"),
    ##         input = c("5312", "5320", "5313"),
    ##         productivity = c("5421", "5417", "5417")
    ##         )
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
         formulaTuples = formulaTuples,
         prefixTuples = prefixTuples)
}





## Function to compute the yield data
computeYieldData = function(data, formulaTuples, prefixTuples,
    newMethodFlag = "i", flagTable = faoswsFlagTable){
    computeYield(productionValue =
                 paste0(prefixTuples$valuePrefix, formulaTuples$output),
                 productionObservationFlag =
                 paste0(prefixTuples$flagObsPrefix,
                        formulaTuples$output),
                 areaHarvestedValue =
                 paste0(prefixTuples$valuePrefix, formulaTuples$input),
                 areaHarvestedObservationFlag =
                 paste0(prefixTuples$flagObsPrefix, formulaTuples$input),
                 yieldValue = paste0(prefixTuples$valuePrefix,
                                     formulaTuples$productivity),
                 yieldObservationFlag =
                 paste0(prefixTuples$flagObsPrefix,
                        formulaTuples$productivity),
                 yieldMethodFlag = paste0(prefixTuples$flagMethodPrefix,
                     formulaTuples$productivity),
                 newMethodFlag = newMethodFlag,
                 flagTable = flagTable, data = data)
}    



    
## Function to save data back
saveYieldData = function(dataContext, data){
    SaveData(domain = slot(dataContext[[1]], "domain"),
             dataset = slot(dataContext[[1]], "dataset"),
             data = data, normalized = FALSE)
}


## Function to execute the whole yield module
executeYieldModule = function(){
    require(faoswsFlag)
    require(faoswsExtra)
    ## Maybe we can put the for loop here for the multiple elements.
    compute = try(
        {
            datasets = getYieldData(swsContext.datasets[[1]])
            with(datasets,
                 {
                     computeYieldData(data = query,
                                      formulaTuples = formulaTuples,
                                      prefixTuples = prefixTuples)
                     saveYieldData(dataContext = swsContext.datasets,
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


executeYieldModule()

