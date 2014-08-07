########################################################################
## Title: Temporary script for computing yield
## Date:2014-04-25
## Author: Michael. C. J. Kao
########################################################################


## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "4ad7bc88-5521-4be1-81e2-a1a5b3b6413f"
        )
}


## Function for obtaining the data and meta data.
getYieldData = function(dataContext){
    ## Setting the tuples, this should be accessed by the API
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
        Pivoting(code= "geographicAreaM49", ascending = TRUE),
        Pivoting(code= "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code= "measuredElement", ascending = TRUE)
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
computeYieldData = function(data, formulaTuples, prefixTuples){
    for(i in NROW(formulaTuples)){
        ## set the names
        valueNames =
            as.list(paste0(prefixTuples$valuePrefix, formulaTuples[i]))
        names(valueNames) =
            colnames(formulaTuples)
        flagObsNames =
            as.list(paste0(prefixTuples$flagObsPrefix,formulaTuples[i]))
        names(flagObsNames) = colnames(formulaTuples)
        ## Compute Value
        data[, eval(valueNames$productivity) :=
             computeRatio(get(valueNames$output),
                          get(valueNames$input))]             
        ## Compute observation flag
        data[, eval(flagObsNames$productivity) :=
             aggregateObservationFlag(get(flagObsNames$output),
                                      get(flagObsNames$input))]
        ## Assign method flag
        data[, eval(paste0(prefixTuples$flagMethod,
                           formulaTuples$productivity[i])) := "i"]
    }
}

## computeYieldData(data = query, formulaTuples = formulaTuples,
##                  prefixTuples = prefixTuples)


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
    compute = try(
        datasets = getYieldData(swsContext.datasets[[1]])
        with(datasets,
             {
                 computeYieldData(data = query,
                                  formulaTuples = formulaTuples,
                                  prefixTuples = prefixTuples)
                 saveYieldData(dataContext = swsContext.datasets[[1]],
                               data = query)
             }
             )
        )
    if(inherits(compute, "try-error")){
        print("Yield Module Failed")
    } else {
        print("Yield Module Executed Successfully")
    }
}

executeYieldModule()

