## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(faoswsProductionImputation)
library(data.table)
library(splines)
library(lme4)

## Setting up variables
areaVar = "geographicAreaM49"
yearVar = "timePointYears"
itemVar = "measuredItemCPC"
elementVar = "measuredElement"

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    ## Define directories
    apiDirectory1 = "~/Documents/Github/sws_r_api/r_modules/production_imputation/faoswsProduction/"
    apiDirectory2 = "~/Documents/Github/sws_r_api/r_modules/production_imputation/faoswsProductionImputation/"
    packageDirectory1 = "~/Documents/Github/sws_production/faoswsProduction/R/"
    packageDirectory2 = "~/Documents/Github/sws_imputation/codes/R/"
    
    ## Get SWS Parameters
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "22fb397d-d9e6-416d-acb8-717451714c62"
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "90bb0f92-e345-4401-945d-1e43af801167"
    )
    
    # Don't copy right now, as we're using original productionImputation pkg.
#     ## Copy over scripts from package directory
#     file.copy(from = dir(packageDirectory1, pattern = ".*\\.R$",
#                          full.names = TRUE),
#               to = apiDirectory, overwrite = TRUE)
#     file.copy(from = dir(packageDirectory2, pattern = ".*\\.R$",
#                          full.names = TRUE),
#               to = apiDirectory, overwrite = TRUE)

    ## Source copied scripts for this local test
    for(file in dir(apiDirectory1, full.names = T))
        source(file)
    for(file in dir(apiDirectory2, full.names = T))
        source(file)
}

## Function to get the yield formula triplets
getYieldFormula = function(itemCode){
    itemData = GetCodeList(domain = "agriculture", dataset = "agriculture",
                           dimension = "measuredItemCPC", codes = itemCode)
    itemData = itemData[!is.na(type), ]
    if(nrow(itemData) == 0)
        stop("No valid data to process!  Maybe the item code isn't in the ",
             "database?")
    uniqueItemTypes = unique(itemData$type)
    condition =
        paste0("WHERE item_type IN (",
               paste(paste0("'", as.character(uniqueItemTypes), "'"),
                      collapse = ", "), ")")
    yieldFormula =
        GetTableData(schemaName = "ess",
                     tableName = "item_type_yield_elements",
                     whereClause = condition)
    yieldFormula = merge.data.frame(itemData, yieldFormula,
                                    by.x = "type", by.y = "item_type")
    yieldFormula = yieldFormula[, c("code", "element_31", "element_41",
                                    "element_51", "factor")]
    yieldFormula = data.table(yieldFormula)
    setnames(yieldFormula,
             old = c("code", "element_31", "element_41",
                 "element_51", "factor"),
             new = c(itemVar, "input", "productivity",
                 "output", "unitConversion")
             )
    yieldFormula
}



## Function to get all country keys
getAllCountryCode = function(dataContext){
    countryTable =
        GetCodeList(domain = slot(dataContext, "domain"),
                    dataset = slot(dataContext, "dataset"),
                    dimension = areaVar)
    unique(countryTable[type == "country", code])
}


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
    
    ## Assign flags of "M" where data is missing
    elements = grepl("Value_measuredElement", colnames(query))
    elements = gsub("Value_measuredElement_", "", colnames(query)[elements])
    for(element in elements)
        query[is.na(get(paste0("Value_measuredElement_", element))),
              c(paste0("flagObservationStatus_measuredElement_", element)) := "M"]

    ## Remove data where flag is missing
    for(element in elements)
        remove0M(data = query,
            value = paste0("Value_measuredElement_", element),
            flag = paste0("flagObservationStatus_measuredElement_", element))
    
    list(query = query,
         formulaTuples = formulaTuples,
         prefixTuples = prefixTuples)
}

assignColumnNameVars = function(prefixTuples, formulaTuples){
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
}

executeImputationModule = function(){
    fullKey = swsContext.datasets[[1]]
    subKey = fullKey
    uniqueItem = fullKey@dimensions$measuredItemCPC@keys
    for(singleItem in uniqueItem){
        subKey@dimensions$measuredItemCPC@keys = singleItem
        print(paste0("Imputation for item: ", singleItem))
        
        impute = try({
            cat("Reading in the data...\n")
            datasets = getImputationData(subKey)
            ## This is a temporary hack until the API issue is
            ## resolved
            ## datasets$query =
            ##     as.data.table(lapply(datasets$query, FUN = NULLtoNA))

            ## NOTE (Michael): The yield should have been
            ##                 calculated a priori to the
            ##                 imputation module.
            
            ## Some commodities have multiple formulas.  For example, the LNSP
            ## (livestock non-primary) item type has beef, indigenous beef, and
            ## biological beef.  Rather than being different commodities, these
            ## three commodities are stored under the beef commodity with
            ## different element codes for production/yield/output.  So, we
            ## need to process each one and thus loop over all the
            ## formulaTuples (which specifies the multiple element codes if
            ## applicable).
            for(i in 1:nrow(datasets$formulaTuples)){
                cat("Processing pair", i, "of", nrow(datasets$formulaTuples),
                    "element triples.\n")
                
                ## Set the names
                assignColumnNameVars(prefixTuples = datasets$prefixTuples,
                                     formulaTuples = datasets$formulaTuples[i])
    
                ## Recompute the yield
                cat("Computing yield...\n")
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
                             data = datasets$query,
                             unitConversion =
                                 datasets$formulaTuples$unitConversion[i])
    
                ## Impute the dataset
                yieldDefaultFormula =
                    paste0(yieldValue, " ~ -1 + (1 + bs(timePointYears,",
                           "df = 2, degree = 1)|geographicAreaM49)")
                
                imputed = imputeProductionDomain(data = datasets$query,
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
    
    #                 ## Validate data: not necessary since saveProductionData does this
    #                 valid = faoswsUtil::removeInvalidDates(data = imputed)
    
                ## Save back to database
                saveProductionData(imputed,
                        areaHarvestedCode = datasets$formulaTuples$input[i],
                        yieldCode = datasets$formulaTuples$productivity[i],
                        productionCode = datasets$formulaTuples$output[i],
                        verbose = TRUE)
            } # close item type for loop
        }) # close try block
        if(inherits(impute, "try-error")){
            print("Imputation Module Failed")
        } else {
            print("Imputation Module Executed Successfully")
        }
    }
}

executeImputationModule()
"Module completed!"