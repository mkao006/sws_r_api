cat("Loading packages...\n")

suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(tidyr)
})

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Running locally\n")
    
    ## Define directories
    apiDirectory1 = "~/Documents/Github/sws_r_api/r_modules/production_validation/faoswsProduction/"
    packageDirectory1 = "~/Documents/Github/sws_production/faoswsProduction/R/"
    apiDirectory2 = "~/Documents/Github/sws_r_api/r_modules/production_validation/faoswsValidation/"
    packageDirectory2 = "~/Documents/Github/sws_validation/faoswsValidation/R/"
    
    ## Get SWS Parameters
    GetTestEnvironment(
        # baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "e83ab505-a8bf-421c-a500-df7a8a8912db"
    )
    R_SWS_SHARE_PATH = paste0(apiDirectory1, "/..")

    ## Copy over scripts from package directory
    file.copy(from = dir(packageDirectory1, pattern = ".*\\.R$",
                         full.names = TRUE),
              to = apiDirectory1, overwrite = TRUE)
    file.copy(from = dir(packageDirectory2, pattern = ".*\\.R$",
                         full.names = TRUE),
              to = apiDirectory2, overwrite = TRUE)

    ## Source copied scripts for this local test
    for(file in dir(apiDirectory1, full.names = T))
        source(file)
    for(file in dir(apiDirectory2, full.names = T))
        source(file)
} else {
    cat("Running on the SWS\n")
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/browningj/production/validationModels"
}

# swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys = c("0111", "0112")
getValidationData = function(){
    allCountries =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")[type == "country", code]    
    pivot = list(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),
        Pivoting(code = "measuredElement", ascending = TRUE)
    )
    validationData = GetData(key = swsContext.datasets[[1]],
                             normalized = FALSE, pivoting = pivot)
    validationData = validationData[geographicAreaM49 %in% allCountries, ]
    validationData
}

cat("Setting up validation models...\n")

## Set up validation models
validationModels = list()
param = swsContext.computationParams
if(is.null(param$meanTest) || param$meanTest)
    validationModels = c(validationModels, basicMeanTest)
if(is.null(param$lmTest) || param$lmTest)
    validationModels = c(validationModels, basicLmTest)
if(is.null(param$ratioTest) || param$ratioTest)
    validationModels = c(validationModels, basicRatioTest)
intervalTest = function(y){
    basicIntervalTest(y = y,
            upper = ifelse(is.null(param$maxValue), Inf, param$maxValue),
            lower = ifelse(is.null(param$minValue), -Inf, param$minValue))
}
validationModels = c(validationModels, intervalTest)

cat("Fitting validation models...\n")

validatedData = getValidationData() %>%
    ## The flagmethod "s" is an error, probably a test by someone.
    ## The flagmethod "n" corresponds to missing data.
    .[!get(paste0("flagMethod_measuredElement_", param$elementCode)) %in%
          c("s", "n"), ] %>%
    validateData(., valueColumnName = paste0("Value_measuredElement_",
                                             param$elementCode),
                 byKey = c("geographicAreaM49", "measuredItemCPC"),
                 validationModels = validationModels) %>%
    ## Define severity based on the number of tests which were successful.
    ## Also, it must be between 0 and 5, so scale appropriately.
    .[, Severity := round(5 * flaggedCounts / successfulTests)]

cat("Restructuring validated data so it can be saved...\n")

valueColumns = (1:ncol(validatedData))[grep("Value_measuredElement",
                                            colnames(validatedData))]
validatedData = gather(data = validatedData, key = "measuredElement",
                       value = "Value", valueColumns)
# Remove values which don't exist in the database:
validatedData = validatedData[!is.na(Value), ]
validatedData = validatedData[, c("geographicAreaM49", "measuredItemCPC",
                                  "timePointYears", "measuredElement",
                                  "Severity"), with = FALSE]
validatedData[, measuredElement := gsub("Value_measuredElement_", "",
                                        measuredElement)]
validatedData[, Description := ""]

cat("Attempting to save validation data to database...\n")

SaveValidation(domain = "agriculture", dataset = "agriculture",
               validation = validatedData)

"Validation completed!"
