suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(reshape2)
    library(caret)
})

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    ## Define directories
    apiDirectory1 = "~/Documents/Github/sws_r_api/r_modules/production_validation/faoswsProduction/"
    packageDirectory1 = "~/Documents/Github/sws_production/faoswsProduction/R/"
    apiDirectory2 = "~/Documents/Github/sws_r_api/r_modules/production_validation/faoswsValidation/"
    packageDirectory2 = "~/Documents/Github/sws_validation/faoswsValidation/R/"
    
    ## Get SWS Parameters
    GetTestEnvironment(
        # baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "13f167bf-839f-40a6-b724-fcc25b0ec0df"
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
    R_SWS_SHARE_PATH = "/work/SWS_R_Share/browningj/production/validationModels"
    updateModel = as.logical(swsContext.computationParams$updateModel)
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
    validationData = GetData(swsContext.datasets[[1]], normalized = FALSE,
                             pivoting = pivot)
    validationData = validationData[geographicAreaM49 %in% allCountries, ]
    validationData
}

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

getValidationData() %>%
    ## The flagmethod "s" is an error, probably a test by someone.
    ## The flagmethod "n" corresponds to missing data.
    .[!get(paste0("flagMethod_measuredElement_", param$elementCode)) %in%
          c("s", "n"), ] %>%
    validateData(., valueColumnName = paste0("Value_measuredElement_",
                                             param$elementCode),
                 byKey = c("geographicAreaM49", "measuredItemCPC"),
                 validationModels = validationModels) %>%
    ## Define severity based on the number of tests which were successful
    .[, Severity := flaggedCounts / successfulTests] %>%
    SaveValidation(domain = "agriculture",
                   dataset = "agriculture",
                   validation = .)

"Validation completed!"
