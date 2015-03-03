## load the library
library(faosws)
library(faoswsUtil)
library(faoswsFlag)
library(data.table)
library(FAOSTAT)
library(lattice)
library(reshape2)
library(entropy)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "7bc2ab09-72b9-4e54-b79f-d9132a4e72fe"
        )
}

newPivot = c(
    Pivoting(code= "geographicAreaM49", ascending = TRUE),
    Pivoting(code= "measuredItemCPC", ascending = TRUE),
    Pivoting(code = "timePointYears", ascending = FALSE),
    Pivoting(code= "measuredElement", ascending = TRUE)
    )

newKey = swsContext.datasets

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

getAllItemCode = function(){
    items =
        GetCodeTree(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "measuredItemCPC")$parent
    items[-c(grep("[A-Z]", items), 1)]
}

## Create new key and download data, the history is for the whole
## world since 1970 for wheat.
## newKey[[1]]@dimensions$geographicAreaM49@keys = getAllCountryCode()
## newKey[[1]]@dimensions$timePointYears@keys = as.character(1970:2013)
## newKey[[1]]@dimensions$measuredItemCPC@keys = "0111"

## ## Compute the table
## history = GetHistory(newKey[[1]], newPivot)
## history[, timePointYears := as.numeric(timePointYears)]
## (obsTable = computeFlagWeight(history, method = "entropy"))


## ## Now lets change commodity
## newKey[[1]]@dimensions$geographicAreaM49@keys = getAllCountryCode()
## newKey[[1]]@dimensions$timePointYears@keys = as.character(1970:2013)
## newKey[[1]]@dimensions$measuredItemCPC@keys = "01657"

## newKey[[1]]@dimensions$geographicAreaM49@keys = getAllCountryCode()
## newKey[[1]]@dimensions$timePointYears@keys = as.character(1970:2013)
## newKey[[1]]@dimensions$measuredItemCPC@keys = "01450"

## history = GetHistory(newKey[[1]], newPivot)
## history[, timePointYears := as.numeric(timePointYears)]
## (obsTable = computeFlagWeight(history, method = "entropy"))




newKey[[1]]@dimensions$geographicAreaM49@keys = getAllCountryCode()
newKey[[1]]@dimensions$timePointYears@keys = as.character(1980:2013)
newKey[[1]]@dimensions$measuredItemCPC@keys = getAllItemCode()
history = GetHistory(newKey[[1]], newPivot)
history[, timePointYears := as.numeric(timePointYears)]
historyImputeRemoved = history[flagObservationStatus != "I", ]
(obsTable = computeFlagWeight(historyImputeRemoved))

## Need to fix the problem when there are two weights that are equal.
