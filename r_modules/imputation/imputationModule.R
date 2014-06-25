library(reshape2)
library(earth)
library(forecast)
library(FAOSTAT)
library(lattice)
library(lme4)
library(data.table)
library(splines)
source("~/Github/sws_imputation/support_functions/ensembleImpute.R")
source("~/Github/sws_imputation/support_functions/naiveImputation.R")
source("~/Github/sws_imputation/support_functions/containInfo.R")
source("~/Github/sws_imputation/support_functions/imputeYield.R")
source("~/Github/sws_imputation/support_functions/imputeProduction.R")
source("~/Github/sws_r_api/r_modules/compute_yield/flag2weight.R")
source("~/Github/sws_r_api/r_modules/compute_yield/weight2flag.R")
source("~/Github/sws_r_api/r_modules/compute_yield/computeYield.R")

## load the library
require("faosws")
library(data.table)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "003c2e91-18ce-4302-a6ca-65bae0fce97c"
        )
}

## Get all country keys
##
## TODO (Michael): Need to get CIO to provide a proper functionality
##                 for this.
allCountryCode =
    slot(
        slot(swsContext.datasets[[1]], "dimensions")$geographicAreaM49,
        "keys")

## Create hte new expanded keys
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
    key = swsContext.datasets[[1]],
    flags = TRUE,
    normalized = FALSE,
    pivoting = newPivot
)


## Hack the names for now
productionVar = "Value_measuredElement_5510"
areaHarvestedVar = "Value_measuredElement_5312"
yieldVar = "Value_measuredElement_5421"

productionFlagObservation = "flagObservationStatus_measuredElement_5510"
areaHarvestedFlagObservation = "flagObservationStatus_measuredElement_5312"
yieldFlagObservation = "flagObservationStatus_measuredElement_5421"


productionFlagMethod = "flagMethod_measuredElement_5510"
areaHarvestedFlagMethod = "flagMethod_measuredElement_5312"
yieldFlagMethod = "flagMethod_measuredElement_5421"

## Temporary flag table
flagTable.dt =
    data.table(flagObservationStatus = c("", "T", "E", "I", "M"),
               flagObservationWeights = c(1, 0.8, 0.75, 0.5, 0))

## Create weights
query[, eval(parse(text = paste0(gsub("flag", "weight", productionFlagObservation), " := flag2weight(", productionFlagObservation, ",flagTable = flagTable.dt)")))]

query[, eval(parse(text = paste0(gsub("flag", "weight", areaHarvestedFlagObservation), " := flag2weight(", areaHarvestedFlagObservation, ",flagTable = flagTable.dt)")))]

query[, eval(parse(text = paste0(gsub("flag", "weight", yieldFlagObservation), " := pmin(", gsub("flag", "weight", productionFlagObservation), ", ", gsub("flag", "weight", areaHarvestedFlagObservation), ")")))]

## Build the yield formula
yieldFormula = formula(paste0(yieldVar, " ~ ", "timePointYears", "|",
    "geographicAreaM49", ":", "measuredItemCPC"))

## Create the indexing
index = c("geographicAreaM49", "measuredItemCPC")

## Impute yield
queryYieldImputed =
    imputeYield(formula = yieldFormula, data = query,
                weights = parse(text = paste0(gsub("flag", "weight",
                                    yieldFlagObservation))),
                index = index)

## calculate production if area harvested and yield both exist
queryYieldImputed[, eval(parse(text = paste0(productionVar, " := ",
                                   areaHarvestedVar, " * ", yieldVar)))]

## Impute production
queryProductionImputed =
    imputeProduction(queryYieldImputed, productionVar, index)

## balance area harvested
queryAllImputed =
    queryProductionImputed[, eval(parse(text = paste0(areaHarvestedVar,
                                            " := ", productionVar,
                                            "/", yieldVar)))]

