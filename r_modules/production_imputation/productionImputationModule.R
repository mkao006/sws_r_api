## load the library
library(faosws)
library(faoswsExtra)
library(faoswsFlag)
library(faoswsProductionImputation)
library(data.table)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "bba5e7b2-9aa8-4dac-929f-2ffeab3e3b6d"
        )
}

## Get all country keys
##
## TODO (Michael): Need to get CIO to provide a proper functionality
##                 for this.
allCountryCode =
    slot(
        slot(swsContext.datasets[[1]], "dimensions")$geographicAreaM49,
        "keys"
        )

## Create the new expanded keys
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

## NOTE (Michael): The yield should have been calculated a priori to
##                 the imputation modeul.
query[, Value_measuredElement_5421 :=
      computeRatio(Value_measuredElement_5510,
                   Value_measuredElement_5312)]
query[, flagObservationStatus_measuredElement_5421 :=
      aggregateObservationFlag(
          flagObservationStatus_measuredElement_5312,
          flagObservationStatus_measuredElement_5510
          )]
query[, flagMethod_measuredElement_5421 := NULL]
query[, flagMethod_measuredElement_5421 := "c"]

## Convert time to numeric
query[, timePointYears := as.numeric(timePointYears)]

## Create the indexing
index = c("geographicAreaM49")

## Build the yield formula
yieldFormula = Value_measuredElement_5421 ~ -1 +
    (1 + timePointYears|geographicAreaM49)

## Impute the data
imputed = imputeProductionDomain(data = query,
    productionVar = "Value_measuredElement_5510",
    areaHarvestedVar = "Value_measuredElement_5312",
    yieldVar = "Value_measuredElement_5421",
    productionObservationFlag =
        "flagObservationStatus_measuredElement_5510",
    areaHarvestedObservationFlag =
        "flagObservationStatus_measuredElement_5312",
    yieldObservationFlag =
        "flagObservationStatus_measuredElement_5421",
    index = index, yieldFormula = yieldFormula,
    flagTable = faoswsFlagTable)


## Save data back
SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
         datasets = slot(swsContext.datasets[[1]], "datasets"),
         data = imputed, normalized = FALSE)
         
