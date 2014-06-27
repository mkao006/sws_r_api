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


## Assign names to variable
productionVar = "Value_measuredElement_5510"
areaHarvestedVar = "Value_measuredElement_5312"
yieldVar = "Value_measuredElement_5421"

productionFlagObservation =
    "flagObservationStatus_measuredElement_5510"
areaHarvestedFlagObservation =
    "flagObservationStatus_measuredElement_5312"
yieldFlagObservation =
    "flagObservationStatus_measuredElement_5421"


productionWeightObservation =
    "weightObservationStatus_measuredElement_5510"
areaHarvestedWeightObservation =
    "weightObservationStatus_measuredElement_5312"
yieldWeightObservation =
    "weightObservationStatus_measuredElement_5421"

productionFlagMethod = "flagMethod_measuredElement_5510"
areaHarvestedFlagMethod = "flagMethod_measuredElement_5312"
yieldFlagMethod = "flagMethod_measuredElement_5421"


## Create weights
query[, eval(parse(text = paste0(productionWeightObservation,
                       " := flag2weight(", productionFlagObservation,
                       ",flagTable = flagTable)")))]
query[, eval(parse(text = paste0(areaHarvestedWeightObservation,
                       " := flag2weight(", areaHarvestedFlagObservation,
                       ",flagTable = flagTable)")))]
query[, eval(parse(text = paste0(yieldWeightObservation,
                       " := pmin(", productionWeightObservation,
                       ", ", areaHarvestedWeightObservation, ")")))]

## Compute yield
query[, Value_measuredElement_5421 :=
      computeRatio(Value_measuredElement_5510,
                   Value_measuredElement_5312)]

## Convert time to numeric
query[, timePointYears := as.numeric(timePointYears)]

## Create the indexing
index = c("geographicAreaM49")

## Build the yield formula
yieldFormula = formula(paste0(yieldVar, " ~ ", "timePointYears", "|",
    "geographicAreaM49"))

## Impute Yield
queryYieldImputed =
    imputeYield(formula = yieldFormula, data = query,
                weights =
                unlist(query[, yieldWeightObservation, with = FALSE]),
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
