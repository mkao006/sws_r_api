########################################################################
## Title: Temporary script for computing yield
## Date:2014-04-25
########################################################################

## NOTE (Michael): Need to find a way of handling formulas.

## load the library
library(faosws)
source("computeYield.R")
source("computeYieldFlagObservationStatus.R")
source("flag2weight.R")
source("weight2flag.R")

## Set up for the test environment
GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                   token = "d8756927-3c5c-45c3-b09d-60cb0651d8dd")


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



## Temporary flag table
flagTable.dt =
    data.table(flagObservationStatus = c("", "T", "E", "I", "M"),
               flagObservationWeights = c(1, 0.8, 0.75, 0.5, 0))

## Compute the yield
query[, Value_measuredElement_5421 :=
      computeYield(Value_measuredElement_5510,
                   Value_measuredElement_5312)]

## Compute observation flags for yield
query[,
      flagObservationStatus_measuredElement_5421 :=
      computeYieldFlagObservationStatus(flagObservationStatus_measuredElement_5510, flagObservationStatus_measuredElement_5312, flagTable.dt)]

## Compute the method flag for yield
query[, flagMethod_measuredElement_5421 :=
      as.character(ifelse(is.na(Value_measuredElement_5421), NA, "i"))]

## Add the newly computed key back to the dimension
addKey(swsContext.datasets[[1]]@dimensions[["measuredElement"]]) = "5421"

## write back to the data
SaveData(domain = "agriculture", dataset = "agriculture", data = query,
         normalized = FALSE)
