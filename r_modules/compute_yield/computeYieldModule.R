########################################################################
## Title: Temporary script for computing yield
## Date:2014-04-25
########################################################################

## NOTE (Michael): Need to find a way of handling formulas.

## load the library
library(faosws)
library(faosws_flag)
library(faosws_extra)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "2a4d62c3-e776-4854-a013-0a4cdb5d7541"
        )
}


## Pivot to vectorize yield computation
newPivot = c(
    Pivoting(code= "geographicAreaM49", ascending = TRUE),
    Pivoting(code= "measuredItemCPC", ascending = TRUE),
    Pivoting(code = "timePointYears", ascending = FALSE),
    Pivoting(code= "measuredElement", ascending = TRUE)
    )

## NOTE (Michael): Check what is the accessor function for the
##                 dimensions

## Query the data
query = GetData(
    key = swsContext.datasets[[1]],
    flags = TRUE,
    normalized = FALSE,
    pivoting = newPivot
)


## Compute the yield
query[, Value_measuredElement_5421 :=
      computeRatio(Value_measuredElement_5510,
                   Value_measuredElement_5312)]

## Compute observation flags for yield
query[, flagObservationStatus_measuredElement_5421 :=
      computeYieldFlagObservationStatus(flagObservationStatus_measuredElement_5510,
                                        flagObservationStatus_measuredElement_5312,
                                        flagTable.dt)]

## Compute the method flags for yield
query[, flagMethod_measuredElement_5421 :=
      as.character(ifelse(is.na(Value_measuredElement_5421), NA, "i"))]

## Add the newly computed key back to the dimension
addKey(swsContext.datasets[[1]]@dimensions[["measuredElement"]]) = "5421"

## write back to the data
SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
         dataset = slot(swsContext.datasets[[1]], "dataset"),
         data = query, normalized = FALSE)
