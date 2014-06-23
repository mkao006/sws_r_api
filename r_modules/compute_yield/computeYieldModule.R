########################################################################
## Title: Temporary script for computing yield
## Date:2014-04-25
########################################################################

## NOTE (Michael): Need to find a way of handling formulas.

## load the library
require("faosws")
library(data.table)

## Set up for the test environment
if(Sys.getenv("USERNAME") == "kao"){
    GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                       token = "6d98803b-e623-43dc-b137-4130f0dc140c")
}

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



## Function for computing the yield
computeYield = function(production, areaHarvested){
    ifelse(production == 0 | areaHarvested == 0, NA, production/areaHarvested)
}

## Compute the yield
query[, Value_measuredElement_5421 :=
      as.numeric(computeYield(Value_measuredElement_5510,
                              Value_measuredElement_5312))]

## Temporary flag table
flagTable.dt =
    data.table(flagObservationStatus = c("", "T", "E", "I", "M"),
               weights = c(1, 0.8, 0.75, 0.5, 0))

## Obtain the weight of production and area harvested from the flag
query[, flagObservationStatusWeight_measuredElement_5510 :=
      flagTable.dt$weights[match(flagObservationStatus_measuredElement_5510,
                                 flagTable.dt$flagObservationStatus)]]

query[, flagObservationStatusWeight_measuredElement_5312 :=
      flagTable.dt$weights[match(flagObservationStatus_measuredElement_5312,
                                 flagTable.dt$flagObservationStatus)]]

## Calculate flag for yield
query[, flagObservationStatusWeight_measuredElement_5421 :=
      pmin(flagObservationStatusWeight_measuredElement_5510,
           flagObservationStatusWeight_measuredElement_5312)]
query[, flagObservationStatus_measuredElement_5421 :=
      as.character(ifelse(is.na(Value_measuredElement_5421), NA,
                          flagTable.dt$flagObservationStatus[match(flagObservationStatusWeight_measuredElement_5421, flagTable.dt$weights)]))]

query[, flagMethod_measuredElement_5421 :=
      as.character(ifelse(is.na(Value_measuredElement_5421), NA, "i"))]

## Add the newly computed key back to the dimension
addKey(swsContext.datasets[[1]]@dimensions[["measuredElement"]]) = "5421"

## ## Create column for comments as required in the API documentation
## query[, comment := "yield computed based on production and area harvested"]

## write back to the data
SaveData(domain = "agriculture", dataset = "agriculture", data = query,
         normalized = FALSE)
