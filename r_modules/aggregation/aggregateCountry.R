## load the library
require("faosws")

## Set up for the test environment
if(Sys.getenv("USERNAME") == "kao"){
    GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                       token = "")
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


regionMapping = MappingTableKey(mappingTable = "tradeHS2CPC",
    dimensions = c(swsContext.datasets[[1]]@dimensions[["geographicAreaM49"]]))


## Execute the mapping table call.
GetMapping(key = regionMapping)


## Repeat the call without filters.
k <- MappingTableKey(mappingTable = "tradeHS2CPC")
GetMapping(key = k)
