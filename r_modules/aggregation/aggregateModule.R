########################################################################
## Title: Module for aggregation
## Date: 2014-06-25
########################################################################

## load the library
library(faosws)
library(faoswsUtil)
library(data.table)

## set up for the test environment and parameters
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
DEBUG_MODE = Sys.getenv("R_DEBUG_MODE")

if(!exists("DEBUG_MODE") || DEBUG_MODE == ""){
    cat("Not on server, so setting up environment...\n")
    
    GetTestEnvironment(
        ## baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        ## token = "2a4d62c3-e776-4854-a013-0a4cdb5d7541"
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "a2dd0e14-1cdc-4486-bc4b-1f65d9ecad01"
    )
}

## CHECK (Michael): There are duplicate key in geographic key tree.
geoKeyTree =
    unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
                       dataset = swsContext.datasets[[1]]@dataset,
                       dimension = "timePointYears")
           )

## Convert the code tree to code table
keyTable = adjacent2edge(keyTree)
setnames(x = keyTable, old = "children", new = aggregationType)

## This is a hack to collapse the graph
if(aggregationType == "measuredItemCPC")
    keyTable[, parent := substr(parent, 1, 3)]

## Set up the new key
switch(aggregationType,
       "measuredItemCPC" = {
           newKey = DatasetKey(
               domain = slot(swsContext.datasets[[1]], "domain"),
               dataset = slot(swsContext.datasets[[1]], "dataset"),
               dimensions = list(
                   Dimension(name = "geographicAreaM49",
                             keys = slot(slot(swsContext.datasets[[1]],
                                 "dimensions")$geographicAreaM49, "keys")),
                   Dimension(name = "measuredElement",
                             keys = slot(slot(swsContext.datasets[[1]],
                                 "dimensions")$measuredElement, "keys")),
                   Dimension(name = "measuredItemCPC",
                             keys = keyTable[, measuredItemCPC]),
                   Dimension(name = "timePointYears",
                             keys = slot(slot(swsContext.datasets[[1]],
                                 "dimensions")$timePointYears, "keys"))
                   )
               )
       },
       "geographicAreaM49" = {
           newKey = DatasetKey(
               domain = slot(swsContext.datasets[[1]], "domain"),
               dataset = slot(swsContext.datasets[[1]], "dataset"),
               dimensions = list(
                   Dimension(name = "geographicAreaM49",
                             keys = keyTable[, geographicAreaM49]),
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
       }
       )



## Query the data
query = GetData(
    key = newKey,
    flags = FALSE,
    normalized = TRUE
)

## Merge the key and the data
keyedQuery =
    merge(query, keyTable, by = aggregationType, all.x = TRUE,
          allow.cartesian = TRUE)
setkeyv(x = keyedQuery, cols = aggregationType)

## Aggregate the data
switch(aggregationType,
       "geographicAreaM49" = {
           aggregateIndex =
               c("parent", "measuredElement", "measuredItemCPC",
                 "timePointYears")
       },
       "measuredItemCPC" = {
           aggregateIndex =
               c("geographicAreaM49", "measuredElement", "parent",
                 "timePointYears")
       }
       )

## Compute the aggregation
aggregatedQuery =
    keyedQuery[, list(Value = sumWithNA(Value)),
               by = aggregateIndex]
setnames(aggregatedQuery, "parent", aggregationType)

## Save the data back
SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
         dataset = slot(swsContext.datasets[[1]], "dataset"),
         data = aggregatedQuery, normalized = TRUE)
