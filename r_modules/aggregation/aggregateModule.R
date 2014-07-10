########################################################################
## Title: Module for aggregation
## Date: 2014-06-25
########################################################################

## load the library
require("faosws")
library(faoswsExtra)
library(data.table)

## Set up for the test environment
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
        token = "2a4d62c3-e776-4854-a013-0a4cdb5d7541"
        )
}


## load parameter, type can be area, item. These are examples.
## aggregationType = "geographicAreaM49"
## aggregationCode = "1061"
## aggregationType = "measuredItemCPC"
## aggregationCode = "011"
aggregationType = swsContext.computationParams$aggregationType
aggregationCode = swsContext.computationParams$aggregationCode


## CHECK (Michael): There are duplicate key in geographic key tree.
keyTree =
    unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
                       dataset = swsContext.datasets[[1]]@dataset,
                       dimension = aggregationType,
                       roots = aggregationCode)
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
