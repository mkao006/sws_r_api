########################################################################
## Title: Module for aggregation
## Date: 2015-06-09
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
        token = "301a4e40-5813-4c6c-90c3-b49cdebffa30"
    )
}

## Aggregation of flags may eventually require more elegant processing.  But,
## for the short term, we'll just assign flags of "I" (imputed) and "s"
## (calculated as a sum).
aggObsFlag = function(flagObservationStatus) "I"
aggMetFlag = function(flagMethod) "s"

## CHECK (Michael): There are duplicate key in geographic key tree.
geoKeyTree =
    unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
                       dataset = swsContext.datasets[[1]]@dataset,
                       dimension = "geographicAreaM49")
           )

## Aggregate geo
keyTable = adjacent2edge(geoKeyTree)
setnames(x = keyTable, old = "children", new = "geographicAreaM49")

## Filter keyTable to only have parents of current keys
keyTable[, keep := FALSE]
keyTable[geographicAreaM49 %in%
             swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys,
         keep := TRUE]
newSum = keyTable[, sum(keep)]
oldSum = 0
while(newSum > oldSum){
    ## Keep parents of keys which are being kept
    keptKeys = keyTable[(keep), unique(parent)]
    keyTable[geographicAreaM49 %in% keptKeys, keep := TRUE]
    oldSum = newSum
    newSum = keyTable[, sum(keep)]
}
keyTable = keyTable[(keep), ]
keyTable[, keep := NULL]

## Set up the new key
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

## Query the data
query = GetData(key = newKey, flags = TRUE, normalized = TRUE)

## Merge the key and the data
keyedQuery =
    merge(query, keyTable, by = "geographicAreaM49", all.x = TRUE,
          allow.cartesian = TRUE)
setkeyv(x = keyedQuery, cols = "geographicAreaM49")

## Aggregate the data
aggregateIndex = c("parent", "measuredElement", "measuredItemCPC",
                   "timePointYears")

## Compute the aggregation
aggregatedQuery =
    keyedQuery[, list(Value = sum(Value, na.rm = TRUE),
                      flagObservationStatus = aggObsFlag(flagObservationStatus),
                      flagMethod = aggMetFlag(flagMethod)),
               by = aggregateIndex]
setnames(aggregatedQuery, "parent", "geographicAreaM49")

## Save the data back
SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
         dataset = slot(swsContext.datasets[[1]], "dataset"),
         data = aggregatedQuery, normalized = TRUE)



############################### measuredItemCPC ###############################

cpcKeyTree =
    unique(GetCodeTree(domain = swsContext.datasets[[1]]@domain,
                       dataset = swsContext.datasets[[1]]@dataset,
                       dimension = "measuredItemCPC")
           )

## Aggregate cpc
keyTable = adjacent2edge(cpcKeyTree)
setnames(x = keyTable, old = "children", new = "measuredItemCPC")

## Filter keyTable to only have parents of current keys
keyTable[, keep := FALSE]
keyTable[measuredItemCPC %in%
             swsContext.datasets[[1]]@dimensions$measuredItemCPC@keys,
         keep := TRUE]
newSum = keyTable[, sum(keep)]
oldSum = 0
while(newSum > oldSum){
    ## Keep parents of keys which are being kept
    keptKeys = keyTable[(keep), unique(parent)]
    keyTable[measuredItemCPC %in% keptKeys, keep := TRUE]
    oldSum = newSum
    newSum = keyTable[, sum(keep)]
}
keyTable = keyTable[(keep), ]
keyTable[, keep := NULL]

## This is a hack to collapse the graph
# keyTable[, parent := substr(parent, 1, 3)]

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

## Query the data
query = GetData(key = newKey, flags = TRUE, normalized = TRUE)

## Merge the key and the data
keyedQuery =
    merge(query, keyTable, by = "measuredItemCPC", all.x = TRUE,
          allow.cartesian = TRUE)
setkeyv(x = keyedQuery, cols = "measuredItemCPC")

## Aggregate the data
aggregateIndex = c("geographicAreaM49", "measuredElement", "parent",
                   "timePointYears")

## Compute the aggregation
aggregatedQuery =
    keyedQuery[, list(Value = sum(Value, na.rm = TRUE),
                      flagObservationStatus = aggObsFlag(flagObservationStatus),
                      flagMethod = aggMetFlag(flagMethod)),
               by = aggregateIndex]
setnames(aggregatedQuery, "parent", "measuredItemCPC")

## Save the data back
SaveData(domain = slot(swsContext.datasets[[1]], "domain"),
         dataset = slot(swsContext.datasets[[1]], "dataset"),
         data = aggregatedQuery, normalized = TRUE)
