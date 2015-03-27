## Get trade data
getTradeData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    tradeKey = DatasetKey(
        domain = "trade",
        dataset = "total_trade_CPC",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementTrade",
                      keys = c("5600", "5900")),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeQuery = GetData(
        key = tradeKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradePivot
    )


    ## Convert time to numeric
    tradeQuery[, timePointYears := as.numeric(timePointYears)]
    tradeQuery
}


getTradeStandardDeviation = function(){


    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    tradeSDKey = DatasetKey(
        domain = "trade",
        dataset = "stddev_quantity",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementTrade",
                      keys = c("SD5600", "SD5900")),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradeSDPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeSDQuery = GetData(
        key = tradeSDKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradeSDPivot
    )


    ## Convert time to numeric
    tradeSDQuery[, timePointYears := as.numeric(timePointYears)]
    tradeSDQuery
}



standardizeTradeStd = function(data, commodityTree, weightVariable,
    tradeStandardDeviationVariable, standardizationKey){
    commodityTreeCopy = copy(commodityTree)
    setnames(commodityTreeCopy, old = "cpc_children_code", new = itemVar)

    dataTree = merge(data, commodityTreeCopy, by = itemVar, all.x = TRUE)
    standardized =
        dataTree[, lapply(tradeStandardDeviationVariable,
                          FUN = function(x) {
                              sqrt(sum(.SD[[weightVariable]]^2 * .SD[[x]]^2))
                          }),
                 by = c(standardizationKey)]

    setnames(standardized,
             old = paste0("V", 1:length(tradeStandardDeviationVariable)),
             new = tradeStandardDeviationVariable)
    standardized
}


    

## NOTE (Michael): In order to build the trade profile, we need
##                 to know how to calculate the "trade". Do we
##                 include processed?  If no, then need the set of
##                 primary commodities, and meat for livestock; if
##                 yes, then we should only mapped to all the
##                 commodities which are within the standardization
##                 path.




## Pseudo codes:
##

## For primary:
## ---------------------------------------------------------------------
## (1) Obtain energy, protein, lipid, CHOLAVLDF and Sugar from meHS
## and sdHS. What is the difference between the two dataset. Also,
## what is the difference between the nutrients data used for trade
## and production. 

## (2) Obtain the edible, extraction rate and commodity from file from
## HSm.

## (3) Merge the consolidated trade data with the nutrient from step (1) and (2).

## (4) Calculate the energy, protein, fat, and CHOAVLDF for the trade.

## (5) Calculate the residual for feed, it is defined the same way as
## production. trade multiplied by (1 - extraction rate) then the
## nutrient conversion ratio. Probably should move this to domestic
## supply.

## (6) Aggregate the trade (both import and export), and also the
## columns calculated in step (5) and (6) by area, year, group and
## commodity. Again, need to understand the difference between the use
## of group and commodity.

## Domestic supply:
## (7) Add production and trade energy to obtain the domestic energy supply.

## For Processed:
## ---------------------------------------------------------------------

## (1) Load the D2D3s data set, what is the source of this data?





## Run production compiler to obtain the energy supply of each
## commodity group.


## Get all the HS code using GetCodeTree, then merge with Adam's
## file. Any HS code not mapped will go into HS to commodity group.

## Merge the energy supply by commodity with Adam's mapping, then
## determine the relative ratio. This ratio does not imply that the
## quantity factually came from these group and require sufficient
## availability, rather they suggests how the processed product was
## constructed.

## However, it is possible that the standardized export can be greater
## than the domestic supply. This needs to be checked.

## Build the relative production profile ratio for each parent
## commodity group for each country and year.

## Merge the import of the mirrored trade flow data and the exporter
## production profile, then standardize to obtain the standardized
## import.

## Merge the export of the mirrored trade flow data and the domestic
## production profile, then standardize to obtain the standardized
## export.


## All I need at the end is the trade in commodity group in per capita
