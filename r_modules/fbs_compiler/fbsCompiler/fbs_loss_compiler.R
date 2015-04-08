## Get loss utilization data
getLossData = function(){
    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    lossKey = DatasetKey(
        domain = "lossWaste",
        dataset = "loss",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "measuredElementSuaFbs",
                      keys = "5120"),
            Dimension(name = "measuredItemSuaFbs",
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    lossPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = "measuredElementSuaFbs", ascending = TRUE)
    )

    ## Query the data
    lossQuery = GetData(
        key = lossKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = lossPivot
    )

    setnames(lossQuery, old = "measuredItemSuaFbs", new = "measuredItemCPC")
    setnames(lossQuery,
             old = grep("measuredElementSuaFbs", colnames(lossQuery), value = TRUE),
             new = gsub("measuredElementSuaFbs", "measuredElement",
                 grep("measuredElementSuaFbs", colnames(lossQuery), value = TRUE)))
    

    ## Convert time to numeric
    lossQuery[, timePointYears := as.numeric(timePointYears)]
    lossQuery
}


## Pseudo codes:

## Assuming the loss rates are calculated, maybe we should take the
## loss directly.

## (1) Obtain the loss rate data, then compute the country/group
## average loss rate.

## (2) Use the countr/group average loss rate to impute commodities
## which are missing.

## (3) Mean aggregate the values to obtain the commodity level loss rate.

## (4) Multiply the loss rate to production and nutrient requirements
## to obtain the the nutrients of loss.



## The loss data should have been performed by individual module as
## well. Just need to multiplied by the nutrient and aggregate.


