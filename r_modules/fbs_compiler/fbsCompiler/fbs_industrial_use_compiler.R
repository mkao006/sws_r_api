## Get industrial use data
getIndustrialUseData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for response
    ##                 from Nick on the item table.
    industrialUseKey = DatasetKey(
        domain = "industrialUse",
        dataset = "industrialuse",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = elementVar,
                      keys = "5150"),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    industrialUsePivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    ##
    ## NOTE (Michael): Need to check this, 570 items are queried, but only
    ##                 105 are returned. Also, there are no value for
    ##                 element 5518 which caused the type to be logical.
    industrialUseQuery = GetData(
        key = industrialUseKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = industrialUsePivot
    )


    ## Convert time to numeric
    industrialUseQuery[, timePointYears := as.numeric(timePointYears)]
    industrialUseQuery
}
        


