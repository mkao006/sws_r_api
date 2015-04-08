## Get seed utilization data
getSeedData = function(){

    ## NOTE (Michael): Need to select all the items, waiting for
    ##                 response from Nick.
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = elementVar,
                      keys = "5525"),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    seedPivot = c(
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
    seedQuery = GetData(
        key = seedKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = seedPivot
    )


    ## Convert time to numeric
    seedQuery[, timePointYears := as.numeric(timePointYears)]
    seedQuery
}

