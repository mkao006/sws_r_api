##' Function to extract the production data
##'
##' @return A data.table containing the production data.
##' 

getProductionData = function(){
    productionKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = elementVar,
                      keys = unique(productionElements$element_51)),
            Dimension(name = itemVar,
                      keys = primaryMeasuredItemCPC),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    productionPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = itemVar, ascending = TRUE),
        Pivoting(code = yearVar, ascending = FALSE),
        Pivoting(code = elementVar, ascending = TRUE)
    )

    ## Query the data
    productionQuery = GetData(
        key = productionKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = productionPivot
    )


    ## Convert time to numeric
    productionQuery[, timePointYears := as.numeric(timePointYears)]
    productionQuery
}
