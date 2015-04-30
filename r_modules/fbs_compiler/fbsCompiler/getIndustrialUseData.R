##' The function extracts the data on industrial utilization.
##'
##' Currently, the industrial utilization only contains data on
##' bio-fuel from AGLINK-COSIMO.
##'
##' @return A data.table containing the data on industrial utilization.
##' 

getIndustrialUseData = function(){
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
