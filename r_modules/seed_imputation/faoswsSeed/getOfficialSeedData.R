##' Get Official Seed Data
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

getOfficialSeedData = function(){
    seedKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = getAllCountries()),
            Dimension(name = elementVar,
                      keys = seedElementCode),
            Dimension(name = itemVar,
                      keys = getAllItemCPC()),
            Dimension(name = yearVar,
                      keys = getAllYears())
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
    seedQuery = GetData(
        key = seedKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = seedPivot
    )

    ## Convert time to numeric
    seedQuery[, timePointYears := as.numeric(timePointYears)]
    seedQuery[seedQuery[[paste0(flagObsPrefix, seedElementCode)]] == "", ]
}