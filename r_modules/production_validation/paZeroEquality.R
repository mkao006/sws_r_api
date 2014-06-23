## load the library
require("faosws")

## Set up for the test environment
if(Sys.getenv("USERNAME") == "kao"){
    GetTestEnvironment(baseUrl = "https://hqlqasws1.hq.un.fao.org:8181/sws",
                       token = "6d98803b-e623-43dc-b137-4130f0dc140c")
}

## Pivot to vectorize yield computation
newPivot = c(
    Pivoting(code = "geographicAreaM49", ascending = TRUE),
    Pivoting(code = "measuredItemCPC", ascending = TRUE),
    Pivoting(code = "timePointYears", ascending = FALSE),
    Pivoting(code = "measuredElement", ascending = TRUE)
    )

## Query the data
query = GetData(
    key = swsContext.datasets[[1]],
    flags = TRUE,
    normalized = FALSE,
    pivoting = newPivot
)


## Check zero equality function
paZeroEquality = function(production, areaHarvested){
    tmp = rep(0, length(production))
    productionZero = which(production == 0)
    areaHarvestedZero = which(areaHarvested == 0)
    tmp[union(setdiff(areaHarvestedZero, productionZero),
              setdiff(productionZero, areaHarvestedZero))] = 1
    tmp
}

##
query[, zeroInequality := paZeroEquality(Value_measuredElement_5510,
                Value_measuredElement_5312)]

## Print result
print(table(query$zeroInequality))

