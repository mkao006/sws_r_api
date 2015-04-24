suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(conSTable)
    library(reshape2)
})

selectedCountry = "840"
selectedYear = "2010"

## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
        )
    files = dir(path = "fbsBalancing/", full.names = TRUE)
    lapply(files, source)
}


standardizedItem =
    GetCodeList(domain = "suafbs",
                dataset = "fbs_prebalance",
                dimension = "measuredItemSuaFbs")[grep("^S", code)]

balancing =
    try({
            meanTable <<- getPreBalancingTable()
            stdTable <<- getTradeStandardDeviationCaput()
            feedRequirement <<- getFeedRequirementData()
            structuralZeroParam <<- getStructuralZeroParam()
            
            ## TODO (Michael): Need to apply the structural zero param
            ##                 to the final table.
            finalTable =
                mergeAllTables(meanTable = meanTable,
                               stdTable = stdTable,
                               feedRequirementTable = feedRequirement,
                               keys = c("geographicAreaM49", "measuredItemSuaFbs",
                                   "timePointYears"))
            finalTable[, Imports := Imports * -1]
            finalInputList =
                convertToInputList(data = finalTable,
                                   meanVariable = c("Imports", "Exports", "Feed",
                                       "Seed", "Losses", "Industrial", "Food",
                                       "Stock"))
            
            
            optimalTable =
                sampleBalancedTable(contingencyTableList = finalInputList,
                                    selectedCountry = selectedCountry,
                                    selectedYear = selectedYear,
                                    nIter = 10000,
                                    check = "Stock",
                                    stockShift = 20,
                                    verbose = FALSE)

            saveOptimalBalancedTable(optimalTable,
                                     selectedCountry = selectedCountry,
                                     selectedYear = selectedYear)
        })


NPMLE = npmle(optimalTable)
optimal = which.max(NPMLE)

pdf(file = "balancingCheck.pdf", width = 10, height = 10)
for(i in rownames(finalInputList[[selectedCountry]][[selectedYear]]$data)){
plotItemSamplingDistribution(balancingObject = optimalTable,
                             selectedItem = i,
                             optimalTable = optimal,
                             inputTable = finalInputList[[selectedCountry]][[selectedYear]])
}
graphics.off()


## plotItemSamplingDistribution(balancingObject = optimalTable,
##                              selectedItem = "S2513",
##                              inputTable = finalInputList[[selectedCountry]][[selectedYear]])



## Old testing
## oldFormat =
##      readFBS(file = "adjustedCommodityContigencyTable.csv",
##              file0 = "structuralZeroParameter.csv",
##              filef = "adjustedFeedRange.csv")

## oldBalancingFunction =
##     balanceFBS(FBS = oldFormat,
##                sanityCheck = FALSE, maxErr = 10)

## oldTables =
##     oldBalancingFunction(Country = "231", year = "2010", nIter = 5)


## Tests
## jointDist = do.call("rbind", lapply(optimalTable@tables, FUN = function(x) x[30, ]))
## plot(data.frame(jointDist))
