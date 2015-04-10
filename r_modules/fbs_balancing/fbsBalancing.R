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





## Function to selecte the best table from the sampling
sampleBalancedTable = function(contingencyTableList, selectedCountry,
    selectedYear, sanityCheck = FALSE, maxErr = 10, ...){
    
    balancingFunction =
        balanceFBS(FBS = contingencyTableList,
                   sanityCheck = sanityCheck, maxErr = maxErr)

    sampledTables =
        balancingFunction(Country = selectedCountry, year = selectedYear, ...)

    if(is.null(sampledTables))
        sampledTables =
            new("conTa",
                bestTab =
                    contingencyTableList[[selectedCountry]][[selectedYear]]$data)
    sampledTables
}

plotItemSamplingDistribution = function(balancingObject,
    selectedItem = "wheat.and.products"){
    allSampledTables = balancingObject@tables

    itemIndex = which(rownames(balancingObject@bestTab) == selectedItem)

    samplingDistribution =
        do.call("rbind",
                lapply(allSampledTables, FUN = function(x) x[itemIndex, ]))

    

    samplingDistribution.df = melt(data.frame(samplingDistribution))
    ## ggplot(data = samplingDistribution.df, aes(x = value)) +
    ##     ## geom_histogram(aes(y = ..density..)) +
    ##     ## geom_density() + 
    ##     geom_histogram(binwidth = 1) +
    ##     facet_wrap(~variable)
    
    samplingRange = range(samplingDistribution, na.rm = TRUE)    
    numberOfElements = NCOL(samplingDistribution)
    opar = par()
    par(mfrow = c(3, ceiling(numberOfElements/3)))
    for(i in 1:numberOfElements){
        hist(samplingDistribution[, i], breaks = length(allSampledTables)/10,
             xlim = samplingRange,
             main = colnames(samplingDistribution)[i], xlab = "", ylab = "")
        abline(v = balancingObject@bestTab[itemIndex, i], col = "red", lty = 2)
    }
    par(opar)
}




balancing =
    try({
            meanTable <<- getPreBalancingTable()
            stdTable <<- getTradeStandardDeviationCaput()
            feedRequirement <<- getFeedRequirementData()
            structuralZeroParam <<- getStructuralZeroParam()
            

            finalTable =
                mergeAllTables(meanTable = meanTable,
                               stdTable = stdTable,
                               feedRequirementTable = feedRequirement,
                               keys = c("geographicAreaM49", "measuredItemSuaFbs",
                                   "timePointYears"))
            finalTable[, Imports := Imports * -1]
            finalInputList = convertToInputList(finalTable)

            ## balancingFunction =
            ##     balanceFBS(FBS = finalInputList,
            ##                sanityCheck = FALSE,
            ##                maxErr = 10)

            ## optimalTable =
            ##     balancingFunction(selectedCountry, selectedYear, nIter = 5)
            
            
            optimalTable =
                sampleBalancedTable(contingencyTableList = finalInputList,
                                    selectedCountry = selectedCountry,
                                    selectedYear = selectedYear,
                                    nIter = 5)

            saveOptimalBalancedTable(optimalTable,
                                     selectedCountry = selectedCountry,
                                     selectedYear = selectedYear)
            
        })


## plotItemSamplingDistribution(balancingObject = balancingObject,
##                              selectedItem = "cassava.and.products")




## ## Old testing
## oldFormat =
##      readFBS(file = "adjustedCommodityContigencyTable.csv",
##              file0 = "structuralZeroParameter.csv",
##              filef = "adjustedFeedRange.csv")

## oldBalancingFunction =
##     balanceFBS(FBS = oldFormat,
##                sanityCheck = FALSE, maxErr = 10)

## oldTables =
##     oldBalancingFunction(Country = "231", year = "2010", nIter = 5)
