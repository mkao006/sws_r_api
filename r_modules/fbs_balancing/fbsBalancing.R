suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(conSTable)
    library(reshape2)
})

## NOTE (Michael): The selected country code is in old FAO
##                 classification, need to change to M49 codes when
##                 the contingency table is set up.
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


preBalanceTable = getPreBalancingTable()




feedRequirement = getFeedRequirementData()

structuralZeroParam = getStructuralZeroParam()
    

oldFormat =
    readFBS(file = "adjustedCommodityContigencyTable.csv",
            file0 = "structuralZeroParameter.csv",
            filef = "adjustedFeedRange.csv")
calStd = getTradeStandardDeviationCaput()

finalTable = merge(preBalanceTable, calStd, by = c("geographicAreaM49", "timePointYears", "measuredItemSuaFbs"), all.x = TRUE)
finalTable[is.na(Value_measuredElementTrade_SD5600),
           Value_measuredElementTrade_SD5600 := 0]
finalTable[is.na(Value_measuredElementTrade_SD5900),
           Value_measuredElementTrade_SD5900 := 0]


newData =
    finalTable[, grep("Value", colnames(preBalanceTable), value = TRUE),
                    with = FALSE]
setnames(newData,
         old = c("Value_measuredElement_250",
             "Value_measuredElement_251", 
             "Value_measuredElement_252",
             "Value_measuredElement_55252", 
             "Value_measuredElement_51202", 
             "Value_measuredElement_51502", 
             "Value_measuredElement_51422", 
             "Value_measuredElement_55202", 
             "Value_measuredElement_50712"),
         new = c("Production", "Imports", "Exports", "Seed", "Losses",
             "Industrial", "Food", "Feed", "Stock"))
newData = data.matrix(newData[, Production := NULL])
newData[, 1] = newData[, 1] * -1
attr(newData, "dimnames")[[1]] = preBalanceTable$measuredItemSuaFbs

## This is to prevent numerical error
newDataRowTotal = round(rowSums(newData), 5)



tradeSd =
    data.matrix(finalTable[, list(Value_measuredElementTrade_SD5900,
                                  Value_measuredElementTrade_SD5900)])
attr(tradeSd, "dimnames")[[1]] = preBalanceTable$measuredItemSuaFbs

feed = feedRequirement[, list(EDemand_lb, EDemand_ub)]



newList = list(data = newData, row_Tot = newDataRowTotal,
    sd = tradeSd, feed = feed)

newFormat = list(`840` = list(`2010` = newList))
f = balanceFBS(FBS = newFormat, sanityCheck = FALSE, maxErr = 10)

test = f(Country = "840", year = "2010", nIter = 5)







## Function to selecte the best table from the sampling
sampleBalancedTable = function(contingencyTable, sanityCheck = FALSE, maxErr = 10,
    selectedCountry, selectedYear, ...){
    
    balancingFunction =
        balanceFBS(FBS = contingencyTable,
                   sanityCheck = sanityCheck, maxErr = maxErr)
    sampledTables =
        balancingFunction(Country = selectedCountry, year = selectedYear, ...)
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


## Carry out the balancing
bestBalancedTable =
    getContingencyTable() %>%
    ## test %>%
    sampleBalancedTable(contingencyTable = .,
                        selectedCountry = selectedCountry,
                        selectedYear = selectedYear,
                        oset = c(30, 30, 30, 30, 30, 10000),
                        prop = NULL,
                        nIter = 500,
                        verbose = TRUE,
                        checks = "none",
                        feedShift = 30) %>%
    {
        bestTable <<- .@bestTab
        balancingObject <<- .
    }

plotItemSamplingDistribution(balancingObject = balancingObject,
                             selectedItem = "cassava.and.products")


