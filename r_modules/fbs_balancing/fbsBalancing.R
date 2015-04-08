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
}


standardizedItem =
    GetCodeList(domain = "suafbs",
                dataset = "fbs_prebalance",
                dimension = "measuredItemSuaFbs")[grep("^S", code)]


preBalanceTable = getPreBalancingTable()
setnames(preBalanceTable,
         old = c("Value_measuredElement_250",
             "Value_measuredElement_251", 
             "Value_measuredElement_252",
             "Value_measuredElement_55252", 
             "Value_measuredElement_51202", 
             "Value_measuredElement_51502", 
             "Value_measuredElement_51422", 
             "Value_measuredElement_55202", 
             "Value_measuredElement_50712"),
         new = c("Production", "Imports", "Exports", "Seed", "Loss",
             "IndustrialUse", "Food", "Feed", "Stock"))




feedRequirement = getFeedRequirementData()


getStructuralZeroParam = function(){
    GetTableData(schemaName = "ess", tableName = "fbs_structural_zero")
}

    

oldFormat =
    readFBS(file = "adjustedCommodityContigencyTable.csv",
            file0 = "structuralZeroParameter.csv",
            filef = "adjustedFeedRange.csv")


test = getContingencyTable()


dataset = list(`840` = list(`2010` = test[[1]][[1]]))


f = balanceFBS(FBS = dataset, sanityCheck = FALSE, maxErr = 10)

f(Country = "840", year = "2010")

getCaloricTradeStandardDeviation = function(){}


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


