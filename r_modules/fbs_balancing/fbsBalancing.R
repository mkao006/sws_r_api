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


getStandardizedItem =
    GetCodeList(domain = "fbs",
                dataset = "fbs_prebalanced",
                dimension = "measuredItemCPC")

preBalanceTable = read.csv("contigency_table_example.csv")
setnames(preBalanceTable,
         old = c("Value_measuredElementCalorie_5510",
             "Value_measuredElementCalorie_5600", 
             "Value_measuredElementCalorie_5900",
             "Value_measuredElementCalorie_5525", 
             "Value_measuredElementCalorie_5120", 
             "Value_measuredElementCalorie_5150", 
             "Value_measuredElementCalorie_FoodTotal", 
             "Value_measuredElementCalorie_5520", 
             "Value_measuredElementCalorie_residual"),
         new = c("Production", "Imports", "Exports", "Seed", "Loss", "Industrial",
             "Food", "Feed", "Stock"))


getTradeStandardDeviationCaput = function(){
    tradeStdCaputKey = DatasetKey(
        domain = "trade",
        dataset = "stddev_caloriescap",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "measuredItemSuaFbs",
                      keys = standardizedItem),
            Dimension(name = "measuredElementTrade",
                      keys = c("SD5600", "SD5900")),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    tradeStdCaputPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemSuaFbs", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),        
        Pivoting(code = "measuredElementTrade", ascending = TRUE)
    )

    ## Query the data
    tradeStdCaputQuery = GetData(
        key = tradeStdCaputKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = tradeStdCaputPivot
    )

    setkeyv(tradeStdCaputQuery, cols = c("geographicAreaM49", "timePointYears"))
    ## Convert time to numeric
    tradeStdCaputQuery[, timePointYears := as.numeric(timePointYears)]
}

getFeedRequirementData = function(){
    feedRequirementKey = DatasetKey(
        domain = "feed",
        dataset = "total_feed",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "nutrientType",
                      keys = "1"),
            Dimension(name = "estimator",
                      keys = as.character(2:3)),
            Dimension(name = "feedBaseUnit",
                      keys = "2"),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )
    
    ## Pivot to vectorize yield computation
    feedRequirementPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "nutrientType", ascending = TRUE),
        Pivoting(code = "feedBaseUnit", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),
        Pivoting(code = "estimator", ascending = FALSE)
    )

    ## Query the data
    feedRequirementQuery = GetData(
        key = feedRequirementKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = feedRequirementPivot
    )

    setkeyv(feedRequirementQuery, cols = c("geographicAreaM49", "timePointYears"))
    ## Convert time to numeric
    feedRequirementQuery[, timePointYears := as.numeric(timePointYears)]
    feedRequirementQuery[, list(geographicAreaM49, timePointYears,
                                EDemand_lb = Value_estimator_3,
                                EDemand_ub = Value_estimator_2)]
}



getStructuralZeroParam = function(){
    read.csv(file = "structuralZeroParameterCPC.csv")
}

    
## Function to get the compiled contingency table
getContingencyTable = function(){
    ## TODO (Michael): Need to wait for the compiler of other elements
    ##                 to finish and also get Nick to set up the
    ##                 table.
    readFBS(file = "adjustedCommodityContigencyTable.csv",
            file0 = "structuralZeroParameter.csv",
            filef = "adjustedFeedRange.csv")
}


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


