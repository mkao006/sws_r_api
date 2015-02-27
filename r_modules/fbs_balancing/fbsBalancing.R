suppressMessages({
    library(faosws)
    library(faoswsUtil)
    library(data.table)
    library(magrittr)
    library(conSTable)
})

## NOTE (Michael): The selected country code is in old FAO
##                 classification, need to change to M49 codes when
##                 the contingency table is set up.
selectedCountry = "231"
selectedYear = "2010"

## Set up testing environments
if(Sys.getenv("USER") == "mk"){
    GetTestEnvironment(
        baseUrl = "https://hqlprswsas1.hq.un.fao.org:8181/sws",
        token = "d91934e8-6bf9-4f44-b051-504a18c885fc"
        )
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

## Function to selecte the best table from the sampling
sampleBestTable = function(contingencyTable, sanityCheck = FALSE, maxErr = 10,
    selectedCountry, selectedYear, ...){
    
    balancingFunction =
        balanceFBS(FBS = contingencyTable,
                   sanityCheck = sanityCheck, maxErr = maxErr)
    sampledTables =
        balancingFunction(Country = selectedCountry, year = selectedYear, ...)
    sampledTables@bestTab
}

## Carry out the balancing
bestBalancedTable =
    getContingencyTable() %>%
    sampleBestTable(contingencyTable = .,
                    selectedCountry = selectedCountry,
                    selectedYear = selectedYear,
                    oset = c(30, 30, 30, 30, 30, 10000),
                    prop = NULL,
                    nIter = 1000,
                    verbose = TRUE,
                    checks = "none",
                    feedShift = 30)
