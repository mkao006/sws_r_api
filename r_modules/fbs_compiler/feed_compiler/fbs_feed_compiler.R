## getFeedAvailabilityData = function(){
##     ## HACK (Michael): We need to get this from the feed availability
##     ## table
##     tmp = data.table(read.csv(file = "feedAvailability.csv",
##         colClass = rep("character", 6)))
##     tmp[, `:=`(c("Value_measuredElement_feedAvail", "timePointYears"),
##                list(as.numeric(Value_measuredElement_feedAvail),
##                     as.numeric(timePointYears)))]
##     tmp    
## }


gj2kcal = function(x){
    x * 238845.8966275
}

getFeedRequirementData = function(){
    feedRequirementKey = DatasetKey(
        domain = "feed",
        dataset = "total_feed",
        dimensions = list(
            Dimension(name = areaVar,
                      keys = selectedCountry),
            Dimension(name = "nutrientType",
                      keys = "1"),
            Dimension(name = "estimator",
                      keys = "1"),
            Dimension(name = "feedBaseUnit",
                      keys = "1"),
            Dimension(name = yearVar,
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    feedRequirementPivot = c(
        Pivoting(code = areaVar, ascending = TRUE),
        Pivoting(code = "nutrientType", ascending = TRUE),
        Pivoting(code = "feedBaseUnit", ascending = TRUE),
        Pivoting(code = yearVar, ascending = TRUE),
        Pivoting(code = "estimator", ascending = FALSE)
    )

    ## Query the data
    feedRequirementQuery = GetData(
        key = feedRequirementKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = feedRequirementPivot
    )

    ## Convert GJ to Kcal
    valueColumns = grep("Value", colnames(feedRequirementQuery), value = TRUE)
    feedRequirementQuery[, `:=`(c(valueColumns),
                                lapply(valueColumns,
                                       FUN = function(x) gj2kcal(.SD[[x]])))]

    setkeyv(feedRequirementQuery, cols = c("geographicAreaM49", "timePointYears"))
    ## Convert time to numeric
    feedRequirementQuery[, timePointYears := as.numeric(timePointYears)]
    feedRequirementQuery
}


calculateFeedAvailability = function(data, production, import, export, seed, loss,
    industrialUse, food){

    dataCopy = copy(data)
    dataCopy[, `:=`(c("Value_measuredElementCalorie_feedAvail"),
                    rowSums(dataCopy[, c(production, import), ,with = FALSE]) -
                    rowSums(dataCopy[, c(export, seed, loss, industrialUse, food),
                                     with = FALSE]))]
    dataCopy[Value_measuredElementCalorie_feedAvail < 0,
             Value_measuredElementCalorie_feedAvail := 0]
    dataCopy[, feedAvailableWeights :=
                 computeRatio(Value_measuredElementCalorie_feedAvail,
                              sum(Value_measuredElementCalorie_feedAvail)),
             by = c(areaVar, yearVar)]
    dataCopy
}



disaggregateFeedRequirement = function(data, areaVar, yearVar,
    feedAvailabilityWeight, feedRequirementVar, feedUtilizationVar){
    dataCopy = copy(data)
    dataCopy[, `:=`(c(feedUtilizationVar),
                    list(.SD[[feedRequirementVar]] *
                         .SD[[feedAvailabilityWeight]])),
             by = c(areaVar, yearVar)]
    dataCopy[is.na(dataCopy[[feedUtilizationVar]]),
             `:=`(c(feedUtilizationVar), 0)]
    dataCopy
}


