getFeedAvailabilityData = function(){
    ## HACK (Michael): We need to get this from the feed availability
    ## table
    tmp = data.table(read.csv(file = "feedAvailability.csv",
        colClass = rep("character", 6)))
    tmp[, `:=`(c("Value_measuredElement_feedAvail", "timePointYears"),
               list(as.numeric(Value_measuredElement_feedAvail),
                    as.numeric(timePointYears)))]
    tmp    
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


gj2kcal = function(x){
    x * 238845.8966275
}



disaggregateFeedRequirement = function(data, areaVar, yearVar,
    feedAvailabilityVar, feedRequirementPointVar, feedUtilizationVar){

    dataCopy = copy(data)
    dataCopy[, disaggregationWeights :=
                 computeRatio(.SD[[feedAvailabilityVar]],
                              sum(.SD[[feedAvailabilityVar]],
                         na.rm = TRUE)),
             by = c(areaVar, yearVar)]
    
    dataCopy[, `:=`(c(feedUtilizationVar),
                    list(dataCopy[[feedRequirementPointVar]] * disaggregationWeights))]
    dataCopy[, disaggregationWeights := NULL]
    dataCopy
}




## Pseudo codes:

## Get feed availability data

## Get nutrient data

## Compute Calorie standardization

## Aggregate feed availability

## Disaggregate feed requirement according to the calorie
## availability. If total Availability is greater than total
## requirement, then take upper bound, if less then take lower
## bound. Otherwise no change.

