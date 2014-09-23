##' Function to compute the weights of each flag based on the history
##'
##' @param history The history of data queried from the api
##' @param officialFlag The character string representing official
##' figure.
##' @param method The method to be used to calculate the weights.
##'
##' @export
##' 

computeFlagWeight = function(history, officialFlag = "",
    method = c("entropy", "similarity")){

    ## Remove flags for missing values.
    removeMhistory = history[flagObservationStatus != "M", ]

    ## Calculate the number of history for each entry, only entries
    ## with more than two history can be used for the calculation of
    ## weights.
    removeMhistory[, numberOfHistory :=
                   length(unique(flagObservationStatus)),
                   by = c("geographicAreaM49", "measuredElement",
                       "measuredItemCPC", "timePointYears")]

    ## Average the history if there is more than one symble for each
    ## history. The MSE would be the same, but the entropy will
    ## decrease. This is a temporary solution.
    removeMhistory[, Value := mean(Value),
                   by = c("geographicAreaM49", "measuredElement",
                       "measuredItemCPC", "timePointYears",
                       "flagObservationStatus")]

    ## Subset the data which has more than history.
    finalHistory.dt =
        unique(removeMhistory[numberOfHistory >= 2,
                                list(geographicAreaM49, 
                                     measuredElement, measuredItemCPC,
                                     timePointYears, Value,
                                     flagObservationStatus)])

    ## Change the name of the history
    finalHistory.dt[, flagObservationStatus :=
                    paste0("Flag_", flagObservationStatus)]

    castedHistory =
        data.table(
            dcast(finalHistory.dt[, list(geographicAreaM49,
                                         measuredElement,
                                         measuredItemCPC,
                                         timePointYears, Value,
                                         flagObservationStatus)],
                  geographicAreaM49 + measuredElement + measuredItemCPC +
                  timePointYears ~ flagObservationStatus,
                  value.var = "Value")
            )
    ## castedHistory[, Flag_I := mean(Flag_, na.rm = TRUE),
    ##                    by = c("geographicAreaM49", "measuredElement",
    ##                        "measuredItemCPC")]
    cat("Number of entries for history: ", NROW(castedHistory), "\n")

    if(method == "entropy"){
        symbNames =
            colnames(castedHistory)[grepl("Flag_",
                                          colnames(castedHistory))]
        condition = paste0("!is.na(castedHistory$",
            symbNames, ")", collapse = " & ")
        commodityNoMiss.dt =
            castedHistory[eval(parse(text = condition)), ]
        official = paste0("Flag_", officialFlag)
        
        
        finalWeights =
            apply(data.matrix(
                commodityNoMiss.dt[, symbNames[symbNames != official],
                                   with = FALSE]),
                  2,
                  FUN = computeEntropyWeights, benchmark =
                  unlist(commodityNoMiss.dt[, official, with = FALSE])
                  )
        finalWeights = sort(finalWeights, decreasing = TRUE)
                         
        weightTable =
            data.frame(flagObservationStatus =
                       c(officialFlag,
                         gsub("Flag_", "", names(finalWeights)), "M"),
                       flagObservationWeights =
                           c(1, finalWeights, 0),
                       row.names = NULL)

    } ## else if(method == "similarity"){
    ##     symbNames =
    ##         colnames(castedHistory)[grepl("Flag_",
    ##                                       colnames(castedHistory))]

    ##     imputedHistory =
    ##         amelia(castedHistory, m = 100, ts = "timePointYears",
    ##                cs = "geographicAreaM49",
    ##                logs = symbNames,
    ##                idvars = c("measuredElement", "measuredItemCPC"),
    ##                p2s = 0)
   
    ##     computeCentroidWeights = function(x){
    ##         similarity = 1/rowSums(as.matrix(dist(t(x))))
    ##         weights = similarity/sum(similarity)
    ##         weights
    ##     }
        
    ##     finalWeights =
    ##         rowMeans(sapply(imputedHistory$imputation,
    ##                         FUN = function(x)
    ##                             computeCentroidWeights(
    ##                                 data.matrix(x[, symbNames,
    ##                                               with = FALSE])))
    ##                  )
        
    ##     weightTable =
    ##         data.frame(flagObservationStatus =
    ##                    c(gsub("Flag_", "",
    ##                           names(sort(finalWeights,
    ##                                      decreasing = TRUE)
    ##                                 )
    ##                           ), "M"
    ##                      ),
    ##                    flagObservationWeights =
    ##                        c(sort(finalWeights, decreasing = TRUE), 0),
    ##                    row.names = NULL)
        
    ## }                 
    weightTable
}
