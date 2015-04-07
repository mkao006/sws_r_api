selectBalancedTargetData = function(data){    
    saveSelection =
        data[, c(names(swsContext.datasets[[1]]@dimensions),
                 "reliableValue", "reliabilityFlag"), with = FALSE]
    setnames(saveSelection,
             old = c("reliableValue", "reliabilityFlag"),
             new = c(valuePrefix, flagPrefix))
    saveSelection
}
