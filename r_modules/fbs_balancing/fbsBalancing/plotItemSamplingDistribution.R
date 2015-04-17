plotItemSamplingDistribution = function(balancingObject,
    selectedItem = "S2511"){
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
