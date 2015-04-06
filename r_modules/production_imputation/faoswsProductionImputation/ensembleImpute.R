##' Function to perform ensemble imputation
##'
##' This is an implementation of the ensemble imputation methodology
##' developed for the FAO production domain.
##'
##' @param x A numeric vector
##' @param restrictWeights Whether a maximum weight restriction should
##' be imposed.
##' @param maximumWeights The maximum weight to be imposed, must be
##' between [0.5, 1].
##' @param ensembleModel A list of models to be used to build the
##' ensemble.
##' @param plot Whether the result of the ensemble should be plotted.
##'
##' @export
##' 


ensembleImpute = function(x, restrictWeights = TRUE,
    maximumWeights = 0.7,
    ensembleModel = list(defaultMean = defaultMean,
        defaultLm = defaultLm, defaultExp = defaultExp,
        defaultLogistic = defaultLogistic, defaultLoess = defaultLoess,
        defaultSpline = defaultSpline, defaultArima = defaultArima,
        defaultMars = defaultMars, defaultNaive = defaultNaive),
    plot = FALSE){
    T = length(x)
    n.model = length(ensembleModel)
    ensemble = x
    missIndex = is.na(ensemble)
    if(any(is.na(x))){
        if(length(na.omit(x)) == 0){
            ensemble = rep(NA_real_, length(x))
        } else if(length(unique(na.omit(x))) == 1){
            ensemble = defaultMean(x)
        } else {
            modelFits = computeEnsembleFit(x = x,
                ensembleModel = ensembleModel)
            modelWeights = computeEnsembleWeight(x, modelFits,
                restrictWeights = restrictWeights,
                maximumWeights = maximumWeights)
            ## print(modelWeights)
            ensembleFit = computeEnsemble(modelFits, modelWeights)
            ensemble[missIndex] = ensembleFit[missIndex]
            if(plot){
                if(is.null(names(ensembleModel))){
                    modelNames = paste0("Model ", 1:n.model)
                } else {
                    modelNames = names(ensembleModel)
                }

                plot(x, ylim = c(0, 1.1 * max(sapply(modelFits, max),
                            na.rm = TRUE)), type = "n",
                     xlab = "", ylab = "")
                colPal = brewer.pal(n.model, "Paired")
                for(i in 1:n.model){
                    lines(modelFits[[i]], col = colPal[i])
                }
                lines(1:T, ensembleFit, col = "steelblue", lwd = 3)
                points((1:T)[missIndex], ensembleFit[missIndex],
                       col = "steelblue", cex = 1, pch = 19)
                points(x, pch = 19)
                legend("topleft",
                       legend = c(paste0(modelNames, "(",
                           round(modelWeights * 100, 2),
                           "%)"), "Ensemble"),
                       col = c(colPal, "steelblue"),
                       lwd = c(rep(1, n.model), 3),
                       bty = "n")
            }
        }
    } else {
        ensemble = x
    }
    ensemble
}
