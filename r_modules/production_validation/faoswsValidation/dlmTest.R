##' DLM Test
##' 
##' This function computes a dynamic linear model based on the dlm package.
##' This model is a time-series based model which is able to estimate a moving
##' mean and standard deviation based on time series data, and it is able to
##' handle missing data.  For more details about the model, check out
##' vignette("dlm").
##' 
##' @param y A numeric vector containing the data values.
##' @param returnType One of "score" or "flag".  If "score", a p-value is
##' returned; otherwise a binary value is returned corresponding to whether
##' or not the observation is three standard deviations from the expected
##' value.
##' @param alphaLevel Specify the p-value at which we conclude that an
##' observation is an outlier.  The default value will lead to 3 standard
##' deviations as an outlier.
##' @param order The order of the dlm model.  The default of 1 should only
##' be changed if alot of data is available.
##' 
##' @return A vector of the same length as the input y.  See returnType.  In
##' the case of the binary return value, a 1 will represent a flagged value,
##' i.e. a value which is considered bad or a potential outlier.
##' 

dlmTest = function(y, returnType = "flag", alphaLevel = 2*pnorm(-3),
                       order = 1){
    
    ## Data Quality Checks
    stopifnot(is(y, "numeric"))
    stopifnot(is(time, "numeric"))
    stopifnot(length(time) == length(y))
    stopifnot(returnType %in% c("score", "flag"))

    data = ts(y)
    ## Define linear state-space model
    dlmModelStructure = function(par) {
        dlmModPoly(order = order, dV = par[1], dW = par[2])
    }
    ## Estimate the paramater (Variance)
    fitModel = dlmMLE(data, rep(1, 2), dlmModelStructure)
    ## Build the model
    dlmModel = dlmModelStructure(fitModel$par)
    
    ## Apply a Kalman filter to the model to get the mean and variance
    filter = dlmFilter(data, dlmModel)
    rollingMean = filter$m[-1]
    rollingVar = unlist(dlmSvd2var(filter$U.C, filter$D.C))[-1]
    
    score = (as.numeric(data) - rollingMean) / sqrt(rollingVar)
    if(returnType == "score")
        return(1-abs(pnorm(score)))
    else
        return(as.numeric(abs(score) > -qnorm(alphaLevel/2)))
}