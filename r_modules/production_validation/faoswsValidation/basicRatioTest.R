##' Basic Ratio Test
##' 
##' This function transforms the time series into a series of ratios.  So,
##' if the original observations are X1, X2, ... then the new observations
##' are X2/X1, X3/X2, etc.  We can then look for outliers on this new dataset.
##' 
##' Note: Use this test with caution!  Negative values will give negative
##' ratios, and that could make the results quite bizarre.  Additionally,
##' values of 0 will cause problems.
##' 
##' @param y A numeric vector containing the data values.
##' @param returnType One of "score" or "flag".  If "score", a p-value is
##' returned; otherwise a binary value is returned corresponding to whether
##' or not the observation is three standard deviations from the mean.
##' @param robust Logical indicating if a robust estimate of the mean ratio
##' should be used.  Defaults to TRUE, and this is recommended as the data
##' may have outliers.
##' @param alphaLevel Specify the p-value at which we conclude that an
##' observation is an outlier.  The default value will lead to 3 standard
##' deviations as an outlier.
##' 
##' @return A vector of the same length as the input y.  See returnType.  In
##' the case of the binary return value, a 1 will represent a flagged value,
##' i.e. a value which is considered bad or a potential outlier.  Note that if
##' the ith ratio is deemed to be a potential outlier, this means X_{i+1}/Xi is
##' large and so we flag X_{i+1}.
##' 

basicRatioTest = function(y, returnType = "flag", robust = TRUE,
                          alphaLevel = 2*pnorm(-3)){
    
    ## Data Quality Checks
    stopifnot(is(y, "numeric"))
    stopifnot(returnType %in% c("score", "flag"))
    stopifnot(is(robust, "logical"))
    if(any(y == 0))
        warning("Some values are 0!  Are you sure a ratio test is reasonable?")
    
    ## Check if enough data exists, and if not return all NA's.
    ## For ratio means, we need more than 3 observations, as estimating the
    ## mean and error requires 2 d.o.f. and we lose one observation by taking
    ## ratios.
    if(sum(!is.na(y)) <= 3)
        return(rep(NA_real_, length(y)))

    ratio = y[-1] / y[-length(y)]
    out = basicMeanTest(y = ratio, returnType = returnType, robust = robust,
                        alphaLevel = alphaLevel)
    ## Append on an NA or a 0 to account for the fact that the first value of
    ## y isn't really tested by this test.
    if(returnType == "score")
        return(c(NA_real_, out))
    else
        return(c(0, out))
}

# # Test on simple data
# y = rexp(20)
# y[10] = 10
# 
# ratio = y[-1]/y[-20]
# qplot(1:20, y, color = factor(basicRatioTest(y, robust = FALSE)))
# qplot(1:19, y[-1]/y[-20], color = factor(basicRatioTest(y, robust = FALSE))[-1]) +
#     geom_ribbon(aes(ymax = mean(ratio) + 3*sd(ratio),
#                     ymin = mean(ratio) - 3*sd(ratio), color = NA), alpha = .15)
