##' Basic LM Test
##' 
##' This function computes a linear model for a set of values and returns
##' either a 0/1 indicating if a value is flagged as "bad" or a score
##' indicating how far that value is from the expected value at that point.
##' 
##' @param y A numeric vector containing the data values.
##' @param x A numeric vector containing the independent variable.  If
##' omitted, this is assumed to be 1:length(y).  Typically, this variable
##' corresponds to time.
##' @param returnType One of "score" or "flag".  If "score", a p-value is
##' returned; otherwise a binary value is returned corresponding to whether
##' or not the observation is three standard deviations from the expected
##' value.
##' @param robust Logical indicating if a robust estimate of the linear
##' regression model should be used.  Defaults to TRUE, and this is
##' recommended as the data may have outliers.
##' @param alphaLevel Specify the p-value at which we conclude that an
##' observation is an outlier.  The default value will lead to 3 standard
##' deviations as an outlier.
##' 
##' @return A vector of the same length as the input y.  See returnType.  In
##' the case of the binary return value, a 1 will represent a flagged value,
##' i.e. a value which is considered bad or a potential outlier.
##' 

basicLmTest = function(y, x = 1:length(y), returnType = "flag", robust = TRUE,
                       alphaLevel = 2*pnorm(-3)){
    
    ## Data Quality Checks
    stopifnot(is(y, "numeric"))
    stopifnot(is(x, "numeric"))
    stopifnot(length(x) == length(y))
    stopifnot(returnType %in% c("score", "flag"))
    stopifnot(is(robust, "logical"))

    ## Check if enough data exists, and if not return all NA's.
    ## For regression, we need more than 3 observations, as estimating the
    ## intercept, slope, and error requires 3 d.o.f.
    if(sum(!is.na(y)) <= 3)
        return(rep(NA_real_, length(y)))
    
    data = data.frame(x = x, y = y)
    if(robust){
        fit = try(robustbase::lmrob(y ~ x, data = data))
        modelPrediction = try(robustbase:::predict.lmrob(fit, data,
                                                         se.fit = TRUE))
    }
    # Note: modelPrediction won't exist if robust is FALSE, but lazy evaluation
    # prevents this from being an issue.
    if(!robust || is(modelPrediction, "try-error")){
        fit = lm(y ~ x, data = data)
        modelPrediction = predict(fit, data, se.fit = TRUE)
    }
    SE = modelPrediction$se.fit + summary(fit)$sigma
    
    score = (y - modelPrediction$fit)/SE
    if(returnType == "score")
        return(1-abs(pnorm(score)))
    else
        return(as.numeric(abs(score) > -qnorm(alphaLevel/2)))
}

# # Test on simple data, verify code agrees with R's default confidence interval.
# y = 1:20 + rnorm(20)
# y[10] = 18
# x = 1:20
# 
# fit = lm(y ~ x)
# error = predict(fit, interval = "predict", level = .997)
# 
# qplot(x, y, color = factor(basicLmTest(y, x, robust = FALSE))) +
#     geom_smooth(method = "lm", color = 1, se = FALSE) +
#     geom_ribbon(data = data.frame(error), aes(x = 1:20, ymin = lwr, ymax = upr), alpha = .15)
