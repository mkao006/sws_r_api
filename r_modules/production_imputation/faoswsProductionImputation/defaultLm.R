##' The default linear regression model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultLm = function(x){
    time = 1:length(x)
    lmFit = predict(lm(formula = x ~ time),
        newdata = data.frame(time = time))
    lmFit[lmFit < 0] = 0
    lmFit    
}
