##' The default exponential model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultExp = function(x){
    time = 1:length(x)
    T = length(x)
    expFit = exp(predict(lm(formula = log(x + 1) ~ time),
        newdata = data.frame(time = time)))
    if(max(expFit, na.rm = TRUE) > 5 * max(x, na.rm = TRUE) |
       length(na.omit(head(x, 5))) < 1 |
       length(na.omit(tail(x, 5))) < 1)
        expFit = rep(NA, T)
    expFit
}
