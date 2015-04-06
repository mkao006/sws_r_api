##' The default exponential model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultLoess = function(x){
    time = 1:length(x)
    T = length(x)
    n.obs = length(na.omit(x))
    ## Need to check this span
    span = ifelse(n.obs/T >= 0.5, 0.3, ifelse(n.obs >= 10, 0.75, 1))
    loessFit = try(predict(loess(formula = x ~ time,
        control = loess.control(surface = "direct"),
        span = span, degree = 1),
        newdata = data.frame(time)), silent = TRUE)
    
    if(!inherits(loessFit, "try-error") & n.obs >= 5){
        loessFit[loessFit < 0] = 0
    } else {
        loessFit = rep(NA, T)
    }
    loessFit
}
