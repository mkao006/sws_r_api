##' The default logistic model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export

defaultLogistic = function(x){
    time = 1:length(x)
    xmax = max(x, na.rm = TRUE)
    x.scaled = x/xmax
    logisticModel = glm(formula = x.scaled ~ time, family = "binomial")
    logisticFit =
        predict(logisticModel, 
                newdata = data.frame(time = time),
                type = "response") * xmax
    midpoint = - coef(logisticModel)[1]/coef(logisticModel)[2]
    if(length(na.omit(x[time < midpoint])) < 1 |
       length(na.omit(x[time > midpoint])) < 1 )
        logisticFit = rep(NA, length(x))
    logisticFit
}
