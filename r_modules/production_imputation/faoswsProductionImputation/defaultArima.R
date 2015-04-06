##' The default ARIMA model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export

defaultArima = function(x){
    T = length(x)
    arimaModel = try(auto.arima(x), silent = TRUE)
    if(!inherits(arimaModel, "try-error")){
        n.coef = length(coef(arimaModel))
        n.coef = ifelse(length(n.coef) == 0, 0, n.coef)
        name.coef = names(coef(arimaModel))
        name.coef = ifelse(length(name.coef) == 0, "", name.coef)
        if(n.coef == 1 &
           name.coef == "intercept"){
            arimaFit = rep(NA, T)
        } else {
            ## kr = KalmanRun(x, arimaModel$model)
            kr = KalmanSmooth(x, arimaModel$model)            
            tmp = which(arimaModel$model$Z == 1)
            id = ifelse (length(tmp) == 1, tmp[1], tmp[2])
            ## arimaFit = kr$states[,id]
            arimaFit = kr$smooth[,id]
            arimaFit[arimaFit < 0] = 0
        }
    } else {
        arimaFit = rep(NA, T)
    }
    arimaFit
}
