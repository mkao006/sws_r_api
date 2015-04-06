##' Function to compute the mean squared error of different model fits
##'
##' @param x A numeric vector to be imputed.
##' @param fit The fitted value from the model.
##' @export

computeErrorRate = function(x, fit, type = "mse"){
    if(type == "mse")
        er = sum((x - fit)^2, na.rm = !all(is.na(fit)))
    er
}
