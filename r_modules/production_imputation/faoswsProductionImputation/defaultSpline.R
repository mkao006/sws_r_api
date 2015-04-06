##' The default spline model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export

defaultSpline = function(x){
    T = length(x)
    time = 1:length(x)
    splineFit = spline(time, x, n = T * 5 - 4, xout = time,
        method = "natural")$y
    splineFit[splineFit < 0] = 0
    splineFit
}
