##' The default MARS model for the ensemble model.
##'
##' @param x A numeric vector to be imputed.
##' @export


defaultMars = function(x){
    time = 1:length(x)
    T = length(x)
    obs.x = na.omit(x)
    obs.time = time[-attr(obs.x, "na.action")]
    marsFit = try(predict(earth(obs.x ~ obs.time),
        newdata = data.frame(obs.time = time)), silent = TRUE)
    if(!inherits(marsFit, "try-error")){
        marsFit = c(marsFit)
        marsFit[marsFit < 0] = 0
    } else {
        marsFit = rep(NA, T)
    }
    marsFit
}
