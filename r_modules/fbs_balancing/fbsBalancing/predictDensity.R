##' Function to predict the density of a given value based on the
##' estimated density.
##'
##' @param density The fitted object returned from ''density''.
##' @param xnew The new set of x for prediction
##'
##' @return The predicted sample density 

predictDensity = function(density,xnew){
    ## Source:
    ## https://github.com/PecanProject/pecan/blob/master/modules/emulator/R/predict.density.R
    ##
    ## function does simple interpolation of a density object to new points
    neval = length(density$x)
    nnew = length(xnew)
    ynew = rep(NA,nnew)
    for(i in 1:nnew){
        j = findInterval(xnew[i],density$x)
        if(j == 0 || j==neval){
            ynew[i] = 0 ## don't extrapolate beyond range,set to 0
        } else {
             ynew[i] =
                 density$y[j] + (density$y[j+1]-density$y[j])/
                     (density$x[j+1]-density$x[j])*(xnew[i]-density$x[j])
         }   
    }
    ynew
}
