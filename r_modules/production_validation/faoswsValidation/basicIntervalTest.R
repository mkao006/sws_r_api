##' Basic Interval Test
##' 
##' This function simply flags any values that fall outside of a user-specified
##' range.
##' 
##' @param y A numeric vector containing the data values.
##' @param upper A numeric value specifying the largest acceptable value for
##' data in y.
##' @param lower A numeric value specifying the smallest acceptable value for
##' data in y.
##' 
##' @return A numeric vector of the same length as the input y.  A 1 will
##' represent a flagged value, i.e. a value which is considered bad.
##' 

basicIntervalTest = function(y, upper, lower){
    
    ## Data Quality Checks
    stopifnot(is(y, "numeric"))
    stopifnot(lower <= upper)

    return(as.numeric(y > upper | y < lower))
}

# # Test on simple data, verify code agrees with R's default confidence interval.
# basicIntervalTest(1:10, upper = 5, lower = 0)
# basicIntervalTest(1:10, upper = 10, lower = 0)
# basicIntervalTest(1:10, upper = 100, lower = 5)
# basicIntervalTest(1:10, upper = 0, lower = 5)
# 