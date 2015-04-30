##' Function to save the standardized calorie back
##'
##' @param data The standardized standard deviation of trade.
##'
##' @return NULL
##' 


saveTradeStandardDeviation = function(data){
    if(NROW(data) > 0)
        SaveData(domain = "trade",
                 dataset = "stddev_caloriescap",
                 data = data, normalized = FALSE)
}
