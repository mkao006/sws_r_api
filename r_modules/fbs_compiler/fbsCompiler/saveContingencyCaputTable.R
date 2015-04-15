##' Function to save the contingency table back
##'
##' @param data The balanced contingency table which will be the input
##' of the balancing algorithm.

saveContingencyCaputTable = function(data){
    SaveData(domain = "suafbs",
             dataset = "fbs_prebalance",
             data = data,
             normalized = FALSE)
}
