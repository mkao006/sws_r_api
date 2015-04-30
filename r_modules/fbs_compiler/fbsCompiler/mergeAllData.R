##' Function to merge all the datasets
##'
##' @param ... The names of data.table to be merged
##'
##' @return A single data.table with all the datasets merged.

mergeAllData = function(...){
    datasets = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all = TRUE)
    },
           x = datasets[-1], init = datasets[[1]])
}
