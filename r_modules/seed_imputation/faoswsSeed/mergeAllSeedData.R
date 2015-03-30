##' Merge All Seed Data
##' 
##' <DESCRIPTION>
##' 
##' @param
##' 
##' @return
##' 

mergeAllSeedData = function(seedData, ...){
    explanatoryData = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all.x = TRUE)
    },
           x = explanatoryData, init = seedData
           )
}