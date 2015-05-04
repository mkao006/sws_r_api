##' Remove Non 1-1
##' 
##' This function removes any non 1-1 mappings from the mapping
##' table.
##' 
##' @param data A data.table object containing the mapping.
##' @param key1Colname The column name of one of the keys.
##' @param key2Colname The column name of the other key.
##' 
##' @return data is returned, but with all the one-to-many, many-to-one, and
##' many-to-many relationships removed.
##' 

removeNon1to1 = function(data, key1Colname, key2Colname){
    repeatedKey1 = data[, .N, key1Colname][N > 1, get(key1Colname)]
    repeatedKey2 = data[, .N, key2Colname][N > 1, get(key2Colname)]
    data[!get(key1Colname) %in% repeatedKey1 &
         !get(key2Colname) %in% repeatedKey2, ]
}