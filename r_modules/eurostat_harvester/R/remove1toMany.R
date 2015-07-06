##' Remove 1 to Many
##' 
##' This function removes any 1-to-Many and Many-to-Many mappings from the 
##' mapping table.
##' 
##' @param data A data.table object containing the mapping.
##' @param key1Colname The column name of one of the keys.  Note: key1 will be
##'   unique (i.e. any maps from key2 to multiple key1 values will be removed).
##' @param key2Colname The column name of the other key.
##'   
##' @return data is returned, but with all the one-to-many, many-to-one, and 
##'   many-to-many relationships removed.
##'   

remove1toMany = function(data, key1Colname, key2Colname){
    data = unique(data)
    repeatedKey1 = data[, .N, key1Colname][N > 1, get(key1Colname)]
    data[!get(key1Colname) %in% repeatedKey1, ]
}