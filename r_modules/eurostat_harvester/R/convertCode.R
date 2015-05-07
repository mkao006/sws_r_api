##' Convert Code
##' 
##' This function takes a table with eurostat or FBS data and converts codes
##' (commodity, country, element, etc.) from one system to another.  The
##' user must, however, provide a mapping table as well.
##' 
##' @param data The data.table object containing the data of interest.
##' @param mappingTable A data.table object containing a map between the
##' original code and the new code.
##' @param keyData The name of the column of data which contains the old key.
##' @param newKeyName The name which should be given to the new column of data
##' after the merge.
##' @param newKeyMap The name of the column of mappingTable which contains the
##' old key.
##' @param oldKeyMap The name of the column of mappingTable which contains the
##' new key.
##' 
##' @return No object is returned.  Instead, data is modified to contain the
##' new code.
##' 

convertCode = function(data, mappingTable, keyData, newKeyName,
                       newKeyMap, oldKeyMap = keyData){
    
    ## Data QUality Checks
    stopifnot(is(data, "data.table"))
    stopifnot(is(mappingTable, "data.table"))
    stopifnot(keyData %in% colnames(data))
    stopifnot(c(newKeyMap, oldKeyMap) %in% colnames(mappingTable))
    
    ## Get original keys to reassign at end
    origKeyData = key(data)
    origKeyMap = key(mappingTable)
    origRowCount = nrow(data)

    ## Prepare tables for merge and perform the merge
    if(oldKeyMap != keyData)
        setnames(mappingTable, oldKeyMap, keyData)
    setkeyv(data, keyData)
    setkeyv(mappingTable, keyData)
    data[mappingTable, c(newKeyName) := get(newKeyMap), nomatch = 0]
    if(nrow(data) != origRowCount)
        stop("Rows have been lost or added by the merge.  This should NOT be ",
             "happening during a merge of codes.  Please check the data and ",
             "ensure the mapping tables/code are set up correctly.")
    if(is.null(data[[newKeyName]]))
        stop("The merge has failed.  Are you sure you're using the right ",
             "columns?")
    
    ## Get rid of excess columns, set keys to original
    data[, c(keyData) := NULL]
    if(oldKeyMap != keyData)
        setnames(mappingTable, keyData, oldKeyMap)
    setkeyv(data, origKeyData)
    setkeyv(mappingTable, origKeyMap)
}