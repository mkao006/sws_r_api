##' Save Production Data
##' 
##' This function takes the a seed data dataset and saves it back to the
##' database.
##' 
##' @param data The data.table object containing the seed data to be written to
##' the database.
##' @param areaHarvestedCode Character string containing the element code
##' corresponding to the area harvested variable.
##' @param yieldCode Character string containing the element code
##' corresponding to the yield variable.
##' @param productionCode Character string containing the element code
##' corresponding to the production variable.
##' @param verbose Should output be printed about the progress of the save
##' data?  Defaults to FALSE.
##' 
##' @return No R objects are returned, as this functions purpose is solely to
##' write to the database.
##' 
##' @export
##' 

saveProductionData = function(data, areaHarvestedCode = "5312",
                              yieldCode = "5421", productionCode = "5510",
                              verbose = FALSE){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    
    ## Remove columns to match the database
    requiredColumns = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears")
    requiredCodes = c(areaHarvestedCode, yieldCode, productionCode)
    additionalColumns = lapply(requiredCodes, function(x)
        paste0(c("Value_measuredElement_",
                 "flagObservationStatus_measuredElement_",
                 "flagMethod_measuredElement_"), x))
    requiredColumns = c(requiredColumns, do.call("c", additionalColumns))
    missingColumns = requiredColumns[!requiredColumns %in% colnames(data)]
    if(length(missingColumns) > 0)
        stop("Missing required columns, so data cannot be saved!  Missing:\n",
             paste0(missingColumns, collapse = "\n"))
    data = data[, requiredColumns, with = FALSE]
    
    ## Filter the data by removing any invalid date/country combinations
    if(verbose)
        cat("Removing invalid date/country combinations from the dataset.\n")
    data = removeInvalidDates(data)
    
    ## Can't save NA's back to the database, so convert to 0M
    for(code in c(areaHarvestedCode, yieldCode, productionCode)){
        valName = paste0("Value_measuredElement_", code)
        obsFlag = paste0("flagObservationStatus_measuredElement_", code)
        methodFlag = paste0("flagMethod_measuredElement_", code)
        data[is.na(get(valName)) & get(obsFlag) == "M",
             `:=`(c(valName, obsFlag, methodFlag),
                  list(0, "M", "n"))]
    }
        
    ## Save the data back
    if(verbose)
        cat("Attempting to write data back to the database.\n")
    faosws::SaveData(domain = "agriculture",
                     dataset = "agriculture",
                     data = data,
                     normalized = FALSE)
    if(verbose)
        cat("Data writing has completed!\n")
}