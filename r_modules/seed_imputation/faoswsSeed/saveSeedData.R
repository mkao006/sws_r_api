##' Save Seed Data
##' 
##' This function takes the seed data dataset and saves it back to the
##' database.
##' 
##' @param data The data.table object containing the seed data to be written to
##' the database.
##' 
##' @return No R objects are returned, as this function's purpose is solely to
##' write to the database.
##' 
##' @export
##' 

saveSeedData = function(data){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    
    ## Remove columns to match the database
    requiredColumns = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears", "Value_measuredElement_5212",
                        "flagObservationStatus_measuredElement_5212",
                        "flagMethod_measuredElement_5212",
                        "Value_measuredElement_5312",
                        "flagObservationStatus_measuredElement_5312",
                        "flagMethod_measuredElement_5312",
                        "Value_measuredElement_5525",
                        "flagObservationStatus_measuredElement_5525",
                        "flagMethod_measuredElement_5525")
    data = data[, requiredColumns, with = FALSE]
    missingColumns = requiredColumns[!requiredColumns %in% colnames(data)]
    if(length(missingColumns) > 0)
        stop("Missing required columns, so data cannot be saved!  Missing:\n",
             paste0(missingColumns, collapse = "\n"))
    
    ## Filter the data by removing any invalid date/country combinations
    data = faoswsUtil::removeInvalidDates(data)
    
    ## Save the data back
    SaveDataNew(domain = "agriculture",
             dataset = "agriculture",
             data = data,
             normalized = FALSE)
}