##' Save Production Data
##' 
##' This function takes the a seed data dataset and saves it back to the
##' database.
##' 
##' @param data The data.table object containing the seed data to be written to
##' the database.
##' 
##' @return No R objects are returned, as this functions purpose is solely to
##' write to the database.
##' 
##' @export
##' 

saveProductionData = function(data, areaHarvestedCode = "5312",
                              yieldCode = "5421", productionCode = "5510"){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    
    ## Remove columns to match the database
    requiredColumns = c("geographicAreaM49", "measuredItemCPC",
                        "timePointYears")
    requiredCodes = c(areaHarvestedCode, yieldCode, productionCode)
    additionalColumns = lapply(requiredCodes, function(x)
        paste0(c("Value_measuredElement_", "flagMethod_measuredElement_",
                 "flagObservationStatus_measuredElement_"), x))
    requiredColumns = c(requiredColumns, do.call("c", additionalColumns))
    missingColumns = requiredColumns[!requiredColumns %in% colnames(data)]
    if(length(missingColumns) > 0)
        stop("Missing required columns, so data cannot be saved!  Missing:\n",
             paste0(missingColumns, collapse = "\n"))
    data = data[, requiredColumns, with = FALSE]
    
    ## Filter the data by removing any invalid date/country combinations
    data = faoswsUtil::removeInvalidDates(data)
        
    ## Save the data back
    faosws::SaveData(domain = "agriculture",
                     dataset = "agriculture",
                     data = data,
                     normalized = FALSE)
}