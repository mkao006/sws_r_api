##' Remove Invalid Dates
##' 
##' Some countries have specific date ranges for which they have valid data.
##' For example, Belarus became a country in 1992, and therefore should not
##' have data prior to 1992.  If the user attempts to write data to this
##' country at a point in time prior to 1992, an error will occur.  This
##' function contains the logic for removing such invalid country/year pairs.
##' 
##' @param data A data.table object, typically one which has been extracted
##' from the SWS and modified/analyzed.
##' @param context An element of type DatasetKey (which is defined in faosws).
##' This object contains information on the domain and dataset of interest.
##' It defaults to swsContext.datasets[[1]], which should usually be what is
##' desired.
##' 
##' @return A data.table object whose invalid rows have been removed.
##' Ultimately, this function should work like the other data.table functions:
##' modify the object in place and not return anything.  However, row deletion
##' cannot be done without copying in the current data.table framework.  This
##' issue is in progress: https://github.com/Rdatatable/data.table/issues/635.
##' 
##' @export
##' 

removeInvalidDates = function(data, context = swsContext.datasets[[1]]){
    
    ## Data Quality Checks
    stopifnot(is(data, "data.table"))
    stopifnot("geographicAreaM49" %in% colnames(data))
    stopifnot(is(context, "DatasetKey"))
    
    areaValidRange = GetCodeList(domain = slot(context, "domain"),
                                 dataset = slot(context, "dataset"),
                                 dimension = "geographicAreaM49")
    cleanDates = function(date){
        date = lapply(date, function(x) ifelse(is.null(x), NA, x))
        do.call("c", date)
    }
    areaValidRange[, startDate := cleanDates(startDate)]
    areaValidRange[, endDate := cleanDates(endDate)]
    setnames(areaValidRange, old = "code", new = "geographicAreaM49")
    data = merge(data, areaValidRange, by = "geographicAreaM49", all.x = TRUE)
    data[, date := as.Date(paste0(data$timePointYears, "-01-01",
                                  format = "%Y-%m-%d"))]
    data = data[is.na(startDate) | date > startDate, ]
    data = data[is.na(endDate) | date < endDate, ]
    data[, c("startDate", "endDate", "date") := NULL]
    return(data)
}