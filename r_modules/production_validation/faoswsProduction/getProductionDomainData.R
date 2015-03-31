##' Get Production Domain Data
##' 
##' This function is designed to pull production data from the working system.
##' It is essentially a wrapper to the GetData function in faosws, but it 
##' massages the data from that function slightly.
##' 
##' @param key A DatasetKey object, typically as created by GetTestEnvironment.
##' See the argument with the same name in faosws::GetData.
##' 
##' @return A data.table object containing the dataset of interest.
##' 

getProductionDomainData = function(key){

    ### Data Quality Checks
    stopifnot(is(key, "DatasetKey"))
    stopifnot(key@domain == "agriculture")
    stopifnot(key@dataset == "agriculture")
    requiredDimensions = c("geographicAreaM49", "measuredElement",
                           "measuredItemCPC", "timePointYears")
    stopifnot(names(key@dimensions) %in% requiredDimensions)
    stopifnot(requiredDimensions %in% names(key@dimensions))
    requiredElements = c("5312", "5416", "5510")
    stopifnot(key@dimensions$measuredElement@keys %in% requiredElements)
    stopifnot(requiredElements %in% key@dimensions$measuredElement@keys)
    
    ### Define a pivot and pull the data
    pivot = c(Pivoting(code = "measuredItemCPC"),
              Pivoting(code = "geographicAreaM49"),
              Pivoting(code = "timePointYears"),
              Pivoting(code = "measuredElement"))
    data = GetData(key, flags = TRUE, normalized = FALSE, pivoting = pivot)
    
    ## Temporary fix to SWS-797 issue.  Should be able to remove this block when
    ## issue is resolved.
    ### Coerce to appropriate class (NA vectors default to logical)
    warning("Coercion performed assuming SWS-797 is still an issue.")
    data[,`:=`(
        timePointYears = as.numeric(timePointYears),
        Value_measuredElement_5312 = as.numeric(Value_measuredElement_5312),
        Value_measuredElement_5416 = as.numeric(Value_measuredElement_5416),
        Value_measuredElement_5510 = as.numeric(Value_measuredElement_5510),
        flagObservationStatus_measuredElement_5312 =
            as.character(flagObservationStatus_measuredElement_5312),
        flagObservationStatus_measuredElement_5416 =
            as.character(flagObservationStatus_measuredElement_5416),
        flagObservationStatus_measuredElement_5510 =
            as.character(flagObservationStatus_measuredElement_5510),
        flagMethod_measuredElement_5312 =
            as.character(flagMethod_measuredElement_5312),
        flagMethod_measuredElement_5416 =
            as.character(flagMethod_measuredElement_5416),
        flagMethod_measuredElement_5510 =
            as.character(flagMethod_measuredElement_5510)
    )]
    
    ### Some values don't exist in the database, and thus are given NA's.
    ### However, for our purposes, these should be 0M values.
    data[is.na(Value_measuredElement_5312), `:=` (
        flagObservationStatus_measuredElement_5312 = "M",
        flagMethod_measuredElement_5312 = "u")]
    data[is.na(Value_measuredElement_5416), `:=` (
        flagObservationStatus_measuredElement_5416 = "M",
        flagMethod_measuredElement_5416 = "u")]
    data[is.na(Value_measuredElement_5510), `:=` (
        flagObservationStatus_measuredElement_5510 = "M",
        flagMethod_measuredElement_5510 = "u")]
}