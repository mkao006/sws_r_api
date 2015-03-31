##' Default Processing Parameters
##' 
##' This function can be used to generate the input parameters for the
##' data pre-processing code.  This is a good way to get a list of the required
##' parameters and then modify parameters to match your particular
##' configuration.
##' 
##' @return Returns a list of the default parameters used in the data
##' pre-processing algorithm.
##' 
##' @details Below is a description of the parameters:
##' \itemize{
##'   \item productionValue: The column name of the production variable.
##'   \item productionObservationFlag: The column name of the observation flag
##'    corresponding to the production variable.
##'   \item productionMethodFlag: The column name of the method flag
##'   corresponding to the production variable.
##'   \item yieldValue: The column name of the yield variable.
##'   \item yieldObservationFlag: The column name of the observation flag
##'   corresponding to the yield variable.
##'   \item yieldMethodFlag: The column name of the method flag corresponding
##'   to the yield variable.
##'   \item areaHarvestedValue: The column name of the area harvested variable.
##'   \item areaHarvestedObservationFlag: The column name of the observation
##'   flag corresponding to the area harvested variable.
##'   \item areaHarvestedMethodFlag: The column name of the method flag
##'   corresponding to the area harvested variable.
##'   \item yearValue: The column name for the year variable in data.
##'   \item byKey: The column name for the variable representing the splitting
##'   group.  Usually, this is the country variable.
##'   \item removePriorImputation: 
##'   \item removeConflictValues: 
##'   \item imputedFlag: 
##'   \item naFlag: How are missing values specified in the database? Usually,
##'   this is "M".
##' }
##' 
##' @export
##' 

defaultProcessingParameters = function(){
    list(productionValue = "Value_measuredElement_5510",
         productionObservationFlag = "flagObservationStatus_measuredElement_5510",
         productionMethodFlag = "flagMethod_measuredElement_5510",
         yieldValue = "Value_measuredElement_5416",
         yieldObservationFlag = "flagObservationStatus_measuredElement_5416",
         yieldMethodFlag = "flagMethod_measuredElement_5416",
         areaHarvestedValue = "Value_measuredElement_5312",
         areaHarvestedObservationFlag = "flagObservationStatus_measuredElement_5312",
         areaHarvestedMethodFlag = "flagMethod_measuredElement_5312",
         yearValue = "timePointYears",
         byKey = "geographicAreaM49",
         removePriorImputation = TRUE,
         removeConflictValues = TRUE,
         imputedFlag = "E",
         naFlag = "M")
}