##' Example seed data for the vignette.
##'
##' The data contain seed data from 8 countries, 140 commodities and spanning
##' the years 1994-2013.  The countries were not randomly chosen: some were
##' picked because of the availability of area sown variables (which is
##' generally rather rare).
##' 
##' The rather cryptic columns correspond to what exists in the SWS:
##' \itemize{
##'     \item{geographicAreaM49: }{The M49 codes specifying the country}
##'     \item{measuredItemCPC: }{The codes specifying the commodity}
##'     \item{timePointYears: }{The year of the observation}
##'     \item{Element_5212: }{These three variables correspond to area sown.
##'     The value variable contains the data, and the flagObservationStatus and
##'     flagMethod variables contain the flags.}
##'     \item{Element_5312: }{Area Harvested}
##'     \item{Element_5525: }{Seed usage}
##' }
##'
##' @docType data
##' @keywords datasets
##' @name seedData
##' @usage data(seedData)
##' @format A data.table object with 8960 rows and 12 variables
##'
NULL