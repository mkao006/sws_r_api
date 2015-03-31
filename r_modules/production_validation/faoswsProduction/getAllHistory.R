##' Get All Production History
##' 
##' This function is used to pull the historical production values from the
##' database.  One main purpose of this data is to train machine learning
##' algorithms on invalid observations, and use those models to attempt to
##' detect invalid observations in the current data.
##' 
##' @return A data.table object with the following columns:
##' \itemize{
##'     \item geographicAreaM49: The code for the country of this observation.
##'     \item measuredElement: The code for the element of this observation 
##'     (these match with the "Element" dimension in the agriculture domain,
##'     for example 5312 is area harvested, 5510 is production, etc.).
##'     \item measuredItemCPC: The CPC code for the commodity.
##'     \item timePointYears: The year for this observation.
##'     \item Version: ?
##'     \item StartDate: The initial date this value was entered into the
##'     system.
##'     \item EndDate: The date that this value was overwritten.  If NA, this
##'     value is the current best estimate and is considered valid.  If not NA,
##'     this value is considered invalid.
##'     \item Metadata: ?
##'     \item Metadata_Language: An abbreviation for the language of the
##'     metadata comments.
##'     \item Metadata_Group: ?
##'     \item Metadata_Element: ?
##'     \item Metadata_Value: ?
##'     \item Value: The value stored in the database for this observation.
##'     \item flagObservationStatus: The flag corresponding to the observation
##'     status of the current observation.  See the faoswsFlag package for more
##'     details.
##'     \item flagMethod: The method flag for the current observation.  Again,
##'     see the faoswsFlag package.
##'     \item valid: A Y/N value indicating whether or not the current value is
##'     valid.  This is determined solely on the basis of whether or not this
##'     value was overwritten.
##'     \item productionValue: The value corresponding to production (element
##'     5510) for this particular observation.  This may be useful in training
##'     the classification ensemble.
##' }
##' 
##' @export
##' 

getAllHistory = function(){

    allCountries =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "geographicAreaM49")[type == "country", code]

    ## Lets just test on subtree
    allItems =
        adjacent2edge(GetCodeTree(domain = "agriculture",
                                  dataset = "agriculture",
                                  dimension = "measuredItemCPC",
                                  roots = "01"))$children

    ## Only data after 1990 has history
    allYears =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = "timePointYears")[description != "wildcard" ,code]
    allYears = as.numeric(allYears)
    allYears = allYears[allYears >= 1990]
    
    ## Create the new expanded keys
    newKey = DatasetKey(
        domain = "agriculture",
        dataset = "agriculture",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = allCountries),
            Dimension(name = "measuredElement",
                      keys = c("5312", "5510", "5412")),
            Dimension(name = "measuredItemCPC",
                      keys = allItems),
            Dimension(name = yearVar,
                      keys = as.character(allYears))
            )
        )

    ## Pivot to vectorize yield computation
    newPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = FALSE),
        Pivoting(code = "measuredElement", ascending = TRUE)
        )

    allHistory = GetHistory(key = newKey)

    ## Convert time to numeric and levels to factor
    allHistory[, `:=`(c("geographicAreaM49", "measuredItemCPC", "measuredElement",
                        "timePointYears", "flagMethod", "flagObservationStatus"),
                      list(factor(geographicAreaM49, levels = allCountries), 
                           factor(measuredItemCPC, level = allItems),
                           factor(measuredElement,
                                  levels = c("5312", "5510", "5412")),
                           as.numeric(timePointYears),
                           factor(flagMethod),
                           factor(flagObservationStatus)))]

    ## Remove missing observation
    allHistory = allHistory[flagObservationStatus != "M", ]

    ## create the binary response
    allHistory[, valid := factor(ifelse(is.na(EndDate), "Y", "N"))]

    ## Removing duplicated values.
    allHistory[valid == "Y", validValue := Value]
    allHistory[, validValue := na.omit(unique(validValue)),
               by = c("geographicAreaM49", "measuredItemCPC", "measuredElement",
                   "timePointYears")]

    allHistory = allHistory[!(valid == "N" & validValue == Value), ]
    allHistory[, validValue := NULL]
    ## Add productionValue to all records.  For a very few unique combinations
    ## of geographicAreaM49, measuredItemCPC, and timePointYears we have
    ## multiple, valid production values in the database.  To accomodate that,
    ## take the mean so that the same value is filled in.
    allHistory[, productionValue :=
                   mean(.SD[measuredElement == "5510" & is.na(EndDate), Value]),
               by = c("geographicAreaM49", "measuredItemCPC", "timePointYears")]
    allHistory
}