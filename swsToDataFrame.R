##' A function to read data from the statistical working system
##'
##' The function reads, and normalize the data from the statistical
##' working system.
##'
##' @param file The csv file
##' @param denormalizer character names of the column in which the
##' data to be denormalized.
##' @export
##'

swsToDataFrame = function(file, denormalizer = NULL){
    swsData = read.csv(file = file, header = TRUE,
        stringsAsFactors = FALSE)
    meltSwsData = melt(swsData,
        id.var = grep("Code|Name", colnames(swsData), value = TRUE))
    
    ind = sapply(regexpr("_", meltSwsData$variable), function(x) x[[1]])
    meltSwsData$Year =
        as.numeric(substring(meltSwsData$variable, ind + 1))
    meltSwsData$type = substring(meltSwsData$variable, 1, ind - 1)
    meltSwsData$variable = NULL

    lefthandside = paste0(c(grep("Code|Name",
        colnames(meltSwsData)[!colnames(meltSwsData) %in% denormalizer],
        value = TRUE), "Year"), collapse = "+")
    righthandside = paste0(c(denormalizer, "type"), collapse = "+")
    final.df = dcast(meltSwsData,
        paste0(lefthandside, " ~ ", righthandside), value.var = "value")
    ## final.df$Num = as.numeric(final.df$Num)
    final.df[, grep("Num", colnames(final.df))] =
        lapply(X = final.df[, grep("Num", colnames(final.df))],
               FUN = as.numeric)
    final.df
}
