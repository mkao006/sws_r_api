
selectMirroredTargetData = function(data){
    valueColumns = c(import_quantity, import_value, export_quantity, export_value,
                     import_unit_value, export_unit_value)
    flagColumns = sapply(valueColumns,
        FUN = function(x) gsub(valuePrefix, flagPrefix, x))
    colIndex = which(colnames(data) %in%
        c(reportingCountryVar, partnerCountryVar, itemVar, yearVar,
          valueColumns, flagColumns))
    mirroredData = data[, colIndex, with = FALSE]
    setcolorder(mirroredData,
                neworder = c(reportingCountryVar, partnerCountryVar,
                    colnames(mirroredData)[!colnames(mirroredData) %in%
                                           c(reportingCountryVar,
                                             partnerCountryVar)]))
    mirroredData[, timePointYears := as.character(timePointYears)]
    mirroredData
}
