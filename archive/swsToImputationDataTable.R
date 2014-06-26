##' A function to prepare data for imputation purposes.
##'
##' @param file The csv file
##' @param denormalizer The denormalizer, see details of the
##' \code{swsToDataFrame}
##' 

imputationDataManipulation = function(file, denormalizer){
    sua.df = swsToDataFrame(file = file, denormalizer = denormalizer)

    suaRmImp.df = swsRmImputation(sua.df,
        valueVar = grep("Num", colnames(sua.df), value = TRUE),
        symbVar = grep("Symb", colnames(sua.df), value = TRUE))

    colnames(suaRmImp.df) = gsub("31_", "area", colnames(suaRmImp.df))
    colnames(suaRmImp.df) = gsub("51_", "production",
                colnames(suaRmImp.df))
    
    suaRmImp.df[which(suaRmImp.df$areaNum == 0 &
                      suaRmImp.df$productionNum != 0),
                "areaNum"] = NA
    suaRmImp.df[which(suaRmImp.df$areaNum != 0 &
                      suaRmImp.df$productionNum == 0),
                "productionNum"] = NA

    suaRmImp.df$yieldNum = with(suaRmImp.df,
        computeYield(productionNum, areaNum))
    suaRmImp.df$yieldNumRaw = with(suaRmImp.df,
        computeYield(productionNumRaw, areaNumRaw))

    region.df = FAOregionProfile[, c("FAOST_CODE", "UNSD_MACRO_REG",
        "UNSD_SUB_REG")]
    colnames(region.df)[1] = "Area.Code"
    suaMergeRegion.df = arrange(merge(suaRmImp.df,
        region.df, all.x = TRUE), Area.Code, Item.Code, Year)
    
    suaMergeRegion.df =
        suaMergeRegion.df[!is.na(suaMergeRegion.df$UNSD_SUB_REG), ]
    
    suaImputation.dt = data.table(suaMergeRegion.df)
    suaImputation.dt[, UNSD_MACRO_REG := factor(UNSD_MACRO_REG)]
    suaImputation.dt[, UNSD_SUB_REG := factor(UNSD_SUB_REG)]
    setkeyv(suaImputation.dt, c("Area.Code", "Area.Name", "Year"))

    setnames(suaImputation.dt, old = colnames(suaImputation.dt),
         new = toLowerCamel(colnames(suaImputation.dt)))

    suaImputation.dt
}
