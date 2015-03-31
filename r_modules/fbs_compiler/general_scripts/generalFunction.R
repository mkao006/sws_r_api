## NOTE (Michael): Need a function to get primary commodities
getPrimaryMeasuredItemCPC = function(dataContext){

    itemTable =
        GetCodeList(domain = "agriculture",
                    dataset = "agriculture",
                    dimension = itemVar)

    ## This is a hack to get primary commodities
    ## itemTable[nchar(gsub("[^0-9]", "", code)) == 4 & nchar(code) == 4 &
    ##           !is.na(type), code]
    itemTable[!is.na(type), code]
}

## Get nutrient data
getNutrientData = function(){
    nutrientKey =
        DatasetKey(domain = "suafbs",
                   dataset = "nutrient_factors_cpc",
                   dimensions = list(
                       ## NOTE (Michael): We take energy in Kcal here
                       Dimension(name = "measuredElementNutritive",
                                 keys = "904"),
                       Dimension(name = "measuredItemCPC",
                                 keys = primaryMeasuredItemCPC),
                       Dimension(name = "timePointFake",
                                 keys = "1")
                   )
                   )

    nutrientPivot = c(
        Pivoting(code = "measuredItemCPC", ascending = TRUE),
        Pivoting(code = "timePointFake", ascending = TRUE),
        Pivoting(code = "measuredElementNutritive", ascending = TRUE)
    )

    nutrientQuery =
        GetData(key = nutrientKey,
                flags = FALSE,
                normalized = FALSE,
                pivoting = nutrientPivot)
    nutrientQuery[, timePointFake := NULL]
    nutrientQuery
}


getPopulationData = function(){
    populationKey = DatasetKey(
        domain = "population",
        dataset = "population",
        dimensions = list(
            Dimension(name = "geographicAreaM49",
                      keys = selectedCountry),
            Dimension(name = "measuredElementPopulation",
                      keys = "11"),
            Dimension(name = "timePointYears",
                      keys = selectedYear)
        )
    )

    ## Pivot to vectorize yield computation
    populationPivot = c(
        Pivoting(code = "geographicAreaM49", ascending = TRUE),
        Pivoting(code = "timePointYears", ascending = TRUE),
        Pivoting(code = "measuredElementPopulation", ascending = FALSE)
    )

    ## Query the data
    populationQuery = GetData(
        key = populationKey,
        flags = TRUE,
        normalized = FALSE,
        pivoting = populationPivot
    )

    ## Convert population in thousands to count
    populationQuery[, Value_measuredElementPopulation_11:=
                        Value_measuredElementPopulation_11 * 1000]
    
    ## Convert time to numeric
    populationQuery[, timePointYears := as.numeric(timePointYears)]
    populationQuery
}

computeCalorie = function(data, quantityVariable, calorieVariable,
    quantityToTonMultiplier, calorieToTonMultiplier, outputName){
    tmp = copy(data)
    if(NROW(tmp) > 0)
        tmp[, `:=`(c(outputName),
                   list(tmp[[quantityVariable]] * quantityToTonMultiplier *
                            tmp[[calorieVariable]] * calorieToTonMultiplier))]
    tmp
}

calorieStandardization = function(data, commodityTree, standardizeVariable,
                                  standardizationKey){
    commodityTreeCopy = copy(commodityTree)
    setnames(commodityTreeCopy, old = "cpc_children_code", new = itemVar)

    dataTree = merge(data, commodityTreeCopy, by = itemVar, all.x = TRUE)

    standardized =
        dataTree[, lapply(standardizeVariable, FUN = function(x) sum(.SD[[x]])),
                 by = c(standardizationKey)]

    setnames(standardized, old = paste0("V", 1:length(standardizeVariable)),
             new = standardizeVariable)
    standardized
}

mergeAllData = function(...){
    datasets = list(...)
    Reduce(f = function(x, y){
        keys = intersect(colnames(x), colnames(y))
        setkeyv(x, keys)
        setkeyv(y, keys)
        merge(x, y, all = TRUE)
    },
           x = datasets[-1], init = datasets[[1]])
}


getFBSHiearchy = function(level){

    ## Get the nth level root
    fullList =
        adjacent2edge(GetCodeTree(domain = "suafbs",
                                  dataset = "fbs",
                                  dimension = "measuredItemSuaFbs",
                                  roots = "S2901"))
    fullGraph = graph.data.frame(fullList[, list(children, parent)])
    distanceToRoot = shortest.paths(fullGraph, to = "S2901", mode = "out")
    nthLevelFBS = rownames(distanceToRoot)[which(distanceToRoot == level)]
    ## HACK (Michael): The tree hierachy is set up wrong, these two items
    ##                 should be third level not second level.
    nthLevelFBS = nthLevelFBS[!nthLevelFBS %in% c("S2541", "S2899")]
    

    ## Get all the sub tree
    nthLevelList =
        adjacent2edge(GetCodeTree(domain = "suafbs",
                                  dataset = "fbs",
                                  dimension = "measuredItemSuaFbs",
                                  roots = nthLevelFBS))

    ## HACK (Michael): Remove duplicate, the tree again is set up
    ##                 incorrectly in the database.
    nthLevelTree = nthLevelList[!nthLevelList[, duplicated(children)], ]
    nthLevelGraph = graph.data.frame(nthLevelTree[, list(children, parent)])
    leaf = names(which(degree(nthLevelGraph, mode = "in") == 0))

    ## Find the relation between leaf and the nth terminal root
    distMatrix =
        shortest.paths(nthLevelGraph, v = leaf, to = nthLevelFBS, mode = "out")

    index = which(is.finite(distMatrix), arr.ind = TRUE)

    ## Create the mapping
    commodityTree =
        data.table(cpc_children_code =  leaf[index[, 1]],
                   cpc_standardized_code = nthLevelFBS[index[, 2]])

    commodityTree
}


getHStoCPCMapping = function(){    
    mappingKey = MappingTableKey(mappingTable = "tradeHS2CPC")
    GetMapping(mappingKey)
}


missingValueToZero = function(data, valueColumns){
    if(missing(valueColumns))
        valueColumns = grep("Value", colnames(data), value = TRUE)
    dataCopy = copy(data)    
    dataCopy[, `:=`(c(valueColumns),
                    lapply(valueColumns,
                           FUN = function(x){
                               tmp = .SD[[x]]
                               tmp[is.na(tmp)] = 0
                               tmp
                           }))]
    dataCopy
}


calculateResidual = function(data, production, import, export, seed, loss,
    industrialUse, food, feed, residualVariable){

    dataCopy = copy(data)
    dataCopy[, `:=`(c("Value_measuredElementCalorie_residual"),
                    rowSums(dataCopy[, c(production, import), ,with = FALSE]) -
                    rowSums(dataCopy[, c(export, seed, loss, industrialUse, food,
                                         feed),
                                     with = FALSE]))]
}


calculatePerCaput = function(data, populationVar, valueColumns){
    if(missing(valueColumns))
        valueColumns = grep("Value", colnames(data), value = TRUE)
    dataCopy = copy(data)    
    dataCopy[, `:=`(c(valueColumns),
                    lapply(valueColumns,
                           FUN = function(x){
                               computeRatio(.SD[[x]],
                                            .SD[[populationVar]] * 365)
                           }))]
    dataCopy
}

saveContingencyCaputTable = function(data){
    SaveData(domain = "suafbs",
             dataset = "fbs_prebalance",
             data = data,
             normalized = FALSE)
}
             

addFBSFlags = function(data, valueColumns, valuePrefix,
    flagObsStatusPrefix, flagMethodPrefix){

    dataCopy = copy(data)
    flagObsColumns = gsub(valuePrefix, flagObsStatusPrefix, valueColumns)
    flagMethodColumns = gsub(valuePrefix, flagMethodPrefix, valueColumns)
    
    dataCopy[, `:=`(c(flagObsColumns), "E")]
    dataCopy[, `:=`(c(flagMethodColumns), "e")]
    
    
    keyColumns = colnames(dataCopy)[!colnames(dataCopy) %in%
        c(valueColumns, flagObsColumns, flagMethodColumns)]

    valueTupleOrder = c(t(cbind(valueColumns, flagObsColumns, flagMethodColumns)))
    setcolorder(dataCopy, neworder = c(keyColumns, valueTupleOrder))
    dataCopy
}
