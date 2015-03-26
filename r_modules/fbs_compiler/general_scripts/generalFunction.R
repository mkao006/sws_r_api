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


computeCalorie = function(data, quantityVariable, calorieVariable,
    quantityToTonMultiplier, calorieToTonMultiplier, outputName){
    tmp = copy(data)
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
        data.frame(cpc_children_code =  leaf[index[, 1]],
                   cpc_standardized_code = nthLevelFBS[index[, 2]])

    commodityTree
}
