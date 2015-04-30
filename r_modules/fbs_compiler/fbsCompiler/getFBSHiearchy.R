##' Function to get the standardization relationship based on the CPC
##' hiearchy.
##'
##' The function finds maps the leaves of the CPC commodity tree to
##' the level specified in the argueent.
##'
##' @param level The level of aggregation. (e.g. Cereals is level 1
##' while wheat and products are level 2.)
##'
##' @return A data.table object specifying the standardization
##' relationship.

getCPCHiearchy = function(level){

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
