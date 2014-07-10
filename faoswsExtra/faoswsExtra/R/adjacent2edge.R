##' Function to convert tree in adjacent list format to edge
##' representation.
##'
##' @param The graph or tree in adjacent list format.
##'
##' @export
##' 


adjacent2edge = function(tree){
    children = strsplit(unlist(tree[, 2, with = FALSE]), ", ")
    data.table(parent = rep(unlist(tree[, 1, with = FALSE]),
                   sapply(children, length)),
               children = unlist(children))
}
