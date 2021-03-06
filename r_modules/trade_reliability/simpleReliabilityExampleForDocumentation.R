library(igraph)

edges = data.frame(report = c(1, 1, 1, 2, 2, 3),
                   partner = c(2, 3, 4, 3, 4, 4),
                   concord = c(.1, .1, .1, .9, .9, .9))

singleYearGraph = graph.data.frame(edges, directed = FALSE)
plot(singleYearGraph, edge.width = E(singleYearGraph)$concord*10)
reliability = igraph::evcent(singleYearGraph, weights = edges[, 3])$vector
reliability

edges = data.frame(report = c(1, 1, 1, 1, 2, 2, 2, 3, 3, 4),
                   partner = c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5),
                   concord = c(.9, .85, .2, .15, .8, .3, .1, .1, .15, .8))
singleYearGraph = graph.data.frame(edges, directed = FALSE)
plot(singleYearGraph, edge.width = E(singleYearGraph)$concord*10,
     rescale = FALSE, layout = layout.circle)
reliability = evcent(singleYearGraph, weights = edges[, 3])$vector
reliability

## Verify that reliability is the stable state of the transition matrix
## provided by the "concordance" or agreement between nodes.  On other words,
## if all countries transfer their reliability scores at each step based on
## their argeement with all partners, then reliability is a stable state of
## this process.
edges = as.matrix(edges)
edges = rbind(edges, as.matrix(edges)[, c(2, 1, 3)])
edges = data.frame(edges)
A = reshape::cast(edges, report ~ partner, value = "concord", mean)
A$report = NULL
diag(A) = 0
A
evec = abs(eigen(A)$vector[, 1])
evec / max(evec) - reliability
as.matrix(A) %*% reliability/eigen(A)$value[1]-reliability

## Make some pretty plots showing how reliability is calculated over time

library(RColorBrewer)
cols = brewer.pal(n = 9, name = "Blues")
reliability = rep(.5, 5)
for(i in 1:100){
    colorIndex = round(reliability*9)
    png(paste0("~/Documents/Github/sws_r_api/r_modules/trade_reliability/plots/reliability_",
               i-1, ".png"), width = 300, height = 300)
    plot(singleYearGraph, edge.width = E(singleYearGraph)$concord*10,
         vertex.color = cols[colorIndex], rescale = FALSE, layout = layout.circle,
         vertex.size = 30, ylim = c(-1.1, 1.1))
    dev.off()
    reliability = as.matrix(A) %*% reliability
    reliability = reliability / max(reliability)
}