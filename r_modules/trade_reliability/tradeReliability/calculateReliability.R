##' Calculate Reliability
##' 
##' This function calculates the trade reliability for the reporting
##' country.  The reliability is the "eigenvector centrality" score, and is
##' computed via the evcent function in the igraph package.  Qualitatively,
##' this score measures how important a particular node is in a network when
##' we're examining flow through a network.  In this context, we can think
##' loosely think about concordance as a measure of flow through the network,
##' and in this context the eigenvector centrality score will give a measure of
##' which nodes "have the most concordance flowing through them", or really
##' which nodes are the most reliable.
##'
##' @param data A data table object summarizing the concordance between a
##' reporting and partner country.  This data.table object is usually generated
##' by calculatePairWiseConcordance.
##' @param reportingCountryVar The column name of data corresponding to the
##' reporting country.
##' @param partnerCountryVar The column name of data corresponding to the
##' partner country.
##' @param yearVar The column name of data corresponding to the year variable.
##' @param concordanceVar The column name of data which contains the pairwise
##' concordance values.
##' @param plot Logical.  Currently unused.
##'
##' @return A data.table object containing three columns: geographicAreaM49,
##' timePointYears, and reliability.  The reliability column provides a measure
##' of how reliable an individual country is at reporting trade in a particular
##' year.
##' 
##' @seealso igraph::evcent
##' 

calculateReliability = function(data, reportingCountryVar, partnerCountryVar, yearVar,
                                concordanceVar = "concordance", plot = FALSE){
  
  yearData = split(data, data[[yearVar]])
  
  calculateEigenReliability = function(data){
    singleYearGraph =
      igraph::graph.data.frame(data[, c(reportingCountryVar, partnerCountryVar,
                                        concordanceVar), with = FALSE],
                               directed = FALSE)
    reliability =
      igraph::evcent(singleYearGraph, weights = data[[concordanceVar]])$vector
    reliabilityTable =
      data.table(geographicAreaM49 = names(reliability),
                 timePointYears = unique(data[[yearVar]]),
                 reliability = reliability)
    reliabilityTable
  }
  
  do.call("rbind", lapply(yearData, FUN = calculateEigenReliability))
}
