#' Aggregate a list of causality.graphs into a single object
#'
#' \code{aggregate_graphs} will combine \code{graphs} into an
#' "aggregated.causality.graph" object which can be used for bootstrapping and
#' jackknifing causal discovery algorithms among other things.
#' @param graphs A list of causality.graphs. Each graph must have the same nodes.
#' @param weights An optional vector of graph weights to be used in the
#'   aggregation process. If weights is not supplied, it defaults to
#'   \code{length(graphs)}. If weights is non NULL, the length of weights must
#'   equal the length of graphs, the sum of weights must be greater than 0,
#'   and each weight must be non-negative.
#' @details
#' TODO
#' @examples
#' TODO
#' @author Alexander Rix
#' @rdname aggregate-graphs
#' @useDynLib causality r_causality_aggregate_graphs
#' @export
aggregate_graphs <- function(graphs, filter = .1, weights = NULL)
{
  if (!is.list(graphs))
    stop("graphs is not as list")
  if (length(graphs) == 1)
    stop("graphs has length 1")
  if (sum(unlist(lapply(graphs, is.cgraph))) != length(graphs))
    stop("Every graph in graphs must be a causality.graph!")
  if (is.null(weights)) {
    weights <- rep(1, length(graphs))
  }
  else {
  if (length(weights) != length(graphs))
    stop("weights must have the same length as graphs")
  if (sum(weights) <= 0)
    stop("weights must sum up to a number greater than 0")
  if (sum(weights >= 0) < length(weights))
    stop("Each weight must be non negative")
  }

  graphs <- lapply(graphs, function(graph) {graph$nodes <- sort(graph$nodes)
                                            graph})
  base <- graphs[[1]]
  # see if all the graphs have the EXACT same nodes
  same_nodes <- lapply(graphs, function(graph) {
    isTRUE(all.equal(base$nodes, graph$nodes))
  })
  same_nodes <- isTRUE(all.equal(unlist(same_nodes), rep(T, length(graphs))))
  if (!same_nodes)
    stop("Not all the graphs have the same nodes")

  table <- .Call("r_causality_aggregate_graphs", graphs, weights)
  acg <- data.frame(table[[1]], table[[2]], table[[10]], table[[3]], table[[4]],
                    table[[11]], table[[5]], table[[12]], table[[6]],
                    table[[13]], table[[7]], table[[8]],
                    table[[9]], stringsAsFactors = F)
  acg.names <- c("x", "y", "<--", "-->", "---", "<++", "++>", "<~~", "~~>",
                 "<-o", "o->", "o-o", "<->")
  names(acg) <- acg.names

  for (col in names(acg)[-(1:2)])
    if (sum(acg[[col]]) == 0) acg[[col]] <- NULL
  acg <- acg[(rowSums(acg[, -(1:2)]) >= filter),]
  row.names(acg) <- 1:nrow(acg)
  output <- list(nodes = base$nodes, edge.table = acg)
  class(output) <- "aggregated.causality.graph"
  return(output)
}

#' @rdname aggregate-graphs
#' @export
coalesce <- function(aggregated.graph) {
  if (class(aggregated.graph) != "aggregated.causality.graph")
  stop("aggregated.graph must be an aggregated.causality.graph")
  nodes     <- aggregated.graph$nodes
  table     <- aggregated.graph$edge.table
  table[[" "]] <- 1 - rowSums(table[, -(1:2)])
  arrows    <- names(table[, -(1:2)])[max.col(table[, -(1:2)])]
  edge      <- function(node1, node2, arrow) {
    if (arrow == "<--")
      return(c(node2, node1, "-->"))
    if (arrow == "<~~")
      return(c(node2, node1, "~~>"))
    if (arrow == "<++")
       return(c(node2, node1, "++>"))
    if (arrow == "<-o")
      return(c(node2, node1, "o->"))
    if (arrow == " ")
      return(NULL)
    return(c(node1, node2, arrow))
  }
  edges <- c()
  for (i in 1:length(arrows))
    edges <- rbind(edges, edge(table$x[i], table$y[i], arrows[i]))
  return(cgraph(nodes, edges))
}
