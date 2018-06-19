
.arrowhead_wrap <- function(true_graph, est_graph,
                           method = c("precision", "recall"))
{
  if (!is.cgraph(true_graph))
    stop("true_graph must be a causality graph")
  if (!is.cgraph(est_graph))
    stop("est_graph must be a causality graph")
  if (!(is.pdag(true_graph | is.dag(true_graph) | is.pattern(true_graph))))
      stop("true_graph must be either a DAG, PDAG, or pattern!")
  if (!(is.pdag(est_graph | is.dag(est_graph) | is.pattern(est_graph))))
    stop("est_graph must be either a DAG, PDAG, or pattern!")

  method <- match.arg(method)

  if (method == "recall") {
    n_true_arrows <- 0
    edges <- true_graph$edges
    for (i in 1:nrow(edges)) {
      if (edges[i, 3] == .DIRECTED)
        n_true_arrows <- n_true_arrows + 1
    }
    if(n_true_arrows == 0) {
      warning("true_graph contains no oriented edges. Returning NA")
      return(NA)
    }
    return(.arrow_intersect(true_graph, est_graph)/n_true_arrows)
  }
  else { # ie, method is precision
    n_predicted_arrows <- 0
    for (i in 1:nrow(est_graph$edges)) {
      edges <- est_graph$edges
      if (edges[i, 3] == .DIRECTED)
        n_predicted_arrows <- n_predicted_arrows + 1
    }
    if(n_predicted_arrows == 0) {
      warning("est_graph contains no oriented edges. Returning NA")
      return(NA)
    }
    return(.arrow_intersect(true_graph, est_graph)/n_true_arrows)
  }
}

.arrow_intersect <- function(true_graph, est_graph) {
  n_correctly_predicted <- 0
  # index over the edges in the true graph and estimated graph. Recall that an
  # edge is a vector that consists of (origin, destination, edge_type) eg
  # ("X1", "X2", "-->")
  eg_edges <- est_graph$edges
  tg_edges <- true_graph$edges
  for (i in 1:nrow(eg_edges)) {
    eg_edge <- eg_edges[i, ]
    # if the edge in unoriented, skip
    if (eg_edge[3] == .UNDIRECTED)
      next
    for (j in 1:nrow(tg_edges)) {
      tg_edge <- tg_edges[j, ]
      # if the edge is unoriented, skip
      if (tg_edge[3] == .UNDIRECTED)
        next
      # check to see if the edges have the same edge type,
      # and if the eg_edge is unoriented
      if (eg_edge[3] == tg_edge[3]) {
        # if they do have the same edge type, check to see if they have the same
        # origin and destination
        if (eg_edge[1] == tg_edge[1] && eg_edge[2] == tg_edge[2])
          n_correctly_predicted <- n_correctly_predicted + 1
        # if they don't have the same edge type, check to see if
        # they are reversed
      }
    }
  }
  return(n_correctly_predicted)
}
