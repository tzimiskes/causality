adjacency_precision <- function(true_graph, est_graph) {
  # if (class(true_graph) != "cgraph" || class(est_graph) != "cgraph")
  #   stop("at least of the graphs are not the correct type!")

  # get the number of predicted adjacencies
  n_pred_adjs <- sum(lengths(est_graph$skeleton))
  n_correct <- 0
  # for each node, calculate the intersection true graph neighborhood and the estimated graph neighborhood
  # the cardinality is the number correctly predicted for that node
  for (node in names(true_graph$skeleton)) {
    # get the size for intersection of the adjacencies of 'node'
    # in est graph and true graph
    n_correct <- n_correct + length(
      intersect(true_graph$skeleton[[node]], est_graph$skeleton[[node]])
    )
  }
  return(n_correct/n_pred_adjs)
}

adjacency_recall <- function(true_graph, est_graph) {
  # if (class(true_graph) != "cgraph" || class(est_graph) != "cgraph")
  #   stop("at least of the graphs are not the correct type!")
  # if (!setequal(true_graph$names,est_graph$names))
  #   stop ("the nodes do not match!")
  n_true_adjs <- sum(lengths(true_graph$skeleton))
  n_correct <- 0
  # for each node, calculate the intersection true graph neighborhood and the estimated graph neighborhood
  # the cardinality is the number correctly predicted for that node
  for (node in names(true_graph$skeleton)) {
    n_correct <- n_correct + length(
      intersect(true_graph$skeleton[[node]], est_graph$skeleton[[node]])
    )
  }
  return(n_correct/n_true_adjs)
}