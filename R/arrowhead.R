#' Determine how many arrows in graph 1 are in graph2.
#'
#' \code{test_fisher_independence} returns whether or not the \code{z_score} implies independence at the given signifigence level \code{alpha}
#' @param true_graph Fisher z-score from (ideally) \code{fisher_z_score}, or elsewhere
#' @param est_graph The signifigence level of the test, i.e., .05, .01, .001 etc
#' @return A number between 0 and 1.
#' \code{arrowhead_precision} returns \code{NA} if there are no orriented edges (arrow) in \code{est_graph}
#' @details
#' The test is H0: x, y are independent and H1: x,y are dependent.
#' @examples
#' test_fisher_independence(5, .05)
#' test_fisher_independence(100,.001)
#' @seealso fisher_z_score
arrowhead_precision <- function(true_graph, est_graph) {
  # if (class(true_graph) != "cgraph" || class(est_graph) != "cgraph")
  #   stop("at least of the graphs are not the correct type!")
  # if (!setequal(true_graph$names,est_graph$names))
  #   stop ("names of the nodes do not match!")
  n_predicted_arrows <- 0
  for (i in 1:length(est_graph$edges[, 3])) {
    edge_type <- est_graph$edges[i, 3]
    if (edge_type == "-->" || edge_type == "<--")
      n_predicted_arrows <- n_predicted_arrows + 1
  }
  if(n_predicted_arrows == 0) {
    warning("est_graph contains no oriented edges. Returning NA")
    return(NA)
  }

  n_correctly_predicted <- 0
  for (i in 1:length(est_graph$edges[, 1])) {
    eg_edge <- est_graph$edges[i, ]
    # if the edge in unoriented, skip
    if (eg_edge[3] == "---")
      next
    if (eg_edge[3] == "<->") {
      warning(sprintf("edge %i in est_graph is a bidirected edge.
                      Treating the edge as ---", i))
      next
    }
    for (j in 1:length(true_graph$edges[, 1])) {
      tg_edge <- true_graph$edges[j, ]
      # if the edge is unoriented, skip
      if (tg_edge[3] == "---")
        next
      if (tg_edge[3] == "<->") {
        warning(sprintf("Edge %i in true_graph is a bidirected edge.
                        Treating the edge as unidirected", i))
        next
      }
      # check to see if the edges have the same edge type,
      # and if the eg_edge is unoriented
      if (eg_edge[3] == tg_edge[3]) {
        # if they do have the same edge type, check to see if they have the same
        # origin and destination
        if (eg_edge[1] == tg_edge[1] && eg_edge[2] == tg_edge[2])
          n_correctly_predicted <- n_correctly_predicted + 1
        # if they don't have the same edge type, check to see if
        # they are reversed
      } else if ( (eg_edge[3] == "<--" && tg_edge[3] == "--> ") ||
                  (eg_edge[3] == "-->" && tg_edge[3] == "<--") ) {
        if (eg_edge[1] == tg_edge[2] && eg_edge[2] == tg_edge[1])
          n_correctly_predicted <- n_correctly_predicted + 1
        # now, check to see if the eg_edge is a double sided edge
      }
    }
  }
  return(n_correctly_predicted/n_predicted_arrows)
}

arrowhead_recall <- function(true_graph, est_graph) {
  #TODO(arix) implement type checking

  # calculate the number of arrow in the true graph
  n_true_arrows <- 0
  for (i in 1:length(true_graph$edges[, 3])) {
    edge_type <- true_graph$edges[i, 3]
    if (edge_type == "-->" || edge_type == "<--")
      n_true_arrows <- n_true_arrows + 1
  }
  if(n_true_arrows == 0) {
    warning("true_graph contains no oriented edges. Returning NA")
    return(NA)
  }
  n_correctly_predicted <- 0
  # index over the edges in the true graph and estimated graph. Recall that an
  # edge is a vector that consists of (origin, destination, edge_type) eg
  # ("X1", "X2", "-->")
  for (i in 1:length(est_graph$edges[, 1])) {
    eg_edge <- est_graph$edges[i, ]
    # if the edge in unoriented, skip
    if (eg_edge[3] == "---")
      next
    if (eg_edge[3] == "<->") {
      warning(sprintf("Edge %i in est_graph is a bidirected edge.
                      Treating the edge as unidirected", i))
      next
    }
    for (j in 1:length(true_graph$edges[, 1])) {
      tg_edge <- true_graph$edges[j, ]
      # if the edge is unoriented, skip
      if (tg_edge[3] == "---")
        next
      if (tg_edge[3] == "<->") {
        warning(sprintf("Edge %i in true_graph is a bidirected edge.
                        Treating the edge as unidirected", i))
        next
      }
      # check to see if the edges have the same edge type,
      # and if the eg_edge is unoriented
      if (eg_edge[3] == tg_edge[3]) {
        # if they do have the same edge type, check to see if they have the same
        # origin and destination
        if (eg_edge[1] == tg_edge[1] && eg_edge[2] == tg_edge[2])
          n_correctly_predicted <- n_correctly_predicted + 1
        # if they don't have the same edge type, check to see if
        # they are reversed
      } else if ( (eg_edge[3] == "<--" && tg_edge[3] == "--> ") ||
                  (eg_edge[3] == "-->" && tg_edge[3] == "<--") ) {
        if (eg_edge[1] == tg_edge[2] && eg_edge[2] == tg_edge[1])
          n_correctly_predicted <- n_correctly_predicted + 1
        # now, check to see if the eg_edge is a double sided edge
      }
    }
  }
  return(n_correctly_predicted/n_true_arrows)
}