#' Determine the percentage of arrows in a graph are in the true graph
#'
#' \code{arrowhead_precision} calculates the arrowhead precison between a graph
#' and an oracle.
#' @param true_graph graph being treated as the truth.
#' @param est_graph The graph being compared to \code{true_graph}
#' @return Length one numeric between 0 and 1. If there are no oriented edges in
#'   \code{est_graph}, \code{NA} is returned. 1 implies that every directed edge
#'   in \code{est_graph} is also in \code{true_graph}, while 0 implies that no
#'   directed ege in \code{est_graph} is in \code{true_graph}
#' @details \code{arrowhead_precision} counts the number of directed edges
#'   (\code{"<--"},\code{"-->"}) in \code{est_graph} and then counts how many
#'   directed edges in \code{est_graph} are also in \code{true_graph}. Then, the
#'   ratio is returned.
#' @examples
#' TODO(arix)
#' @seealso \code{\link{arrowhead_recall}}
#' @references Joseph D. Ramsey: “Scaling up Greedy Causal Search for Continuous
#'   Variables”, 2015; \url{http://arxiv.org/abs/1507.07749} [cs.AI].
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
      }
    }
  }
  return(n_correctly_predicted/n_predicted_arrows)
}

#' Determine how many arrows in graph 1 are in graph2.
#'
#' \code{arrowhead_recall} calculates the arrowhead recall between a graph and an oracle
#' @param true_graph graph being treated as as the truth.
#' @param est_graph The graph being compared to the \code{true_graph}
#' @return Numeric between 0 and 1;
#' \code{arrowhead_recall} returns \code{NA} if there are
#'   no orriented edges (arrows) in \code{true_graph}
#' @details \code{arrowhead_recall} counts the number of directed edges (<--,
#' -->) in \code{true_graph} and then counts how many directed edges in
#' \code{est_graph} are in \code{true_graph}. Then, the ratio is returned. 1
#' implies that every directed edge in \code{true_graph} is also in
#' \code{est_graph}
#' @examples
#' TODO(arix)
#' @seealso arrowhead_precision
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
      }
    }
  }
  return(n_correctly_predicted/n_true_arrows)
}