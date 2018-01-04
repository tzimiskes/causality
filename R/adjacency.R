#' Determine the percentage of adjacencies in a graph are in the true graph
#'
#' \code{adjacency_precision} calculates the adjacency precison between a graph
#' and an oracle.
#' @param true_graph graph being treated as the truth.
#' @param est_graph The graph being compared to \code{true_graph}
#' @return Length one numeric between 0 and 1. If every node in \code{est_graph}
#'   is an orphan, \code{NA} is returned. A return value of 1 implies every
#'   adjacency in \code{est_graph} is also in \code{true_graph}
#'
#' @details First, \code{adjacency_precision} caclulates the total number of
#'   adjacencies in \code{est_graph}. Then, for each vertex, \eqn{V} in
#'   \code{est_graph}, \eqn{EG}, \code{adjacency_precision} calculates the size
#'   of  the itersection of \eqn{Adj(V, EG)} and  \eqn{Adj(V, TG)}. After that,
#'   the results for each vertex is summed and the ratio to number of
#'   adjacencies in \code{est_graph} is returned.
#' @examples
#' TODO(arix)
#' @seealso   \code{\link{adjacency_recall}} \code{\link{arrowhead_precision}} \code{\link{arrowhead_recall}}
#' @references Joseph D. Ramsey: “Scaling up Greedy Causal Search for Continuous
#'   Variables”, 2015; \href{http://arxiv.org/abs/1507.07749}{arxiv:1507.07749 [cs.AI]}.
adjacency_precision <- function(true_graph, est_graph) {
  # if (class(true_graph) != "cgraph" || class(est_graph) != "cgraph")
  #   stop("at least of the graphs are not the correct type!")

  # get the number of predicted adjacencies
  n_pred_adjs <- sum(lengths(est_graph$skeleton))
  if(n_pred_adjs == 0) {
    warning("est_graph has no adjacencies")
    return(NA)
  }
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
#' Determine the percentage of adjacencies in a true graph are in a graph
#'
#' \code{adjacency_recall} calculates the adjacency precison between a graph
#' and an oracle.
#' @param true_graph graph being treated as the truth.
#' @param est_graph The graph being compared to \code{true_graph}
#' @return Length one numeric between 0 and 1. If every node in \code{true_graph}
#'   is an orphan, \code{NA} is returned. A return value of 1 implies every
#'   adjacency in \code{true_graph} is also in \code{est_graph}
#'
#' @details First, \code{adjacency_precision} caclulates the total number of
#'   adjacencies in \code{true_graph}. Then, for each vertex, \eqn{V} in
#'   \code{est_graph}, \eqn{EG}, \code{adjacency_precision} calculates the size
#'   of  the itersection of \eqn{Adj(V, EG)} and  \eqn{Adj(V, TG)}. After that,
#'   the results for each vertex is summed and the ratio to number of
#'   adjacencies in \code{true_graph} is returned.
#' @examples
#' TODO(arix)
#' @seealso   \code{\link{adjacency_precision}} \code{\link{arrowhead_precision}} \code{\link{arrowhead_recall}}
#' @references Joseph D. Ramsey: “Scaling up Greedy Causal Search for Continuous
#'   Variables”, 2015; \href{http://arxiv.org/abs/1507.07749}{arxiv:1507.07749 [cs.AI]}.
adjacency_recall <- function(true_graph, est_graph) {
  # if (class(true_graph) != "cgraph" || class(est_graph) != "cgraph")
  #   stop("at least of the graphs are not the correct type!")
  # if (!setequal(true_graph$names,est_graph$names))
  #   stop ("the nodes do not match!")
  n_true_adjs <- sum(lengths(true_graph$skeleton))
  if(n_pred_adjs == 0) {
    warning("true_graph has no adjacencies")
    return(NA)
  }
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