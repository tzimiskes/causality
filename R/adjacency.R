#' Adjacency Precision and Recall
#'
#' \code{adjacency_precision} calculates the adjacency precison between two
#' causality graphs.
#' @param cgraph1 A causality graph
#' @param cgraph2 A causality graph
#' @details TODO(arix)
#' @return Numeric between 0 and 1.
#'
#' \code{adjacency_precision} returns \code{NA} if \code{cgprah2} has no
#' adjacencies.
#' @examples
#' TODO(arix)
#' @references
#'   Joseph D. Ramsey: “Scaling up Greedy Causal Search for Continuous Variables
#'   ”, 2015; \href{http://arxiv.org/abs/1507.07749}{arxiv:1507.07749 [cs.AI]}.
#'
#'   Spirtes et al: “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#' @author Alexander Rix
#' @seealso Other graph comparison statistics:
#' \code{\link{arrowhead_precision}}, \code{\link{arrowhead_recall}},
#'   and \code{\link{shd}}
#' @export
adjacency_precision <- function(cgraph1, cgraph2) {
  # type checking
  if (!is.cgraph(cgraph1))
    stop("cgraph1 is not of type cgraph")
  if (!is.cgraph(cgraph2))
    stop("cgraph2 is not of type cgraph")

  # calculate the number adjacents in cgraph2
  # return NA if there are none
  n_cgraph2_adjs <- sum(lengths(cgraph2$adjacencies))
  if (n_cgraph2_adjs == 0) {
    warning("cgraph2 has no adjacencies. Returning NA")
    return(NA)
  }

  # calcluate the intersection of adjacencies over n_cgraph2_adjs and return the
  # ratio
  return(.adjacency_intersect(cgraph1, cgraph2)/n_cgraph2_adjs)
}

#' \code{adjacency_recall} calculates the adjacency recall between two
#' causality graphs.
#' @details TODO(arix)
#' @return Length one numeric between 0 and 1. If every node in \code{true_graph}
#'   is an orphan, \code{NA} is returned. A return value of 1 implies every
#'   adjacency in \code{true_graph} is also in \code{est_graph}
#' @rdname adjacency_precision
#' @export
adjacency_recall <- function(cgraph1, cgraph2) {
  # type checking
  if (!is.cgraph(cgraph1))
    stop("cgraph1 is not of type cgraph")
  if (!is.cgraph(cgraph2))
    stop("cgraph2 is not of type cgraph")

  # calculate the number adjacents in cgraph1
  # return NA if there are none
  n_cgraph1_adjs <- sum(lengths(cgraph1$adjacencies))
  if(n_cgraph1_adjs == 0) {
    warning("cgraph1 has no adjacencies. Returning NA")
    return(NA)
  }

  # calcluate the intersection of adjacencies over n_cgraph1_adjs and return the
  # ratio
  return(.adjacency_intersect(cgraph1, cgraph2)/n_cgraph1_adjs)
}


.adjacency_intersect <- function(cgraph1, cgraph2) {

  n_same <- 0
  # for each node, calculate the intersection of the node's
  # adjacencies in cgraph1 and cgraph2
  for (node in names(cgraph1$adjacencies)) {
    # get the size for intersection of the adjacencies of 'node'
    # in est graph and true graph
    n_same <- n_same + length(
      intersect(cgraph1$adjacencies[[node]], cgraph2$adjacencies[[node]])
    )
  }
  return(n_same)
}