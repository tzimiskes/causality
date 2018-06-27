#' Adjacency Precision and Recall
#'
#' \code{adjacency_precision} calculates the adjacency precison between two
#' causality graphs.
#'
#' \code{adjacency_recall} calculates the adjacency recall between two
#' causality graphs.
#' @param cgraph1 A causality graph
#' @param cgraph2 A causality graph
#' @details TODO(arix) explain what they do
#' @return Both \code{adjacency_precision} and \code{adjacency_recall} return a
#'   numeric between 0 and 1 with the following exceptions:
#'   \itemize{
#'   \item \code{adjacency_precision} returns \code{NA} if \code{cgraph2}
#'     contains 0 adjacencies
#'   \item \code{adjacency_recall} returns \code{NA} if \code{cgraph1}
#'     contains 0 adjacencies
#'   }
#'
#' @examples
#' TODO(arix)
#' @references
#'   Joseph D. Ramsey: “Scaling up Greedy Causal Search for Continuous Variables
#'   ”, 2015; \href{http://arxiv.org/abs/1507.07749}{arxiv:1507.07749 [cs.AI]}.
#'   TODO(arix) see what Spirtes calls these guys
#'   Spirtes et al: “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#' @author Alexander Rix
#' @seealso Other graph comparison statistics:
#' \code{\link{arrowhead_precision}}, \code{\link{arrowhead_recall}},
#'   and \code{\link{shd}}
#' @name adjacency
#' @aliases NULL
NULL

#' @rdname adjacency
#' @export
adjacency_precision <- function(cgraph1, cgraph2) {
  # type checking
  if (!is.cgraph(cgraph1))
    stop("cgraph1 is not of type cgraph")
  if (!is.cgraph(cgraph2))
    stop("cgraph2 is not of type cgraph")
  # check to make use that the nodes are the same
  if (!isTRUE(all.equal(sort(cgraph1$nodes), sort(cgraph1$nodes))))
    stop("cgraph1 and cgraph2 need to have the same nodes")
  # calculate the number adjacents in cgraph2
  # return NA if there are none
  n_cgraph2_adjs <- sum(lengths(cgraph2$adjacencies))
  if (n_cgraph2_adjs == 0) {
    warning("cgraph2 has no adjacencies. Returning NA")
    return(NA)
  }
  # calcluate the intersection of adjacencies over n_cgraph2_adjs and return the
  # ratio
  return(adjacency_intersect(cgraph1, cgraph2)/n_cgraph2_adjs)
}

#' @rdname adjacency
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
  return(adjacency_intersect(cgraph1, cgraph2)/n_cgraph1_adjs)
}

# internal function that is used to cacluate the intersection of the adjacencies
# for each node in cgraph1 and cgraph2
adjacency_intersect <- function(cgraph1, cgraph2) {
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