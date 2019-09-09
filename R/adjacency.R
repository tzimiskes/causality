#' Adjacency Precision and Recall
#'
#' \code{adjacency_precision} calculates the adjacency precison between two
#' causality graphs.
#'
#' \code{adjacency_recall} calculates the adjacency recall between two
#' causality graphs.
#' @param x A causality.graph
#' @param y A causality.graph
#' @details TODO(arix) explain what they do
#' @return Both \code{adjacency_precision} and \code{adjacency_recall} return a
#'   numeric between 0 and 1 with the following exceptions:
#'   \itemize{
#'   \item \code{adjacency_precision} returns \code{NA} if \code{y}
#'     contains 0 adjacencies
#'   \item \code{adjacency_recall} returns \code{NA} if \code{x}
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
adjacency_precision <- function(x, y)
{
    if (!is.cgraph(x))
        stop("x is not of type cgraph")
    if (!is.cgraph(y))
        stop("y is not of type cgraph")
    # calculate the number adjacents in y. return NA if there are none
    n.y.adjs <- sum(lengths(y$adjacencies))
    if (n.y.adjs == 0) {
        warning("y has no adjacencies. Returning NA")
        return(NA)
    }
    # calcluate the intersection of adjacencies over y.adjs and return the ratio
    return(adjacency_intersect(x, y) / n.y.adjs)
}

#' @rdname adjacency
#' @export
adjacency_recall <- function(x, y)
{
    if (!is.cgraph(x))
        stop("x is not of type cgraph")
    if (!is.cgraph(y))
        stop("y is not of type cgraph")
    # calculate the number adjacents in x. return NA if there are none
    n.x.adjs <- sum(lengths(x$adjacencies))
    if (n.x.adjs == 0) {
        warning("x has no adjacencies. Returning NA")
        return(NA)
    }
    # calcluate the intersection of adjacencies over x.adjs and return the ratio
    return(adjacency_intersect(x, y) / n.x.adjs)
}

# internal function that is used to cacluate the intersection of the adjacencies
# for each node in x and y
adjacency_intersect <- function(x, y)
{
    n.same <- 0
    # for each node, calculate the intersection of the node's
    # adjacencies in x and y
    for (node in names(x$adjacencies)) {
        # get the size for intersection of the adjacencies of 'node'
        # in est graph and true graph
        n.same <- n.same + length(
            intersect(x$adjacencies[[node]], y$adjacencies[[node]]))
    }
    return(n.same)
}
