#' Arrowhead Precision and Recall
#'
#' \code{arrowhead_precision} calculates the arrowhead precision between two
#' causality graphs
#' @param x A causality PDAG
#' @param y A causality PDAG
#' @details \code{arrowhead_precision} counts the number of directed edges
#'   (\code{"-->"}) in \code{x} and then counts how many directed edges in
#'   \code{x} are also in \code{y}. Then, the ratio is returned.
#' @return Length one numeric between 0 and 1.
#'
#'   \code{arrowhead_precision} returns \code{NA} if there are no oriented
#'   edges in \code{y}.
#' @examples
#' TODO(arix)
#' @references
#' Joseph D. Ramsey: “Scaling up Greedy Causal Search for Continuous Variables”,
#'  2015; \href{http://arxiv.org/abs/1507.07749}{arxiv:1507.07749[cs.AI]}.
#'
#'   Spirtes et al. “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#' @author Alexander Rix
#' @seealso Other graph comparison statistics:
#'   \code{\link{adjacency_precision}}, \code{\link{adjacency_recall}},
#'   and \code{\link{shd}}
#' @export
arrowhead_precision <- function(x, y) {
  if (!is.cgraph(x))
    stop("x must be a causality.graph")
  if (!is.cgraph(y))
    stop("y must be a causality.graph")
  n_y_arrows <- 0
  for (i in 1:nrow(y$edges)) {
    edges <- y$edges
    if (edges[i, 3] == .DIRECTED)
      n_y_arrows <- n_y_arrows + 1
  }
  if (n_y_arrows == 0) {
    warning("y contains no oriented edges. Returning NA")
    return(NA)
  }
  return(arrow_intersect(x, y) / n_y_arrows)
}
#' Determine how many arrows in graph 1 are in graph2.
#'
#' \code{arrowhead_recall} calculates the arrowhead recall between two
#' causality graphs
#' @return \code{arrowhead_recall} returns \code{NA} if there are
#'   no oriented edges (arrows) in \code{x}
#' @details \code{arrowhead_recall} counts the number of directed edges
#'   \code{x} and then counts how many directed edges in
#'   \code{y} are in \code{x}. Then, the ratio is returned. 1
#'   implies that every directed edge in \code{x} are also in \code{y}.
#' @rdname arrowhead_precision
#' @export
arrowhead_recall <- function(x, y) {
  if (!is.cgraph(x))
    stop("x must be a causality graph")
  if (!is.cgraph(y))
    stop("y must be a causality graph")
  n_x_arrows <- 0
  edges <- x$edges
  for (i in 1:nrow(edges)) {
    if (edges[i, 3] == .DIRECTED)
      n_x_arrows <- n_x_arrows + 1
  }
  if (n_x_arrows == 0) {
    warning("x contains no oriented edges. Returning NA")
    return(NA)
  }
  return(arrow_intersect(x, y) / n_x_arrows)
}

arrow_intersect <- function(x, y) {
  n_same <- 0
  # index over the edges in the x graph and estimated graph. Recall that an
  # edge is a vector that consists of (origin, destination, edge_type) eg
  # ("X1", "X2", "-->")
  x_edges <- x$edges
  y_edges <- y$edges
  for (i in 1:nrow(x_edges)) {
    x_edge <- x_edges[i, ]
    # if the edge in unoriented, skip
    if (x_edge[3] == .UNDIRECTED)
      next
    for (j in 1:nrow(y_edges)) {
      y_edge <- y_edges[j, ]
      # if the edge is unoriented, skip
      if (y_edge[3] == .UNDIRECTED)
        next
      if (y_edge[1] == x_edge[1] && y_edge[2] == x_edge[2])
        n_same <- n_same + 1
    }
  }
  return(n_same)
}
