#' Arrowhead Precision and Recall
#'
#' \code{arrowhead_precision} calculates the arrowhead precision between two
#' causality graphs
#' @param pdag1 A causality PDAG
#' @param pdag2 A causality PDAG
#' @details \code{arrowhead_precision} counts the number of directed edges
#'   (\code{"-->"}) in \code{pdag1} and then counts how many directed edges in
#'   \code{pdag1} are also in \code{pdag2}. Then, the ratio is returned.
#' @return Length one numeric between 0 and 1.
#'
#'   \code{arrowhead_precision} returns \code{NA} if there are no oriented
#'   edges in \code{pdag2}.
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
arrowhead_precision <- function(pdag1, pdag2) {
  if (!is.cgraph(pdag1))
    stop("pdag1 must be a causality graph")
  if (!is.cgraph(pdag2))
    stop("pdag2 must be a causality graph")
  if (!(is.pdag(pdag1) || is.dag(pdag1) || is.pattern(pdag1)))
    stop("pdag1 must be either a DAG, PDAG, or pattern!")
  if (!(is.pdag(pdag2) || is.dag(pdag2) || is.pattern(pdag2)))
    stop("pdag2 must be either a DAG, PDAG, or pattern!")
  n_pdag2_arrows <- 0
  for (i in 1:nrow(pdag2$edges)) {
    edges <- pdag2$edges
    if (edges[i, 3] == .DIRECTED)
      n_pdag2_arrows <- n_pdag2_arrows + 1
  }
  if (n_pdag2_arrows == 0) {
    warning("pdag2 contains no oriented edges. Returning NA")
    return(NA)
  }
  return(arrow_intersect(pdag1, pdag2) / n_pdag2_arrows)
}
#' Determine how many arrows in graph 1 are in graph2.
#'
#' \code{arrowhead_recall} calculates the arrowhead recall between two
#' causality graphs
#' @return \code{arrowhead_recall} returns \code{NA} if there are
#'   no oriented edges (arrows) in \code{pdag1}
#' @details \code{arrowhead_recall} counts the number of directed edges
#'   \code{pdag1} and then counts how many directed edges in
#'   \code{pdag2} are in \code{pdag1}. Then, the ratio is returned. 1
#'   implies that every directed edge in \code{pdag1} are also in \code{pdag2}.
#' @rdname arrowhead_precision
#' @export
arrowhead_recall <- function(pdag1, pdag2) {
  if (!is.cgraph(pdag1))
    stop("pdag1 must be a causality graph")
  if (!is.cgraph(pdag2))
    stop("pdag2 must be a causality graph")
  if (!(is.pdag(pdag1) || is.dag(pdag1) || is.pattern(pdag1)))
    stop("pdag1 must be either a DAG, PDAG, or pattern!")
  if (!(is.pdag(pdag2) || is.dag(pdag2) || is.pattern(pdag2)))
    stop("pdag2 must be either a DAG, PDAG, or pattern!")
  n_pdag1_arrows <- 0
  edges <- pdag1$edges
  for (i in 1:nrow(edges)) {
    if (edges[i, 3] == .DIRECTED)
      n_pdag1_arrows <- n_pdag1_arrows + 1
  }
  if (n_pdag1_arrows == 0) {
    warning("pdag1 contains no oriented edges. Returning NA")
    return(NA)
  }
  return(arrow_intersect(pdag1, pdag2) / n_pdag1_arrows)
}

arrow_intersect <- function(pdag1, pdag2) {
  n_same <- 0
  # index over the edges in the pdag1 graph and estimated graph. Recall that an
  # edge is a vector that consists of (origin, destination, edge_type) eg
  # ("X1", "X2", "-->")
  pdag1_edges <- pdag1$edges
  pdag2_edges <- pdag2$edges
  for (i in 1:nrow(pdag1_edges)) {
    pdag1_edge <- pdag1_edges[i, ]
    # if the edge in unoriented, skip
    if (pdag1_edge[3] == .UNDIRECTED)
      next
    for (j in 1:nrow(pdag2_edges)) {
      pdag2_edge <- pdag2_edges[j, ]
      # if the edge is unoriented, skip
      if (pdag2_edge[3] == .UNDIRECTED)
        next
      if (pdag2_edge[1] == pdag1_edge[1] && pdag2_edge[2] == pdag1_edge[2])
        n_same <- n_same + 1
    }
  }
  return(n_same)
}
