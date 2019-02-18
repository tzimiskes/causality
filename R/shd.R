#' Structural Hamming Distance
#'
#' \code{shd} calculates the structural hamming distance (SHD) between two
#' patterns
#' @param x A causality.graph
#' @param y A causality.graph \code{x}
#' @return Length one numeric between 0 and \eqn{n^2}, where n is the number of
#'   nodes. A distance of 0 implies that the patterns are the same
#' @details \code{shd} takes in patterns and calculates the
#'   structural hamming distance between them as defined in Tsamardinos et
#'   al(2006). DAGs will also work, as they will be converted into patterns.
#'   Inputing PDAGs will work if the PDAGs admit DAG extensions. If (at least)
#'   one of them does not, an error is thrown.
#' @references Tsamardinos I, Brown LE, Aliferis CF (2006). "The Max-Min
#'   Hill-Climbing Bayesian Network Structure Learning Algorithm". Machine
#'   Learning, 65(1), 31-78.
#' @examples TODO(arix)
#' @author Alexander Rix
#' @seealso Other graph comparison statistics:
#' \code{\link{arrowhead_precision}}, \code{\link{arrowhead_recall}},
#'   \code{\link{adjacency_precision}}, and \code{\link{adjacency_recall}}
#' @export
shd <- function(x, y) {
  if (!is.cgraph(x))
    stop("x must be a causality.graph")
  if (!is.cgraph(y))
    stop("y must be a causality.graph")

  # generate the adjacency list of the children of x
  pat1_children <- list()
  for (i in 1:nrow(x$edges)) {
    pat1_edge <- x$edges[i, ]
    pat1_children[[pat1_edge[1]]][[pat1_edge[2]]] <- pat1_edge[3]
  }
  # now, do the same for y
  pat2_children <- list()
  for (j in 1:nrow(y$edges)) {
    pat2_edge <- y$edges[j, ]
    pat2_children[[pat2_edge[1]]][[pat2_edge[2]]] <- pat2_edge[3]
  }
  distance <- 0
  # loop through the x edges to see if x is missing edges,
  # or misoriented edges.
  for (i in 1:nrow(x$edges)) {
    pat1_edge <- x$edges[i, ]
    node1 <- pat1_edge[1]
    node2 <- pat1_edge[2]
    edge  <- pat1_edge[3]
    pat2_edge <- as.list(pat2_children[[node1]])[[node2]]
    if (is.null(pat2_edge)) {
      if (edge == .DIRECTED) {
        distance <- distance + 1
      }
      # if x_edge is not of type -->, it is --- since
      # (true_src, true_dst, ---) is not in true graph, we ned to check to see
      # if (true_dst, true_src, ---) is
      else {
        pat2_edge <- as.list(pat2_children[[node2]])[[node1]]
        # it isn't
        if (is.null(pat2_edge) || pat2_edge != .UNDIRECTED)
          distance <- distance + 1
      }
      # pat2_edge is not null
      # if the orientations don't match, y_edge in not oriented in
    } else if ( pat2_edge != edge) {
      distance <- distance + 1
    }
  }
  # we only need to see if y_edge is extra in the true patterns
  for (i in 1:nrow(y$edges)) {
    pat2_edge <- y$edges[i, ]
    node1 <- pat2_edge[1]
    node2 <- pat2_edge[2]
    pat1_edge <- as.list(pat1_children[[node1]])[[node2]]
    if (is.null(pat1_edge)) {
      pat1_edge <- as.list(pat1_children[[node2]])[[node1]]
      if (is.null(pat1_edge))
        distance <- distance + 1
    }
  }
  return(distance)
}
