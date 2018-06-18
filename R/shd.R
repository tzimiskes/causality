#' Structural Hamming Distance
#'
#' \code{shd} calculates the structural hamming distance (SHD) between two
#' patterns
#' @param pattern1 A causality pattern
#' @param pattern2 A causality pattern that is being compared to \code{pattern1}
#' @return Length one numeric between 0 and \eqn{n^2}, where n is the number of
#'   nodes. A Distance of 0 implies that the patterns are the same
#' @details \code{shd} takes in patterns and calculates the
#'   structural hamming distance between them as defined in Tsamardinos et
#'   al(2006). DAGs will also work, as they will be converted into patterns.
#'   Inputing PDAGs will work if the PDAGs admit DAG extensions. If (at least)
#'   one of them does not, an error is thrown.
#' @references Tsamardinos I, Brown LE, Aliferis CF (2006). "The Max-Min
#'   Hill-Climbing Bayesian Network Structure Learning Algorithm". Machine
#'   Learning, 65(1), 31-78.
#' @examples TODO(arix)
#' @family graph comparison statistics
shd <- function(pattern1, pattern2) {
  if (!is.cgraph(pattern1))
    stop("pattern1 must be a cgraph")
  if (!is.cgraph(pattern2))
    stop("pattern2 must be a cgraph")
  if (!is.pattern(pattern1))
    pattern1 <- as.pattern(pattern1)
  if (!is.pattern(pattern2))
    pattern1 <- as.pattern(pattern2)

  # generate the adjacency list of the children of pattern1
  true_children <- list()
  for (i in 1:nrow(pattern1$edges)) {
    pattern1_edge <- pattern1$edges[i,]
    true_children[[pattern1_edge[1]]][[pattern1_edge[2]]] <- pattern1_edge[3]
  }
  # now, do the same for pattern2
  est_children <- list()
  for (j in 1:nrow(pattern2$edges)) {
    pattern2_edge <- pattern2$edges[j,]
    est_children[[pattern2_edge[1]]][[pattern2_edge[2]]] <- pattern2_edge[3]
  }
  distance <- 0
  # loop through the pattern1 edges to see if pattern1 is missing edges,
  # or misoriented edges.
  for (i in 1:nrow(pattern1$edges)) {
    pattern1_edge <- pattern1$edges[i, ]
    pattern2_edge <- as.list(est_children[[pattern1_edge[1]]])[[pattern1_edge[2]]]
    if (is.null(pattern2_edge)) {
      if (pattern1_edge[3] == "-->") {
        print(pattern1_edge)
        distance <- distance + 1
      }
      # if pattern1_edge is not of type -->, it is --- since (true_src, true_dst,
      # ---) is not in true graph, we ned to check to see if (true_dst,
      # true_src, ---) is
      else {
        pattern2_edge <- as.list(est_children[[pattern1_edge[2]]])[[pattern1_edge[1]]]
        # it isn't
        if (is.null(pattern2_edge) || pattern2_edge != "---")
          distance <- distance + 1
      }
      # pattern2_edge is not null
      # if the orientations don't match, pattern2_edge in not oriented in
    } else if( pattern2_edge != pattern1_edge[3]) {
      distance <- distance + 1
    }
  }
  # we only need to see if pattern2_edge is extra in the true patterns
  for(i in 1:nrow(pattern2$edges)) {
    pattern2_edge <- pattern2$edges[i, ]
    pattern1_edge <- as.list(true_children[[pattern2_edge[1]]])[[pattern2_edge[2]]]
    if (is.null(pattern1_edge)) {
      pattern1_edge <- as.list(true_children[[pattern2_edge[2]]])[[pattern2_edge[1]]]
      if (is.null(pattern1_edge))
        distance <- distance + 1
    }
  }
  return(distance)
}
