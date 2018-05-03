#' Calculate the Structural Hamming Distance between two PDAGS
#'
#' \code{adjacency_precision} calculates the adjacency precison between a graph
#' and an oracle.
#' @param pdag1 A causality pdag being used as the "true" pdag
#' @param pdag2 A causality pdag that is being compared to \code{pdag1}
#' @return Length one numeric between 0 and \eqn{n^2}, where n is the number of
#'   nodes.
#' @details \code{shd} takes in objects of class pdag and calculates the
#'   structural hamming distance between them as defined in Tsamardinos et
#'   al(2006). Note that patterns are a subset of dpags, so inputing these
#'   objects works as well. If the event the at least 1 of the inputs is a dag,
#'   shd returns an error and states that you need to run \code{dag_to_pattern}
#'   to coerce the dag to a pattern first.
#' @examples
#' TODO(arix)
#' @references Tsamardinos I, Brown LE, Aliferis CF (2006). "The Max-Min
#'   Hill-Climbing Bayesian Network Structure Learning Algorithm". Machine
#'   Learning, 65(1), 31-78.
#' @seealso \code{\link{dag_to_pattern}}
# TODO(arix) redo type checking
shd <- function(pdag1, pdag2) {
  # generate the adjacency list of the children of pdag1
  true_children <- list()
  for (i in 1:nrow(pdag1$edges)) {
    pdag1_edge <- pdag1$edges[i,]
    true_children[[pdag1_edge[1]]][[pdag1_edge[2]]] <- pdag1_edge[3]
  }
  # now, do the same for pdag2
  est_children <- list()
  for (j in 1:nrow(pdag2$edges)) {
    pdag2_edge <- pdag2$edges[j,]
    est_children[[pdag2_edge[1]]][[pdag2_edge[2]]] <- pdag2_edge[3]
  }
  distance <- 0
  # loop through the pdag1 edges to see if pdag1 is missing edges, or misoriented edges.
  for (i in 1:nrow(pdag1$edges)) {
    pdag1_edge <- pdag1$edges[i, ]
    pdag2_edge <- as.list(est_children[[pdag1_edge[1]]])[[pdag1_edge[2]]]
    if (is.null(pdag2_edge)) {
      if (pdag1_edge[3] == "-->") {
        print(pdag1_edge)
        distance <- distance + 1
      }
      # if pdag1_edge is not of type -->, it is --- since (true_src, true_dst,
      # ---) is not in true graph, we ned to check to see if (true_dst,
      # true_src, ---) is
      else {
        pdag2_edge <- as.list(est_children[[pdag1_edge[2]]])[[pdag1_edge[1]]]
        # it isn't
        if (is.null(pdag2_edge) || pdag2_edge != "---") {
          print(pdag1_edge)
          distance <- distance + 1
        }
      }
      # pdag2_edge is not null
      # if the orientations don't match, pdag2_edge in not oriented in
    } else if( pdag2_edge != pdag1_edge[3]) {
      print(pdag1_edge)
      distance <- distance + 1
    }
  }
  # we only need to see if pdag2_edge is extra in the true pdags
  for(i in 1:nrow(pdag2$edges)) {
    pdag2_edge <- pdag2$edges[i, ]
    pdag1_edge <- as.list(true_children[[pdag2_edge[1]]])[[pdag2_edge[2]]]
    if (is.null(pdag1_edge)) {
      pdag1_edge <- as.list(true_children[[pdag2_edge[2]]])[[pdag2_edge[1]]]
      if (is.null(pdag1_edge)) {
        print(pdag2_edge)
        distance <- distance + 1
      }
    }
  }
  return(distance)
}
