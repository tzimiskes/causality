#' Get the topological ordering of a dag
#'
#' \code{topological_order} calculates the topological ordering of dag.
#' @param dag causality dag that you wish to calculate the topological ordering
#'   of
#' @return A charcter vector that contains the nodes of the dag ordered
#'   according to their topological order. In the event that \code{dag} contains
#'   a cycle (ie isn't actually a dag) a error is thrown and NA is teturned
#'
#' @details \code{topological_sort} generates a topological ordering of the
#'   given dag by using a depth first search as described in CLRS. The
#'   underlying C implementation of this function is used in the function
#'   \code{\link{dag_to_pattern}} and a slightly different version is used in
#'   \code{\link{as.dag}}: see below.
#' @examples
#' TODO(arix)
#' @note This function is designed to get the topological order from a dag, not
#'   determine whether or not a function is a dag. If you wish to test whether
#'   or not a cgraph object is a dag or not, use the function
#'   \code{\link{as.dag}} instead. It performs additional checking on top of
#'   performing a topological ordering.
#' @seealso   \code{\link{as.dag}} \code{\link{dag_to_pattern}}
#' @references Cormen, Thomas H., et al. Introduction to Algorithms. The MIT
#'   Press, 2014.
#' @useDynLib causality cf_topological_sort
#' @export
sort.causality.graph <- function(graph, decreasing = FALSE) {
  if (!is.cgraph(graph))
    stop("graph must be a causality.graph")

  # creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(graph$nodes))
    hash[[dag$nodes[[i]]]] <- i - 1
  for (i in 1:nrow(graph$edges)) {
    graph$edges[i, 1] <- hash[[graph$edges[i, 1]]]
    graph$edges[i, 2] <- hash[[graph$edges[i, 2]]]
    graph$edges[i, 3] <- 1
  }
  nc <- ncol(graph$edges)
  nr <- nrow(graph$edges)
  graph$edges <- as.integer(graph$edges)
  dim(graph$edges) <- c(nr, nc)
  tmp<-tryCatch(.Call("cf_topological_sort", graph), error = function(e) NA)
  return(graph$nodes[tmp + 1])
}