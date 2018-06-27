#' Sort the nodes of a acylcic graph
#'
#' \code{sort} calculates the topological ordering of graph, that is sorts it.
#' @param dag causality dag that you wish to calculate the topological ordering
#'   of
#' @param decreasing logical value to determine whether or the not sort should
#'   be in decreasing order. default is \code{FALSE}
#' @return A charcter vector that contains the nodes of the graph ordered
#'   according to their topological order. in the event that \code{graph} is
#'   cyclic, a warning is issued and \code{NA} is returning
#' @details \code{sort.causality.graph} generates a topological ordering of the
#'   given \code{graph} by using a depth first search as described in CLRS.
#'   A topological ordering is DEFINITION NEEDED.
#'
#' @examples
#' TODO(arix)
#' @seealso \code{sort} is used in various \code{causality} functions, including
#' \code{\link{as.dag}}, \code{\link{is.cyclic}}/\code{\link{is.acylic}}, and
#' \code{\link{as.pattern}}
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
    hash[[graph$nodes[[i]]]] <- i - 1
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