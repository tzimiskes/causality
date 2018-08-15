#' Sort the nodes of a acylcic graph
#'
#' \code{sort} calculates the topological ordering of graph, that is sorts it.
#' @param x causality graph that you wish to calculate the topological
#' ordering of
#' @param decreasing logical value to determine whether or the not sort should
#'   be in decreasing order. default is \code{FALSE}
#' @param ... additional parameters that can be sent through sort generic
#' @return A charcter vector that contains the nodes of the graph ordered
#'   according to their topological order. In the event that \code{graph} is
#'   cyclic, a warning is issued and \code{NULL} is returned.
#' @details \code{sort.causality.graph} generates a topological ordering of the
#'   given \code{graph} by using a depth first search as described in CLRS.
#'   A topological sort (or ordering) is a linear ordering of nodes in a PDAG.
#'   That is if we have \eqn{x --> y}, then x < y in the sort.
#'   This ordering is not unique; a PDAG may have multiple valid orderings.
#' @examples
#' sort(sachs.dag)
#'
#' # sort also works with pdags/patterns
#' sort(as.pattern(sachs.dag))
#' @seealso \code{sort} is used in various \code{causality} functions, including
#' \code{\link{as.dag}}, \code{\link{is.cyclic}}/\code{\link{is.acyclic}}, and
#' \code{\link{as.pattern}}
#' @references Cormen, Thomas H., et al. Introduction to Algorithms. The MIT
#'   Press, 2014.
#' @useDynLib causality ccf_sort_wrapper
#' @export
sort.causality.graph <- function(x, decreasing = FALSE, ...) {
  if (!is.cgraph(x))
    stop("x must be a causality.graph")
  output <- .Call("ccf_sort_wrapper", x)
  if (is.null(output)) {
    warning("x contains a cycle, returning NULL.")
    return(NULL)
  }
  if (decreasing == TRUE)
    return(rev(output))
  else
    return(output)
}
