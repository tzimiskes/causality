# pattern.R  conatins the implementation for causality.patterns
# Author: Alexander Rix (arix@umn.edu)

#' Causality Patterns
#'
#' Create, test, or manipulate objects of type "causality.pattern"
#' @param edges A \eqn{m x 3} character matrix. Each row is an edge in the form
#'   of (node1, node2, edgetype), with node1 and node2 being in nodes. Valid
#'   edge types are listed below
#' @param validate logical value to determine whether or not to check to see
#'   if the graph is valid before returning it. Default is \code{TRUE}
#' @param graph A graph to coerced or tested
#' @details TODO
#' @return \code{dag} returns object of class "causality.pattern", or an error
#'   if the graph is invalid (assuming \code{validate = TRUE}).
#' @author Alexander Rix
#' @examples TODO
#' @references
#'   Spirtes et al. “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#'
#'  Spirtes P. Introduction to causal inference.
#'  Journal of Machine Learning Research. 2010;11(May):1643-62.
#'
#'   Pearl, Judea. Causality. Cambridge university press, 2009.
#'
#'   The algorithm used to convert dags to patterns is used by Chickering(1995)
#'
#'   Chickering DM. A transformational characterization of equivalent Bayesian
#'   network structures. InProceedings of the Eleventh conference on
#'   Uncertainty in artificial intelligence 1995 Aug 18 (pp. 87-98).
#'   Morgan Kaufmann Publishers Inc..
#'
#'   The algorithm by Dor and Tarsi is used to construct patterns from pdags
#'
#'   Dor D, Tarsi M. A simple algorithm to construct a consistent extension of
#'   a partially oriented graph. Technicial Report R-185,
#'   Cognitive Systems Laboratory, UCLA. 1992 Oct 23.
#' @seealso
#' Other causality classes: \code{\link{cgraph}}, \code{\link{dag}}
#' @export
pattern <- function(nodes, edges, validate = TRUE) {
  if (!is.logical(validate))
    stop("validate must take on a logical value")
  graph <- cgraph(nodes, edges, validate)
  if (validate) {
    if (!is_valid_pattern(graph))
      stop("Input is not a valid causality dag")
  }
  class(graph) <- .PATTERN_CLASS
  return(graph)
}

#' @details \code{is_valid_pattern} checks to see if the input is a valid
#'   "causality.pattern". Specifically, it checks that the \code{graph} is
#'   nonlatent acyclic graph that is invarient under the \code{\link{meek}}
#'   rules (ie that it is a Completed PDAG -- AKA a pattern).
#' @usage is_valid_pattern(graph)
#' @rdname pattern
#' @return \code{is_valid_pattern} returns \code{TRUE} or \code{FALSE} depending
#'   on whether or not the input is a valid "causality.pattern".
#' @export
is_valid_pattern <- function(graph) {
  if (!is.cgraph(graph))
    stop("Input must be a causality graph!")
  if (!is.nonlatent(graph)) {
    warning("graph contains nonlatent edge types")
    return(FALSE)
  }
  else if (is.cyclic(graph)) {
    warning("graph is cylic")
    return(FALSE)
  }
  else if(isTRUE(all.equal(graph$edges, meek(graph)$edges))) {
    return(TRUE)
  }
  else {
    warning("graph is not invariant under the meek rules,
             so it is a pdag not a pattern")
    return(FALSE)
  }
}

#' @usage is.pattern(graph)
#' @details \code{is.pattern} tests whether or not an object has the class
#'   "causality.pattern"
#' @return \code{is.pattern} returns \code{TRUE} or \code{FALSE}.
#' @rdname pattern
#' @export
is.pattern <- function(graph) {
  if (isTRUE(all.equal(.PATTERN_CLASS, class(graph))))
    return(TRUE)
  else
    return(FALSE)
}

#' @rdname pattern
#' @export
as.pattern <- function(graph) {
  UseMethod("as.pattern")
}
#' @rdname pattern
#' @export
as.pattern.default <- function(graph) {
  if (is.pattern(graph))
    return(graph)
  if (!is.cgraph(graph))
    stop("input is not a cgraph")
}

#' @rdname pattern
#' @export
as.pattern.causality.dag <- function(graph) {
  if (!is.dag(graph))
    stop("input is not a dag")
  return(.dag_to_pattern(graph))
}

#' @rdname pattern
#' @export
as.pattern.causality.pdag <- function(graph) {
  if (!is.pdag(graph))
    stop("input is not a pdag")
  return(.dag_from_pdag(graph))
}

#' @rdname pattern
#' @export
as.pattern.causality.pag <- function(graph) {
  if (!is.pag(graph))
    stop("Input is not a pag")
  stop("Not Implemented")
}

#' @rdname pattern
#' @export
as.pattern.causality.graph <- function(graph) {
  if (is_valid_pattern(graph)) {
    class(graph) <- .PATTERN_CLASS
    return(graph)
  }
  else if(is.acyclic(graph)) {
    if (is.directed(graph)) {
      return(.dag_to_pattern(graph))
    }
    else if (is.nonlatent(graph)) {
      graph <- .dag_from_pdag(graph)
      if(is.null(graph)) {
        warning("graph is a pdag that doesn't contain a dag extension.")
        warning("Cannot coerce graph to causality.pattern.")
        return(NULL)
      }
    }
    else {
      return(.dag_to_pattern(graph))
    }
  }
  else {
    warning("graph is cyclic.")
    warning("Cannot coerce graph to causality.pattern")
    return(NULL)
  }
}