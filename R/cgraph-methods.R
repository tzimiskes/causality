# Causality Graph methodos ----------------------------------------------------
#' Causality graph methods
#'
#' These are miscellaneous functions that can be applied any to causality.graph
#' object.
#' @details
#'   \itemize{
#'     \item
#'       \code{is.cyclic} and \code{is.acyclic} determine whether or not
#'       \code{graph} is cyclic/acyclic. It infers this from the class, or by
#'       using the sort method.
#'     \item
#'       \code{is.directed} determines if \code{graph} is directed, ie,
#'       the only edge type is \code{"-->"}
#'     \item
#'       \code{is.nonlatent} determines whether or not \code{graph}
#'       contains only nonlatent edge types (\code{"-->"}, \code{"---"}).
#'     \item
#'       \code{is.latent} determines whether or not \code{graph} contains
#'       only latent edge types
#'       (\code{"o->"}, \code{"o-o"}, \code{"<->"}, \code{"++>"}, \code{"~~>"}).
#' }
#' @param graph a causality.graph
#' @return
#'   \code{is.cyclic}, \code{is.acyclic}, \code{is.directed}, \code{is.nonlatent},
#'   and \code{is.latent} all return \code{TRUE} or \code{FALSE}
#' @name cgraph-methods
#' @aliases NULL
NULL

#' @name cgraph-methods
#' @export
is.cyclic <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  # all these types cannot have cycles
  if (is.dag(cgraph) | is.pdag(cgraph) | is.pattern(cgraph) | is.pag(cgraph))
    return(FALSE)
  if (is.null(sort.causality.graph(cgraph)))
    return(TRUE)
  else
    return(FALSE)
}
#' @name cgraph-methods
#' @export
is.acyclic <- function(graph) {
  return(!is.cyclic(graph))
}

#' @details \code{is.directed} determines if \code{graph} is directed, ie,
#'   the only edge type is \code{"-->"}
#' @name cgraph-methods
#' @export
is.directed <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  edge_types <- cgraph$edges[, 3]
  n_edges <- length(edge_types)
  for (i in 1:n_edges) {
    if (edge_types[i] != .DIRECTED) { # ie  edge_type != -->
      return(FALSE)
    }
  }
  return(TRUE)
}

#' @rdname cgraph-methods
#' @export
is.nonlatent <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a causality.graph")
  edge_types <- cgraph$edges[, 3]
  n_edges <- length(edge_types)
  for (i in 1:n_edges) {
    if (!(edge_types[i] %in% .NONLATENT_EDGE_TYPES))
      return(FALSE)
  }
  return(TRUE)
}

#' @rdname cgraph-methods
#' @export
is.latent <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if (is.pag(cgraph))
    return(TRUE)
  edge_types <- cgraph$edges[, 3]
  n_edges <- length(edge_types)
  for (i in 1:n_edges) {
    if (!(edge_types[i] %in% .LATENT_EDGE_TYPES))
      return(FALSE)
  }
  return(TRUE)
}
