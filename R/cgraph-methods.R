# Causality Graph methods ----------------------------------------------------
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
#'     \item
#'       \code{parents} and \code{children} calculates the parents and children
#'       of each node in \code{graph} respectively.
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
is.cyclic <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a cgraph")
    # all these types cannot have cycles
    if (is.dag(graph) || is.pdag(graph) || is.pattern(graph) || is.pag(graph))
        return(FALSE)
    if (is.null(sort.causality.graph(graph)))
        return(TRUE)
    else
        return(FALSE)
}

#' @name cgraph-methods
#' @export
is.acyclic <- function(graph) {
  return(!is.cyclic(graph))
}

#' @name cgraph-methods
#' @export
is.directed <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a cgraph")
    edge_types <- graph$edges[, 3]
    n_edges <- length(edge_types)
    for (i in 1:n_edges) {
        if (!(edge_types[i] %in% .DIRECTED_EDGE_TYPES))
            return(FALSE)
    }
    return(TRUE)
}

#' @rdname cgraph-methods
#' @export
is.nonlatent <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a causality.graph")

    !any((graph$edges[, 3] %in% .LATENT_EDGE_TYPES))
}

#' @rdname cgraph-methods
#' @export
is.latent <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a cgraph")
    if (is.pag(graph))
        return(TRUE)

    any(graph$edges[, 3] %in% .LATENT_EDGE_TYPES)
}

#' @rdname cgraph-methods
#' @export
parents <- function(graph)
{
  parents <- list()
  edges <- graph$edges
  for (i in 1:nrow(edges)) {
    edge <- edges[i, ]
    if (edge[3] %in% .DIRECTED_EDGE_TYPES)
      parents[[edge[2]]] <- c(edge[1], parents[[edge[2]]])
  }
  return(parents)
}

#' @rdname cgraph-methods
#' @export
children <- function(graph)
{
    children <- list()
    edges <- graph$edges
    for (i in 1:nrow(edges)) {
        edge <- edges[i, ]
        if (edge[3] %in% .DIRECTED_EDGE_TYPES)
            children[[edge[1]]] <- c(edge[2], children[[edge[1]]])
    }
    return(children)
}

#' @rdname cgraph-methods
#' @export
is.empty <- function(graph)
{
    if (!is.cgraph(graph))
        stop("'graph' is not a cgraph")

    is.null(graph$edges)
}
