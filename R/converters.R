#' Turn a causality DAG into a pattern.
#'
#' \code{chickering} converts dags into their markov equivalence class. This
#' function exists mostly to provide documentation on how causality dags are
#' converted to patterns (or pdags). Using \code{as.pattern} instead is
#' recommended.
#' @param graph a causality dag.
#' @return a causality.pattern
#' @details \code{chickering} generates a topological sort of the dag, and
#'     generates an ordering of the edges based off this topological sort. Then,
#'     every edge is marked as 'unkown' and algorithm iterates through the nodes 
#'     acoording to their order. If there is an unshielded collider on a node,
#'     all of its parents are marked as directed.
#' @references
#'    Chickering DM. A transformational characterization of equivalent Bayesian
#'    network structures. InProceedings of the Eleventh conference on
#'    Uncertainty  in artificial intelligence 1995 Aug 18 (pp. 87-98). Morgan
#'    Kaufmann Publishers Inc. \href{http:///arxiv.org/abs/1302.4938}{arXiv:1302.4938 [cs.AI]}.
#' @export
chickering <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a causality.graph.")
    if (!is.dag(graph))
        stop("input is not a causality.dag.")
    return(.chickering(graph))
}

#' @useDynLib causality r_causality_chickering
.chickering <- function(dag)
{
    dag <- .Call("r_causality_chickering", dag)
    class(dag) <- .PATTERN_CLASS
    return(dag)
}

#' @export
pdx <- function(graph)
{
    if (!is.cgraph(graph))
        stop("Input must be a causality.graph!")
    if (!is.pdag(graph) && !is.pattern(graph))
        stop("Input must be a causality.pdag or causality.pattern!")
    return(.pdx(graph))
}

#' @useDynLib causality r_causality_pdx
.pdx <- function(pdag)
{
    pdag <- .Call("r_causality_pdx", pdag)
    if (is.null(pdag)) {
        warning("pdag lacks a DAG extension. Returning NULL")
        return(NULL)
    }
    class(pdag) <- .DAG_CLASS
    return(pdag)
}
