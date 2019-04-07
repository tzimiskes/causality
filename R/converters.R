# converters.R contains interfaces to causality c functions to help convert the
# different cgraph subclasses into each other
# Author: Alexander Rix


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
