#' Partially Directed Acyclic Graphs
#' @name pdag
#' @export
pdag <- function(nodes, edges, validate = TRUE)
{
    if (!is.logical(validate))
        stop("validate must take on a logical value.")
    graph <- cgraph(nodes, edges, validate)
    if (validate) {
        if (!is_valid_pdag(graph))
            stop("input is not a valid causality.pdag.")
    }
    class(graph) <- .PDAG_CLASS
    return(graph)
}

#' @details \code{is_valid_pdag} checks to see if the input is a valid
#'   "causality.pdag". Specifically, it checks that the \code{graph} is
#'   nonlatent and acyclic.
#' @usage is_valid_pdag(graph)
#' @rdname pdag
#' @return \code{is_valid_pdag} returns \code{TRUE} or \code{FALSE} depending
#'   on whether or not the input is a valid "causality.pdag".
#' @export
is_valid_pdag <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a causality graph.")
    if (is.nonlatent(graph) && is.acyclic(graph))
        return(TRUE)
    else if (!is.nonlatent(graph))
        warning("graph contains latent edges")
    else
        warning("graph is not acyclic")
    return(FALSE)
}

#' @export
is.pdag <-function(cgraph)
{
    if (isTRUE(all.equal(.PDAG_CLASS, class(cgraph))))
        return(TRUE)
    else
        return(FALSE)
}

#' @export
as.pdag <- function(cgraph) {
    if (!is.cgraph(cgraph))
        stop("input is not a causality graph.")
    if (is.pdag(cgraph))
        return(cgraph)
    if (is.nonlatent(cgraph)) {
        if (!is.cyclic(cgraph)) {
            class(cgraph) <- .PDAG_CLASS
            return(cgraph)
        }
    }
    else
        stop("input contains a cycle, so it cannot be coerced to pdag.")
}

as.pdag.causality.dag <- function(cgraph)
{
    if(!is.dag(cgraph))
        stop("input is not a causality dag.")
    class(cgraph) = .PDAG_CLASS
    return(cgraph)
}

as.pdag.causality.pattern <- function(cgraph)
{
    if (!is.pattern(cgraph))
        stop("Input is not a causality pattern.")
    class(cgraph) = .PDAG_CLASS
    return(cgraph)
}

as.pdag.causality.pag <- function(cgraph)
{
    if (!is.pag(cgraph))
        stop("input is not a causality pag.")
    stop("not implemented")
}
