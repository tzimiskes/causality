# dag.R  conatins the implementation for causality.dags
# Author: Alexander Rix (arix@umn.edu)

#' Causality DAGS
#'
#' Create, test, or manipulate objects of type "causality.dag"
#' @param nodes A character array of node names
#' @param edges A \eqn{m x 3} character matrix. Each row is an edge in the form
#'   of (node1, node2, edgetype), with node1 and node2 being in nodes. Valid
#'   edge types are listed below
#' @param validate logical value to determine whether or not to check to see
#'   if the graph is valid before returning it. Default is \code{TRUE}
#' @param graph A graph to coerced or tested
#' @details A causality DAG is a causality graph that is directed and acylic
#'   (hence the name DAG). DAGs are typically used to represent Bayesisan
#'   Networks and Structural Equation Models (SEMs).
#' @return \code{dag} returns object of class "causality.dag", or an error
#'   if the graph is invalid (assuming \code{validate = TRUE}).
#' @author Alexander Rix
#' @examples
#' d <- dag(c("X1", "X2", "X3"), c("X1", "X2", "-->",
#'                                 "X2", "X3", "-->"))
#' d <- dag(c("X1", "X2", "X3"), c("X1", "X2", "-->",
#'                                 "X2", "X3", "-->",
#'                                 "X3", "X1", "-->"))
#' @references
#'   Spirtes et al. “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#'
#'  Spirtes P. Introduction to causal inference.
#'  Journal of Machine Learning Research. 2010;11(May):1643-62.
#'
#'   Pearl, Judea. Causality. Cambridge university press, 2009.
#' @seealso
#' Other causality classes: \code{\link{cgraph}}, \code{\link{pattern}}
#' @export
dag <- function(nodes, edges, validate = TRUE)
{
    if (!is.logical(validate))
        stop("validate must take on a logical value")
    graph <- cgraph(nodes, edges, validate)
    if (validate && !is_valid_dag(graph))
        stop("Input is not a valid causality dag")
    class(graph) <- .DAG_CLASS
    return(graph)
}

#' @details \code{is_valid_dag} checks to see if the input is a valid
#'   "causality.dag". Specifically, it checks that the \code{graph} is directed
#'   and acyclic.
#' @usage is_valid_dag(graph)
#' @rdname dag
#' @return \code{is_valid_dag} returns \code{TRUE} or \code{FALSE} depending
#'   on whether or not the input is a valid "causality.dag".
#' @export
is_valid_dag <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a causality graph.")
    if (is.directed(graph) && is.acyclic(graph))
        return(TRUE)
    else if (!is.directed(graph)) {
        warning("graph is not directed.")
        return(FALSE)
    }
    warning("graph is cyclic.")
    return(FALSE)
}

#' @usage is.dag(graph)
#' @details \code{is.dag} tests whether or not an object has the class
#'   "causality.dag"
#' @return \code{is.dag} returns \code{TRUE} or \code{FALSE}.
#' @rdname dag
#' @export
is.dag <- function(graph)
{
    if (isTRUE(all.equal(.DAG_CLASS, class(graph))))
        return(TRUE)
    else
        return(FALSE)
}

#' @rdname dag
#' @export
as.dag <- function(graph)
{
  UseMethod("as.dag")
}

#' @rdname dag
#' @export
as.dag.default <- function(graph)
{
    if (is.dag(graph))
        return(graph)
    if (!is.cgraph(cgraph))
        stop("input is not a cgraph")
}

#' @rdname dag
#' @export
as.dag.causality.graph <- function(graph)
{
    if (!is.cgraph(graph))
        stop("input is not a cgraph")
    if (!is.nonlatent(graph))
        stop("not implemented")
    directed <- is.directed(graph)
    if (directed && is.acyclic(graph)) {
        class(graph) <- .DAG_CLASS
        return(graph)
    }
    if (!directed)
        stop("graph is cyclic. Cannot coerce graph to causality.dag.")
    # pdag / pattern
    dag <- .pdx(graph)
    if (is.null(dag))
        stop("cannot coerce input to causality.dag")
    return(dag)
}

#' @rdname dag
#' @export
as.dag.causality.pdag <- function(graph)
{
    if (!is.pdag(graph))
        stop("input is not a causality.graph")
    dag <- .pdx(graph)
    if (is.null(dag))
        warning("Unable to coerce input to causality.dag")
    return(dag)
}

#' @rdname dag
#' @export
as.dag.causality.pattern <- function(graph)
{
    if (!is.pattern(graph))
        stop("input is not a causality.pattern")
    return(.pdx(graph))
}

#' @rdname dag
#' @export
as.dag.causality.pag <- function(graph)
{
    if (!is.pag(graph))
        stop("input is not a causality.pag")
    stop("Not implemented")
}
