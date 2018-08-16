#' @useDynLib causality ccf_chickering_wrapper




#' @export
chickering <- function(graph) {
  if(!is.cgraph(graph))
    stop("Input must be a causality.graph!")
  if(!is.dag(graph))
    stop("Input must be a causality.dag!")
  return(.chickering(graph))
}


.chickering <- function(dag) {
  dag <- .Call("ccf_chickering_wrapper", dag)
  class(dag) <- .PATTERN_CLASS
  return(dag)
}


pdx <- function(graph) {
  if(!is.cgraph(graph))
    stop("Input must be a causality.graph!")
  if(!is.pdag(graph) && !is.pattern(graph))
    stop("Input must be a causality.pdag or causality.pattern!")
  return(graph)
}

#' @useDynLib causality ccf_pdx_wrapper
.pdx <- function(pdag) {
  pdag <- .Call("ccf_pdx_wrapper", pdag)
  if(is.null(pdag))
    warning("graph lacks a DAG extension. Returning NULL")
  class(pdag) <- .DAG_CLASS
  return(pdag)
}
