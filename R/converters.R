#' @export
chickering <- function(graph) {
  if (!is.cgraph(graph))
    stop("Input must be a causality.graph!")
  if (!is.dag(graph))
    stop("Input must be a causality.dag!")
  return(.chickering(graph))
}

#' @useDynLib causality causalityChickering
.chickering <- function(dag) {
  dag <- .Call("causalityChickering", dag)
  class(dag) <- .PATTERN_CLASS
  return(dag)
}

#' @export
pdx <- function(graph) {
  if (!is.cgraph(graph))
    stop("Input must be a causality.graph!")
  if (!is.pdag(graph) && !is.pattern(graph))
    stop("Input must be a causality.pdag or causality.pattern!")
  return(.pdx(graph))
}

#' @useDynLib causality causalityPDX
.pdx <- function(pdag) {
  pdag <- .Call("causalityPDX", pdag)
  if (is.null(pdag)) {
    warning("graph lacks a DAG extension. Returning NULL")
    return(NULL)
  }
  class(pdag) <- .DAG_CLASS
  return(pdag)
}
