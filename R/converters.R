#' @useDynLib causality ccf_chickering_wrapper
.dag_to_pattern <- function(dag) {
  dag <-.Call("ccf_chickering_wrapper", dag)
  class(dag) <- .PATTERN_CLASS
  return(dag)
}


.dag_from_pattern <- function(pattern) {
  pattern <- .Call("ccf_pdx_wrapper", pattern)
  class(pattern) <- .DAG_CLASS
  return(pattern)
}

#' @useDynLib causality ccf_pdx_wrapper
.dag_from_pdag <- function(pdag) {
  pdag <- .Call("ccf_pdx_wrapper", dag)
  if(is.null(pdag)) {
    warning("input does not admit a dag extension, returning NULL.")
    return(NULL)
  }
  class(pdag) <- .DAG_CLASS
  return(pdag)
}

.pattern_from_pdag <- function(pdag) {
  dag <- .pick_dag_from_pdag(pdag)
  if(is.null(dag)) {
    warning("Cannot convert pdag to pattern")
    return(dag)
  }
  return(.dag_to_pattern(pdag))
}
