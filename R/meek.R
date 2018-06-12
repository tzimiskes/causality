meek <- function(pdag) {
  if(!is.cgraph(pdag))
    stop("Input is not a cgraph")
  if(!is.nonlatent(pdag))
    stop("input must only contain nonlatent model edge types")
  tmp <- .prepare_cgraph_for_call(pdag, nodes = F, edges = T, adjacencies = F)
  tmp <- .Call("cf_meek_rules", tmp)
  pdag$edges[, 1] <- pdag$nodes[tmp[, 1] + 1]
  pdag$edges[, 2] <- pdag$nodes[tmp[, 2] + 1]
  pdag$edges[, 3] <- .NONLATENT_EDGE_TYPES[tmp[, 3]]

  return(pdag)
}

.dag_from_pattern <- function(pattern) {
  n_edges <- nrow(pattern$edges)
  for (i in 1:n_edges) {
    if (pattern$edges[i, 3] == .UNDIRECTED) {
        pattern$edges[i, 3] <- .DIRECTED
        if (runif(1) < .5) {
          tmp <- pattern$edges[i , 1]
          pattern$edges[i, 1] <- pattern$edges[i, 2]
          pattern$edges[i, 2] <- tmp
      }
      pattern <- meek(pattern)
    }
  }
  class(pattern) <- .DAG_CLASS
  return(pattern)
}


.dag_from_pdag <- function(pdag) {
  tmp <- .prepare_cgraph_for_call(pdag, nodes = F, edges = T, adjacencies = F)
  tmp <- .Call("cf_extend_pdag", tmp)

  if(is.null(tmp)) {
    warning("input does not admit a dag extension, returning NULL.")
    return(NULL)
  }

  pdag$edges[, 1] <- pdag$nodes[tmp$edges[, 1] +1]
  pdag$edges[, 2] <- pdag$nodes[tmp$edges[, 2] + 1]
  pdag$edges[, 3] <- .NONLATENT_EDGE_TYPES[tmp$edges[, 3]]
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