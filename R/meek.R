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

.pick_dag_from_pattern <- function(pattern) {
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


.pick_dag_from_pdag <- function(pdag) {
  warning("Coercing a pdag to a dag may be bugged!")
  pdag <- meek(pdag)
  if(is.cyclic(pdag))
    stop("pdag does not admit a dag extension")
  n_edges <- nrow(pdag$edges)
  for (i in 1:n_edges) {
    if (pdag$edges[i, 3] == .UNDIRECTED) {
      pdag$edges[i, 3] <- .DIRECTED
      if (runif(1) < .5) {
        tmp <- pdag$edges[i , 1]
        pdag$edges[i, 1] <- pdag$edges[i, 2]
        pdag$edges[i, 2] <- tmp
      }
      pdag <- meek(pdag)
    }
  }
  class(pdag) <- .DAG_CLASS
  return(pdag)
}

.pattern_from_pdag <- function(pdag) {
  warning("Coercing a pdag to a pattern may be bugged!")
  pdag <- meek(pdag)
  if(is.cyclic(pdag))
    stop("pdag cannot be coerced to a pattern")
  .dag_to_pattern(pdag)
  return(.dag_to_pattern(pdag))
}



pdag_extend <- function(pdag) {
  if(!is.cgraph(pdag))
    stop("Input is not a cgraph")
  if(!is.nonlatent(pdag))
    stop("input must only contain nonlatent model edge types")
  tmp <- .prepare_cgraph_for_call(pdag, nodes = F, edges = T, adjacencies = F)
  tmp <- .Call("cf_extend_pdag", tmp)

  print(tmp)
  pdag$edges[, 1] <- pdag$nodes[tmp[, 1] + 1]
  pdag$edges[, 2] <- pdag$nodes[tmp[, 2] + 1]
  pdag$edges[, 3] <- .NONLATENT_EDGE_TYPES[tmp[, 3]]


}