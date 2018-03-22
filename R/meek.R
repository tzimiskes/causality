meek <- function(pdag) {
  tmp <- .prepare_cgraph_for_call(pdag, nodes = F, edges = T, adjacencies = T)
  tmp <- .Call("meek_rules", tmp)
  pdag$edges[, 1] <- pdag$nodes[tmp[, 1] + 1]
  pdag$edges[, 2] <- pdag$nodes[tmp[, 2] + 1]
  pdag$edges[, 3] <- .NONLATENT_EDGE_TYPES[tmp[, 3]]

  return(pdag)

}

.pick_dag_from_pdag <- function(pdag) {

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
  return(pdag)
}