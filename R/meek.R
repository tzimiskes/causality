pick_dag_from_pdag <- function(pdag) {
  if(!is.pdag(pdag))
    stop("Input is not a pdag!")
}

meek <- function(pdag) {
  tmp <- .prepare_cgraph_for_call(pdag, nodes = F, edges = T, adjacencies = T)

  tmp <- .Call("meek_rules", tmp)

  pdag$edges[, 1] <- pdag$nodes[tmp[, 1] + 1]
  pdag$edges[, 2] <- pdag$nodes[tmp[, 2] + 1]
  pdag$edges[, 3] <- c("-->","---")[tmp[, 3]]

  return(pdag)

}