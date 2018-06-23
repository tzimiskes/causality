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
