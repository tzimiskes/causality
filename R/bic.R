score_graph <- function(cgraph, data) {
  if(!is.cgraph(cgraph))
    stop("Graph is not a causality graph!")
  if(!is.dag(cgraph) & !is.pattern(cgraph))
    stop("Graph must be a dag or pattern!")
  if(is.pattern(cgraph))
    cgraph <- as.dag(cgraph)

  parents <- parents(cgraph)
  sum_BIC <- 0
  for (node in names(parents)) {
    form <- paste(node, "~", paste(parents[[node]], collapse = "+"))
    form <- as.formula(form)
    sum_BIC <- sum_BIC + BIC(lm(form, data))
  }
  return(sum_BIC)
}

parents <- function(cgraph) {
  parents <- list()
  edges <- cgraph$edges
  for(i in 1:nrow(edges)) {
    edge <- edges[i,]
    if(edge[3] == .DIRECTED) {
      parents[[edge[2]]] <- c(edge[1], parents[[edge[2]]])
    }
  }
  return(parents)
}
