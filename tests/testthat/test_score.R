library(causality)

context("test scoring graphs")
score_graph <- function(cgraph, data) {
  if (!is.cgraph(cgraph))
    stop("Graph is not a causality graph!")
  if (!is.dag(cgraph)) {
    cgraph <- as.dag(cgraph)
    if (is.null(cgraph)) {
      warning("Cannot score graph because it lacks a DAG extension")
      return(NA)
    }
  }

  for (var in names(data)) {
    data[[var]] <- (data[[var]]- mean(data[[var]]))/sd(data[[var]])
  }
  parents <- parents(cgraph)
  sum_BIC <- 0
  for (node in names(parents)) {
    ssq   <- 1
    node.parents <- parents[[node]]
    COVXX <- cov(data[node.parents])
    COVXY <- as.vector(cov(data[node], data[node.parents]))
    ssq   <- ssq - COVXY %*% ginv(COVXX) %*% COVXY # SLOW!!!!
    theta <- 2 * length(node.parents) + 1
    n <- nrow(data)
    sum_BIC <- sum_BIC +  n * log(unname(ssq)) + theta * log(n)
  }
  return(as.numeric(sum_BIC))
}

parents <- function(cgraph) {
  parents <- list()
  edges <- cgraph$edges
  for (i in 1:nrow(edges)) {
    edge <- edges[i, ]
    if (edge[3] == .DIRECTED) {
      parents[[edge[2]]] <- c(edge[1], parents[[edge[2]]])
    }
  }
  return(parents)
}
