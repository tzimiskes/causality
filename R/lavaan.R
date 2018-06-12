# score_graph <- function(cgraph, data) {
#   if (!is.cgraph(cgraph))
#     stop("Graph is not a causality graph!")
#   if (!is.dag(cgraph) & !is.pattern(cgraph))
#     stop("Graph must be a dag or pattern!")
#   if (is.pattern(cgraph))
#     cgraph <- as.dag(cgraph)
#   parents <- parents(cgraph)
#   sum_BIC <- 0
#   for (node in names(parents)) {
#     ssq   <- cov(data[node])
#     node.parents <- parents[[node]]
#     COVXX <- cov(data[node.parents])
#     COVXY <- as.vector(cov(data[node], data[node.parents]))
#     ssq   <- ssq - COVXY %*% ginv(COVXX) %*% COVXY
#     theta <- length(node.parents) + 1
#     n <- nrow(data)
#     sum_BIC <- sum_BIC + n*log(unname(ssq)) + theta*log(n)
#   }
#   return(as.numeric(sum_BIC))
# }




as.lavaan.formula <- function(cgraph) {

  cgraph <- as.dag(cgraph)
  parents <- parents(cgraph)
  formulas <- c()
  for (node in names(parents)) {
    formulas <- c(formulas, paste(node, "~", paste(parents[[node]], collapse = " + ")))
  }
  return(paste(formulas, collapse = " ; "))
}

