# score_graph <- function(cgraph, data) {
#   if(!is.cgraph(cgraph))
#     stop("Graph is not a causality graph!")
#   if(!is.dag(cgraph) & !is.pattern(cgraph))
#     stop("Graph must be a dag or pattern!")
#   if(is.pattern(cgraph))
#     cgraph <- as.dag(cgraph)
#
#   parents <- parents(cgraph)
#   sum_BIC <- 0
#   for (node in names(parents)) {
#     form <- paste(node, "~", paste(parents[[node]], collapse = "+"))
#     form <- as.formula(form)
#     sum_BIC <- sum_BIC + BIC(lm(form, data)) + 2*length(parents[[node]]) + 1 #bias correction
#   }
#   return(sum_BIC)
# }
#' @export
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
  parents <- parents(cgraph)
  sum_BIC <- 0
  for (node in names(parents)) {
    ssq   <- cov(data[node])
    node.parents <- parents[[node]]
    COVXX <- cov(data[node.parents])
    COVXY <- as.vector(cov(data[node], data[node.parents]))
    ssq   <- ssq - COVXY %*% ginv(COVXX) %*% COVXY # SLOW!!!!
    theta <- length(node.parents) + 1
    n <- nrow(data)
    sum_BIC <- sum_BIC + n * log(unname(ssq)) + theta * log(n)
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

new_score <- function(graph, df, score = c("BIC", "BDue")) {
  if(!is.cgraph(graph))
    stop("graph is not a causality.graph!")
  if(!is.dag(graph))
    stop("graph is not a causality.dag!")
  score <- match.arg(score, c("BIC", "BDue"))
  # the first step is to convert the data frame into one that only contains
  # numerics and integers. numerics are normalized.
  ncol       <- ncol(df)
  dimensions <- rep(0L, ncol)
  for (j in 1:ncol) {
    col <- df[[j]]
    if (is.numeric(col)) {
      col     <- col - mean(col)
      df[[j]] <- col/sd(col)
    }
    else if (is.integer(col)) {
      dimensions[j] <- length(unique(col))
    }
    else if (is.factor(col)) {
      dimensions[j] <- nlevels(col)
      df[[j]]       <- as.integer(col)
    }
    else if (is.character(col)) {
      col           <- as.factor(col)
      dimensions[j] <- nlevels(col)
      df[[j]]       <- as.integer(col)
    }
    else
      stop("Unrecognized type in df!")
  }
  # score <- .Call("ccf_score_graph", graph, df, score, dimensions)
  return(dimensions)
}

