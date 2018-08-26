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

#' @useDynLib causality ccf_score_graph_wrapper
#' @export
score <- function(graph,
                  df,
                  score = c("BIC", "BDue"),
                  penalty = 1.0,
                  sample_prior = 1.0,
                  structure_prior = 1.0) {
  if (!is.cgraph(graph))
    stop("graph is not a causality.graph!")
  if (!is.dag(graph))
    stop("graph is not a causality.dag!")
  score <- match.arg(score, c("BIC", "BDeu"))
  # the first step is to convert the data frame into one that only contains
  # numerics and integers. numerics are normalized.
  ncol       <- ncol(df)
  dimensions <- rep(0L, ncol)
  for (j in 1:ncol) {
    col <- df[[j]]
    if (is.double(col)) {
      col     <- col - mean(col)
      df[[j]] <- col / sd(col)
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
    if (score == "BIC" && is.integer(col))
      col <- as.double(col)
  }
  # deterime the floating and integer arguments depending on the score
  if (score == "BIC") {
  floating.args <- c(penalty)
  integer.args <- c()
  }
  else if (score == "BDeu") {
    floating.args <- c(sample_prior, structure_prior)
    integer.args <- c()
  }
  score <- .Call("ccf_score_graph_wrapper",
                 graph,
                 df,
                 score,
                 dimensions,
                 floating.args,
                 integer.args)
  return(score)
}
