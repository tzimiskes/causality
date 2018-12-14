#' @useDynLib causality causalityScoreGraph
#' @export
score <- function(graph, df, score = c("bic", "bdue"), penalty = 1.0,
                             sample.prior = 1.0, structure.prior = 1.0)
{
  if (!is.cgraph(graph))
    stop("graph is not a causality.graph!")
  if (!is.dag(graph))
    stop("graph is not a causality.dag!")
  score <- match.arg(score, c("bic", "bdeu"))
  # the first step is to convert the data frame into one that only contains
  # numerics and integers. numerics are normalized.
  dimensions <- rep(0L, ncol(df))
  for (j in 1:ncol(df)) {
    col <- df[[j]]
    if (is.double(col)) {
      col     <- col - mean(col)
      df[[j]] <- col / sd(col)
    }
    else if (is.integer(col)) {
      dimensions[j] <- length(unique(col))
      df[[j]]       <- col - min(col)
    }
    else if (is.factor(col)) {
      dimensions[j] <- nlevels(col)
      df[[j]]       <- as.integer(col) - 1L
    }
    else if (is.character(col)) {
      col           <- as.factor(col)
      dimensions[j] <- nlevels(col)
      df[[j]]       <- as.integer(col) - 1L
    }
    if (score == "bic" && is.integer(col))
      col <- as.double(col)
    if (score == "bdeu" && is.double(col))
      stop("bdeu scoring cannot be used in conjuction with continuous data.
            Use bic or cg")
  }
  # deterime the floating and integer arguments depending on the score
  if (score == "bic") {
    floating.args <- c(penalty)
    integer.args  <- c()
    }
  else if (score == "bdeu") {
    floating.args <- c(sample_prior, structure_prior)
    integer.args <- c()
  }
  else if (score == "cg")
    stop("not implemented")
  score <- .Call("causalityScoreGraph", graph, df, score, dimensions,
                                        floating.args, integer.args)
  return(score)
}
