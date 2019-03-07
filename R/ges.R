#' @export
#' @useDynLib causality r_causality_ges
ges <- function(df, score = c("bic", "bdue"), penalty = 1.0, sample.prior = 1.0,
                    structure.prior = 1.0)
{
  if (!is.data.frame(df))
    stop("df must be a data.frame")
  score <- match.arg(score, c("bic", "bdeu"))
  # the first step is to convert the data frame into one that only contains
  # numerics and integers. numerics are normalized.
  if (any(is.na(df)))
    stop("df must not contain any missing values.")
  ncol       <- ncol(df)
  dimensions <- rep(0L, ncol)
  for (j in 1:ncol) {
    col <- df[[j]]
    if (is.double(col)) {
    # noop
    }
    else if (is.integer(col)) {
      dimensions[j] <- length(unique(col))
      df[[j]]       <- col - min(col)
    }
    else if (is.factor(col)) {
      dimensions[j] <- nlevels(col)
      df[[j]]       <- as.integer(col)
      df[[j]]       <- as.integer(col) - 1L
    }
    else if (is.character(col)) {
      col           <- as.factor(col)
      dimensions[j] <- nlevels(col)
      df[[j]]       <- as.integer(col) - 1L
    }
    if (score == "bic" && is.integer(col)) {
        dimensions[j] <- 0
        col <- as.double(col)
    }
    if (score == "bdeu" && is.double(col)) {
      stop("bdeu scoring cannot be used in conjuction with continuous data.
            Use bic or cg")
    }
  }
  # deterime the floating and integer arguments depending on the score
  if (score == "bic") {
    floating.args <- c(penalty)
    integer.args  <- c()
  }
  else if (score == "bdeu") {
    floating.args <- c(sample.prior, structure.prior)
    integer.args  <- c()
  }
  else if (score == "cg")
    stop("not implemented")

  score.func.args <-
  switch(score,
         "bic"  = list(penalty = penalty),
         "bdeu" = list(sample.prior = sample.prior,
                       structure.prior = structure.prior
                  )
        )
  ges.out <- .Call("r_causality_ges", df, score, dimensions, floating.args,
                                    integer.args)
  names(ges.out) <- c("graph", "graph.score")
  # # add additonal diagnostic info
  ges.out$score.func      <- score
  ges.out$score.func.args <- score.func.args
  return(ges.out)
}
