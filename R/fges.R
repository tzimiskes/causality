#' @export
#' @useDynLib causality ccf_fges_wrapper
fges <- function(df,
                  score = c("BIC", "BDue"),
                  penalty = 1.0,
                  sample_prior = 1.0,
                  structure_prior = 1.0) {
  score <- match.arg(score, c("BIC", "BDeu"))
  # the first step is to convert the data frame into one that only contains
  # numerics and integers. numerics are normalized.
  if (any(is.na(df)))
    stop("df must not contain any missing values.")
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
    if (score == "BDeu" && is.double(col)) {
      stop("BDeu scoring cannot be used in conjuction with continuous data.
            Use BIC or CG")
    }
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
  else if (score == "CG")
    stop("not implemented")
  .Call("ccf_fges_wrapper", df, score ,dimensions, floating.args, integer.args)
  return()
}
