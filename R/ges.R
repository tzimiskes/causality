#' Greedy Equivalence Search (GES)
#'
#' GES is a score based causal discovery algorithm that outputs a pattern, a
#' graph that encodes the markov equilevence class of a set of DAGs. GES
#' contains score functions for continuous and discrete datasets. Mixed datasets
#' will have to be treated treated as continuous or discretized completly.
#' Other versions of ges support background knowledge, but this version does not.
#'
#' @param df A data.frame with no missing values.
#' @param score The scoring function to use. Use BIC for continuous data and
#'        BDeu for discrete.
#' @param penalty Tuning parameter for bic score. Cannot be less than 0;
#'        less than 1 is probably a bad idea. Higher penalties will generate
#'        sparser graphs. Defaults to 1, which corresponds to standard BIC.
#' @param structure.prior First tuning parameter for BDeu score.
#' @param sample.prior Second tuning parameter for BDeu score.
#' @author Alexander Rix
#' @references
#' Chickering DM. Optimal structure identification with greedy search.
#' Journal of machine learning research. 2002;3(Nov):507-54.
#' @examples
#' library(causality)
#' ges(ecoli.df, "bic", penalty = 2)
#' @useDynLib causality r_causality_ges
#' @export
ges <- function(df, score = c("bic", "bdue"), penalty = 1.0,
                    sample.prior = 1.0, structure.prior = 1.0)
{
    if (!is.data.frame(df))
        stop("df must be a data.frame")
    if (any(is.na(df)))
        stop("df must not contain any missing values.")
    score <- match.arg(score, c("bic", "bdeu"))
    ncol       <- ncol(df)
    dimensions <- rep(0L, ncol)
    for (j in 1:ncol) {
        col <- df[[j]]
        if (is.integer(col)) {
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
    else
        stop("error")
    score.func.args <- switch(score,
        "bic"  = list(penalty = penalty),
        "bdeu" = list(sample.prior = sample.prior,
                      structure.prior = structure.prior)
    )
    ges.out <- .Call("r_causality_ges", df, score, dimensions, floating.args,
                        integer.args)
    names(ges.out) <- c("graph", "graph.score")
    # add additonal diagnostic info
    ges.out$score.func      <- score
    ges.out$score.func.args <- score.func.args
    return(ges.out)
}
