#' Turn a causality graph into a lavaan formula
#'
#' Use this function to convert a causality.graph into a lavaan formula so you
#' can fit the model to the data and assess the quality of the model from the
#' graph.
#' @param cgraph A causality.graph to be turned into a lavaan formula
#' @details currently, only DAGs are supported, though patterns and PDAGs with
#'   DAG extensions are accepted. Future work will inlcude PAGs, and including
#'   output from the FnFC family as a latent measurement model.
#' @return a \code{lavaan} formula, or an error
#' @examples TODO(arix)
as.lavaan.formula <- function(cgraph) {
  if (!is.cgraph(cgraph)) {
    stop("Input is not a cgraph")
  }
  if (!is.dag(cgraph))
    cgraph <- as.dag(cgraph)
  parents <- parents(cgraph)
  formulas <- c()
  for (node in names(parents)) {
    formulas <- c(formulas,
                  paste(node, "~", paste(parents[[node]], collapse = " + ")))
  }
  return(paste(formulas, collapse = " ; "))
}

