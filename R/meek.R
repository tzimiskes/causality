#' Apply the Meek rules to a causality PDAG
#'
#' \code{meek} will turn a PDAG into a Pattern if the PDAG has a
#' valid dag extension
#'
#' @param graph a "causality.pdag"
#' @details TODO(arix) -- Perhaps this function shouldn't be public
#' @return If a "causality.pdag is input, either either a pattern or dag is
#'   output. In the event that the pdag doesn't have a dag extension, garbage
#'   is output If a causality.dag or causality.pattern is input, \code{meek}
#'   just returns the input with no changes.
#' @note You are almostly certainly better off using the function
#'   \code{as.pattern}.
#' @examples
#' TODO(arix)
#' @references
#' Meek C. Causal inference and causal explanation with background knowledge.
#'   InProceedings of the Eleventh conference on Uncertainty in artificial
#'   intelligence 1995 Aug 18 (pp. 403-410). Morgan Kaufmann Publishers Inc..
#'
#' Pearl, Judea. Causality. Cambridge university press, 2009.
#' @export
#' @useDynLib causality ccf_meek_wrapper
meek <- function(graph) {
  if (!is.cgraph(graph))
    stop("Input is not a cgraph")
  # Really, this operation only makes sense for PDAGs, so this might change
  if (is.dag(graph) || is.pattern(graph))
    return(graph)
  if (!is.pdag(graph) || !(is.nonlatent(graph) && is.acyclic(graph)))
    stop("The meek rules can only be run on nonlatent acylic graphs.")

  # maybe check to see if it has a dag extension first?
  graph <- .Call("ccf_meek_wrapper", graph)
  if (is.directed(graph))
    class(graph) <- .DAG_CLASS
  else
    class(graph) <- .PATTERN_CLASS
  return(graph)
}
