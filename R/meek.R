#' Apply the Meek rules to a causality PDAG
#'
#' \code{meek} will maximally orient the given causality.pdag, possibly turning
#' it into a pattern or dag.
#' @param graph a "causality.pdag"
#' @details \code{meek} applies the meek rules to the given graph. Applying the
#'     rules results in an a maximally oriented pdag. Depending on the exact
#'     structure of graph, the output graph is either a dag, pattern, or pdag.
#' @return \code{meek} either returns a causality.dag, causality.pattern,
#'     or causality.pdag.
#' @note You are almostly certainly better off using the function
#'     \code{as.pattern}.
#' @examples
#' # Create a causality.pdag
#' nodes <- c("X1", "X2", "X3", "X4", "X5")
#' edges <- matrix(c("X1", "X5", "---",
#'                   "X1", "X2", "-->",
#'                   "X3", "X2", "-->",
#'                   "X3", "X4", "---"), ncol = 3, byrow = T)
#' graph <- cgraph(nodes, edges)
#' # Applying the meek rules will turn this graph into a pattern
#' meek(graph)
#' @references
#' Meek C. Causal inference and causal explanation with background knowledge.
#'   InProceedings of the Eleventh conference on Uncertainty in artificial
#'   intelligence 1995 Aug 18 (pp. 403-410). Morgan Kaufmann Publishers Inc.
#'
#' Pearl, Judea. Causality. Cambridge university press, 2009.
#' @export
#' @useDynLib causality r_causality_meek
meek <- function(graph) {
    if (!is.cgraph(graph))
        stop("input is not a cgraph")
    # Really, this operation only makes sense for PDAGs, so this might change
    if (is.dag(graph) || is.pattern(graph))
        return(graph)
    if (!is.pdag(graph) && !is_valid_pdag(graph))
        stop("The meek rules can only be run on nonlatent acylic graphs.")
    # maybe check to see if it has a dag extension first?
    graph <- .Call("r_causality_meek", graph)
    if (suppressWarnings(is_valid_dag(graph)))
        class(graph) <- .DAG_CLASS
    else if(suppressWarnings(is_valid_pattern(graph)))
        class(graph) <- .PATTERN_CLASS
    return(graph)
}
