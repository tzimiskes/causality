#' Convert a DAG to a Pattern
#'
#' \code{dag_to_pattern} converts a causality dag to a pattern
#' @param dag A cgraph of type dag that is to be converted
#' @return A `causality` pattern. In the event that \code{dag} is not actually a
#'   dag, an error is thrown.
#' @details The algorithm is due to Chickering(1995). See reference for details.
#'   The first step of the function is to perform a topoligical sort, which is
#'   only possible if \code{dag} is a dag.
#' @examples
#' TODO(arix)
#' @references David Maxwell Chickering: “A Transformational Characterization of
#'   Equivalent Bayesian Network Structures”, 1995;
#'   \href{https://arxiv.org/abs/1302.4938}{arXiv:1302.4938 [cs.AI]} Cormen,
#'   Thomas H., et al. Introduction to Algorithms. The MIT Press, 2014.
.dag_to_pattern <- function(dag) {
  # creating a "hash table" makes the next operation faster
  tmp <-.prepare_cgraph_for_call(dag, nodes = F, edges = T, adj = F)
  tmp <-.Call("cf_dag_to_pattern", tmp)
  dag$edges[, 1] <- dag$nodes[tmp[, 1] + 1]
  dag$edges[, 2] <- dag$nodes[tmp[, 2] + 1]
  dag$edges[, 3] <- c(.DIRECTED,.UNDIRECTED)[tmp[, 3]]
  class(dag) <- .PATTERN_CLASS
  return(dag)
}


.dag_from_pattern <- function(pattern) {
  n_edges <- nrow(pattern$edges)
  for (i in 1:n_edges) {
    if (pattern$edges[i, 3] == .UNDIRECTED) {
      pattern$edges[i, 3] <- .DIRECTED
      if (runif(1) < .5) {
        tmp <- pattern$edges[i , 1]
        pattern$edges[i, 1] <- pattern$edges[i, 2]
        pattern$edges[i, 2] <- tmp
      }
      pattern <- meek(pattern)
    }
  }
  class(pattern) <- .DAG_CLASS
  return(pattern)
}


.dag_from_pdag <- function(pdag) {
  tmp <- .prepare_cgraph_for_call(pdag, nodes = F, edges = T, adjacencies = F)
  tmp <- .Call("cf_extend_pdag", tmp)

  if(is.null(tmp)) {
    warning("input does not admit a dag extension, returning NULL.")
    return(NULL)
  }

  pdag$edges[, 1] <- pdag$nodes[tmp$edges[, 1] +1]
  pdag$edges[, 2] <- pdag$nodes[tmp$edges[, 2] + 1]
  pdag$edges[, 3] <- .NONLATENT_EDGE_TYPES[tmp$edges[, 3]]
  class(pdag) <- .DAG_CLASS
  return(pdag)
}

.pattern_from_pdag <- function(pdag) {
  dag <- .pick_dag_from_pdag(pdag)
  if(is.null(dag)) {
    warning("Cannot convert pdag to pattern")
    return(dag)
  }
  return(.dag_to_pattern(pdag))
}


# this is a private version of topological order that is used by as.dag() to
# check whether or not a cgraph object is a dag or not. This is sort of a bad
# use of code, but I didn't want to make topological sort have the formals be
# dag, force = F because topological sort is supposed to give the give the
# topological ordering of a **DAG**, not test whether or not it is one as.dag
# will try to convert an object to a dag, by running topological sort to see if
# it feasable
.topological_sort <- function(dag) {
  # creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(dag$nodes))
    hash[[dag$nodes[[i]]]] <- i - 1
  # convert the matrix of edges, which is a character vector, to an integer
  # vector. This is done so the C end is simpler
  tmp <- .prepare_cgraph_for_call(dag, F, T, F)
  # now that we have an integer matrix, call the C function
  order <- tryCatch(.Call("cf_topological_sort", tmp), error = function(e) NULL)
  if(is.null(order))
    return(NULL)
  else
    return(dag$nodes[order + 1])
}