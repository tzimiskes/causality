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
dag_to_pattern <- function(dag) {
  if (!("dag" %in% class(dag)))
    stop("Input is not a dag!")
  if(("pattern" %in% class(dag)))
    stop("Input is already a pattern!")

  n_edges <- nrow(dag$edges)
  parents <- list()
  for (i in 1:n_edges)
    parents[[dag$edges[i, 2]]] <- c(parents[[dag$edges[i, 2]]], dag$edges[i, 1])

  ordered_edges <- order_edges(dag)$edges

  # 0 means unknown, 1 means compelled, -1 means reverseable
  hash_table <- list(list())
  for(node in dag$names) {
    for(parent in parents[[node]])
      hash_table[[parent]][[node]] <- 0
  }

  n_unknown <- n_edges
  while (n_unknown > 0) {
    i <- 1
    edge <- ordered_edges[i,]
    parent <- edge[1]
    child  <- edge[2]
    while (hash_table[[parent]][[child]] != 0) {
      i <- i + 1
      edge <- ordered_edges[i,]
      parent <- edge[1]
      child  <- edge[2]
    }
    skip_flag <- 0
    for (parent_of_parent in parents[[parent]]) {
      if (hash_table[[parent_of_parent]][[parent]] == 1) {
        if(parent_of_parent %in% parents[[child]]) {
          hash_table[[parent_of_parent]][[child]] <- 1
            n_unknown <- n_unknown -1
        } else {
          for (parent_of_child in parents[[child]]) {
            hash_table[[parent_of_child]][[child]] <- 1
            n_unknown <- n_unknown - 1
          }
          skip_flag <- 1
          break
        }
      }
    }
    if (skip_flag)
      next
    v_structure_flag <- 0
    for (parent_of_child in parents[[child]]) {
      if (parent_of_child != parent) {
        if ( !(parent_of_child %in% parents[[parent]]) ) {
          v_structure_flag <- 1
          break
        }
      }
    }
    if (v_structure_flag) {
      for (parent_of_child in parents[[child]]) {
        if (hash_table[[parent_of_child]][[child]] == 0) {
          hash_table[[parent_of_child]][[child]] <- 1
          n_unknown <- n_unknown - 1
        }
      }
    } else {
      for (parent_of_child in parents[[child]]) {
        if (hash_table[[parent_of_child]][[child]] == 0) {
          hash_table[[parent_of_child]][[child]] <- -1
          n_unknown <- n_unknown - 1
        }
      }
    }
  }
  pattern <- dag
  for(i in 1:n_edges) {
    edge <- pattern$edges[i,]
    if(hash_table[[edge[1]]][[edge[2]]] == -1)
      pattern$edges[i,3] <- "---"
  }
  class(pattern) <- c("pattern","pdag", "cgraph")
  return(pattern)
}


#' Get the topological ordering of a dag
#'
#' \code{topological_order} calculates the topological ordering of dag.
#' @param dag causality dag that you wish to calculate the topological ordering
#'   of
#' @return A charcter vector that contains the nodes of the dag ordered
#'   according to their topological order. In the event that \code{dag} contains
#'   a cycle (ie isn't actually a dag) a error is thrown and NA is teturned
#'
#' @details \code{topological_sort} generates a topological ordering of the
#'   given dag by using a depth first search as described in CLRS. The underlying
#'   C implementation of this function is used in the function
#'   \code{\link{dag_to_pattern}} and a slightly different version is used in \code{\link{as.dag}}: see below.
#' @examples
#' TODO(arix)
#' @note This function is designed to get the topological order from a dag, not
#'   determine whether or not a function is a dag. If you wish to test whether
#'   or not a cgraph object is a dag or not, use the function \code{\link{as.dag}}
#'   instead. It performs additional checking on top of performing a topological
#'   ordering.
#' @seealso   \code{\link{as.dag}} \code{\link{dag_to_pattern}}
#' @references Cormen, Thomas H., et al. Introduction to Algorithms. The MIT
#'   Press, 2014.
topological_sort <- function(dag) {
  if(!is.cgraph(dag)) {
    stop("input dag is not of type cgraph")
  }
  if(!is.dag(dag)) {
    stop("input is not of type dag. try, as.dag(dag) to
          try to coerce dag to class dag")
  }
  # creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(dag$names))
    hash[[dag$names[[i]]]] <- i - 1
  for (i in 1:nrow(dag$edges)) {
    dag$edges[i,1] <- hash[[dag$edges[i,1]]]
    dag$edges[i,2] <- hash[[dag$edges[i,2]]]
    dag$edges[i,3] <- 1
  }
  nc <- ncol(dag$edges)
  nr <- nrow(dag$edges)
  dag$edges <- as.integer(dag$edges)
  dim(dag$edges) <- c(nr, nc)
  tmp<-tryCatch(.Call("c_topological_sort", dag), error = function(e) NA)
  return(dag$names[tmp+1])
}

# this is a private version of topological order that is used by as.dag()
# to check whether or not a cgraph object is a dag or not. This is sort of a bad use of code, but
# I didn't want to make topoloigcal sort have the formals be dag, force = F because topological sort is
# supposed to give the gvie the topological ordering of a **DAG**, not test whether or not it is one
# as.dag will try to convert an object to a dag, by running topological sort to see if it feasable
.topological_sort <- function(dag) {
# creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(dag$names))
    hash[[dag$names[[i]]]] <- i - 1
  # convert the matrix of edges, which is a character vector, to an integer
  # vector. This is done so the C end is simpler
  for (i in 1:nrow(dag$edges)) {
    dag$edges[i,1] <- hash[[dag$edges[i,1]]]
    dag$edges[i,2] <- hash[[dag$edges[i,2]]]
    dag$edges[i,3] <- 1
  }
  nc <- ncol(dag$edges)
  nr <- nrow(dag$edges)
  dag$edges <- as.integer(dag$edges)
  dim(dag$edges) <- c(nr, nc)
  # now that we have an integer matrix, call the C function
  tmp <- tryCatch(.Call("c_topological_sort", dag), error = function(e) NULL)
  if(is.null(tmp))
    return(NULL)
  else
    return(dag$names[tmp+1])
}

order_edges <- function(dag) {
  # creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(dag$names))
    hash[[dag$names[[i]]]] <- i - 1
  # convert the matrix of edges, which is a character vector, to an integer
  # vector. This is done so the C end is simpler
  for (i in 1:nrow(dag$edges)) {
    dag$edges[i,1] <- hash[[dag$edges[i,1]]]
    dag$edges[i,2] <- hash[[dag$edges[i,2]]]
    dag$edges[i,3] <- 1
  }
  nc <- ncol(dag$edges)
  nr <- nrow(dag$edges)
  dag$edges <- as.integer(dag$edges)
  dim(dag$edges) <- c(nr, nc)
  # now that we have an integer matrix, call the C function
  tmp <- tryCatch(.Call("c_order_edges", dag),  error = function(e) NULL)
  if(is.null(tmp))
    return(NULL)
  else {
    for(i in 1:nrow(dag$edges)) {
      tmp[i,1] <- dag$names[as.numeric(tmp[i, 1]) + 1]
      tmp[i,2] <- dag$names[as.numeric(tmp[i, 2]) + 1]
      tmp[i,3] <- "-->"
    }
    dag$edges <- tmp
    return(dag)
    }
}