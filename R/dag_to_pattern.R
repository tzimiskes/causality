#' Convert a DAG to a Pattern
#' \code{dag_to_pattern} converts a `causality` dag to a `causality`
#' pattern
#' @param dag An object of class dag that is to be converted
#' @return A `causality` pattern. In the event that \code{dag} is not actually a
#'   dag, an error is thrown.
#' @details The algorithm is due to Chickering(1995). See reference for details. The first step of the function is to perform a topoligical sort, which is only possible if \code{dag} is a dag.
#' @examples
#' TODO(arix)
#' @references David Maxwell Chickering: “A Transformational Characterization of
#'   Equivalent Bayesian Network Structures”, 1995;
#'   \href{https://arxiv.org/abs/1302.4938}{arXiv:1302.4938 [cs.AI]}
dag_to_pattern <- function(dag) {
  if (!("dag" %in% class(dag)))
    stop("Input is not a dag!")
  if(("pattern" %in% class(dag)))
    stop("Input is already a pattern!")

  n_edges <- nrow(dag$edges)
  parents <- list()
  for (i in 1:n_edges)
    parents[[dag$edges[i, 2]]] <- c(parents[[dag$edges[i, 2]]], dag$edges[i, 1])

  ordered_edges <- .order_edges(dag, parents)

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

  #find-compelled

# Topological sorting algorithm is the one described in CLRS;
.topological_sort <- function(dag) {
  # generate the children of each node
  children <- list()
  for (i in 1:nrow(dag$edges))
      children[[dag$edges[i ,1]]] <- c(children[[dag$edges[i, 1]]], dag$edges[i, 2])

  order <- c()
  marked <- rep(0, length(dag$names))
  nodes <- dag$names
  i <- 1
  while (sum(marked) < length(dag$names)) {
  if (marked[i] == 0) {
    tmp <- .visit(nodes[i], nodes, marked, children, order)
    order <- tmp[[1]]
    marked <- tmp[[2]]
  }
  else
    i <- i + 1
  }
  return(order)
}

.visit <- function(node, nodes, marked, children, order) {
  i <- match(node, nodes)
  if (marked[i] == 1)
    return(list(order, marked))
  if (marked[i] == -1)
    stop("dag contains a cycle, so it cannot be of class \"dag\"")
  marked[i] <- -1
  for (child in children[[node]]) {
    tmp <- .visit(child, nodes, marked, children, order)
    order <- tmp[[1]]
    marked <- tmp[[2]]
  }
  marked[i] <- 1
  order <- c(node, order)
  return(list(order, marked))
}

.order_edges  <- function(dag, parents) {

  top_order <- .topological_sort(dag)

  n_edges <- nrow(dag$edges)

  ordered_edges <- matrix(ncol = 3)
  for (node in top_order) {
    node_parents <- parents[[node]]
    marked <- rep(0, length(node_parents))
    while(sum(marked) < length(node_parents)) {
      max <- node_parents[match(0, marked)]
      for(parent in node_parents) {
        if ((match(parent, top_order) > match(max, top_order)) && !marked[match(parent, node_parents)])
          max <- parent
      }
      marked[match(max, node_parents)] <- 1
      ordered_edges <- rbind(ordered_edges, c(max, node, "-->"))
    }
  }
  return(ordered_edges[-1,])
}


topological_sort <- function(dag) {
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

  tmp<-.Call("c_dag_to_pattern", dag)
  return(tmp)
}

