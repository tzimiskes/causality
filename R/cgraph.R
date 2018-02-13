# alpha version of a data structure that can be used to hold a general graph.
cgraph <- function(nodes = c(character()), adjacencies = matrix(), edges = list(c())) {
  return(
    structure(list(nodes = nodes, adjacencies = adjacencies, edges = edges), class = "cgraph")
  )
}

# The following is a series of is function to do simple type checking for the
# user and the various functions in the package

is.cgraph <- function(graph) {
  if (("cgraph" %in% class(graph)))
    return(TRUE)
  else
    return(FALSE)
}

is.dag <- function(dag) {
  if (("dag" %in% class(dag)))
    return(TRUE)
  else
    return(FALSE)
}

is.pattern <- function(dag) {
  if (("pattern" %in% class(dag)))
    return(TRUE)
  else
    return(FALSE)
}

is.pag <-function(pag) {
  if ("pag" %in% class(pag))
    return(TRUE)
  else
    return(FALSE)
}

is.pdag <-function(pdag) {
  if ("pdag" %in% class(pag))
    return(TRUE)
  else
    return(FALSE)
}

# attempt to coerce a graph of type cgraph to a dag
as.dag <- function(graph) {
  if (!is.cgraph(graph))
    stop("graph is not of type cgraph")
  for (i in 1:nrow(graph$edges)) {
    if (graph$edges[i,3] != "-->") {
      stop("Cannot coerce graph to dag due to incompatable edge type")
    }
  }

  if(is.pattern(graph) || is.pdag(graph)) {
    # TODO(arix)
  }

  # implementation is contained in dag_to_pattern.R
  # check to see if a topological sort is possible
  # if it is, it is a dag
  sort <- .topological_sort(graph)
  if (is.null(sort)) {
    stop("Cannot coerce graph to dag because graph contains a cycle")
  }
  class(graph) <- c("dag", class(graph))
  return(graph)
}

as.pattern <- function(dag) {
  if (is.dag(dag))
    return(dag_to_pattern(dag))
  else if (is.cgraph(dag)) {
    message("input is not labeled as a dag, calling as.dag to see if it is.")
    dag <- as.dag(dag)
    message("success! converting to pattern...")
    return(dag_to_pattern(dag))
  } else if(is.pdag(dag)) {
    # TODO(arix)
    message("this is yet to be implemented")
  }
  stop("input can not be converted to pattern")
}
# TODO(arix)
as.pdag <- function(graph) {
  if(is.pattern(graph)) {
    class(graph) <- c("pdag", "cgraph")
  }
}

# might change this
print.cgraph <- function(graph) {
  print.default(graph)
}

is_valid_cgraph <- function(graph) {
  # check for duplicate nodes
  nodes <- sort(graph$nodes)
  for (i in 1:(length(nodes)-1)) {
    if ( nodes[i] == nodes[i + 1]) {
      message("graph contains duplicate nodes")
      return(FALSE)
    }
  }

  # TODO(arix) check adjacencies

  # check for invalid edges
  n_edges <- nrow(graph$edges)

  parents = list()
  for (i in 1:n_edges) {
    edge <- graph$edges[i,]
    if (edge[1] == edge[2]) {
      message("graph contains a self loop")
      return(FALSE)
    }
    if (!is.null(as.list(parents[[edge[2]]])[[edge[1]]])) {
      message("graph is a multigraph")
      return(FALSE)
    }
    else
      parents[[edge[2]]][[edge[1]]] <- 1
    if (!(edge[1] %in% graph$nodes) || (!edge[2] %in% graph$nodes)) {
      message("graph contains nodes that are not in the node list")
      return(FALSE)
    }
  }
  return(TRUE)
}