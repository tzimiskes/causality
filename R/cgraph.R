# beta version of a data structure that can be used to hold a general graph.
cgraph <- function(nodes, adjacencies, edges) {
  return(structure(
    list(nodes = nodes, adjacencies = adjacencies, edges = edges),
    class = .CGRAPH_CLASS))
  }

#these hidden (lol) variables are used to assign (sub)classes to craph objects
.CGRAPH_CLASS  <- c(                     "causality-graph")
.DAG_CLASS     <- c("causality-dag"    , "causality-graph")
.PDAG_CLASS    <- c("causality-pdag"   , "causality-graph")
.PATTERN_CLASS <- c("causality-pattern", "causality-graph")
.PAG_CLASS     <- c("causality-pag"    , "causality-graph")
# The following is a series of is function to do simple type checking for the
# user and the various functions in the package

is.cgraph <- function(graph) {
  if (.CGRAPH_CLASS %in% class(graph))
    return(TRUE)
  else
    return(FALSE)
}

is.dag <- function(cgraph) {
  if (isTRUE(all.equal(.DAG_CLASS, class(cgraph))))
    return(TRUE)
  else
    return(FALSE)
}

is.pattern <- function(cgraph) {
  if (isTRUE(all.equal(.PATTERN_CLASS, class(cgraph))))
    return(TRUE)
  else
    return(FALSE)
}

is.pdag <-function(cgraph) {
  if (isTRUE(all.equal(.PDAG_CLASS, class(cgraph))))
    return(TRUE)
  else
    return(FALSE)
}

is.pag <-function(cgraph) {
  if (isTRUE(all.equal(.PAG_CLASS, class(cgraph))))
    return(TRUE)
  else
    return(FALSE)
}

is.cyclic <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if (is.null(.topological_sort(cgraph)))
    return(TRUE)
  else
    return(FALSE)
}

is.directed <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  edge_types <- cgraph$edges[, 3]
  n_edges <- length(edge_types)
  for (i in 1:n_edges) {
    if (edge_types[i] != .DIRECTED) { # ie  edge_type != -->
      return(FALSE)
    }
  }
  return(TRUE)
}

is.nonlatent <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  edge_types <- cgraph$edges[, 3]
  n_edges <- length(edge_types)
  for (i in 1:n_edges) {
    if (!(edge_types[i] %in% .NONLATENT_EDGE_TYPES))
      return(FALSE)
  }
  return(TRUE)
}

is.latent <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  edge_types <- cgraph$edges[, 3]
  n_edges <- length(edge_types)
  for (i in 1:n_edges) {
    if (!(edge_types[i] %in% .LATENT_EDGE_TYPES))
      return(FALSE)
  }
  return(TRUE)
}

# attempt to coerce a graph of type cgraph to a dag
as.dag <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if (is.dag(cgraph)) {
    return(cgraph)
  }
  if (is.pattern(cgraph)) {
  return( .pick_dag_from_pattern(cgraph))
  }
  if (is.pdag(cgraph)) {
    return(.pick_dag_from_pdag(cgraph))
  }
  if (is.pag(cgraph))
    stop("not implemented")

  # ok, now attempt to turn cgraph object into dag
  # check check to see if it is acyclic
  if (!is.cyclic(cgraph)) {
    # if it is directed, we then its a DAG!
    if (is.directed(cgraph)) {
      class(cgraph) <- .DAG_CLASS
      return(cgraph)
    }
    # if its nonlatent, its a pdag, so we can try to see if
    # theres a dag extension
    else if (is.nonlatent(cgraph)) {
      return(.pick_dag_from_pdag(cgraph))
    }
  }
  else
    stop("input contains a cycle, so it cannot be coerced to a dag")
}

as.pattern <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if (is.pattern(cgraph))
    return(cgraph)
  if (is.dag(cgraph))
    return(.dag_to_pattern(cgraph))
  if (is.pdag(cgraph))
    return(.pick_dag_from_pdag(cgraph))
  if (is.pag(cgraph))
    stop("pags cannot be converted to patterns.")

  # ok, now attempt to turn cgraph object into pattern
  # check check to see if it is acyclic
  if (!is.cyclic(cgraph)) {
    # if it is directed, we then its a DAG and we
    # need to convert it to a pattern
    if (is.directed(cgraph)) {
      return(.dag_to_pattern(cgraph))
    }
    # if its nonlatent, its a pdag, so we can try to see if
    # theres a dag extension
    else if (is.nonlatent(cgraph)) {
      return(.pattern_from_pdag(cgraph))
    }
  }
  else
    stop("input contains a cycle, so it cannot be coerced to a dag")
}

as.pdag <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if (is.pdag(cgraph))
    return(cgraph)
  if (is.pattern(cgraph)) {
    class(cgraph) <- .PDAG_CLASS
    return(cgraph)
  }
  if (is.dag(cgraph)) {
    class(cgraph) <- .PDAG_CLASS
    return(cgraph)
  }
  if(is.pag(cgraph))
    stop("not implemented")

  # now, try to turn cgraph into a pdag
  if (is.nonlatent(cgraph)) {
    if (!is.cyclic(cgraph)) {
      class(cgraph) <- .PDAG_CLASS
      return(cgraph)
    }
  }
  else
    stop("input contains a cycle, so it cannot be coerced to pdag")
}

as.pag <- function(cgraph) {
  stop("not implemented")
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

  .calculate_adjacencies_from_edges(graph$edges, graph$nodes)

  # check to see if the graph is simple
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