# cgraph.R contains implementations for the core causality classe(s)
# Author: Alexander Rix (arix@umn.edu)

# Casusality Graph Definitons --------------------------------------------------

#' Casuality Graphs
#'
#' Create or test for objects of type "causality.graph"
#' @param nodes A character array of node names
#' @param edges A \eqn{m x 3} character matrix. Each row is an edge in the form
#'   of (node1, node2, edgetype), with node1 and node2 being in nodes. Valid
#'   edge types are listed below
#' @param validate logical number to determine whether or not to check to see
#'   if the cgraph is valid before returning it. Default is \code{TRUE}
#' @param graph A graph to coerced or tested
#' @details A causality-graph consists of three things
#'   \itemize{
#'     \item nodes: a character vector of the nodes of the in the causal graph
#'     \item adjacencies a list of character vectors that contain the
#'       adjacencies of each node. This is calculated when a cgraph is created.
#'     \item edges a m x 3 character matrix which represents the edges in a
#'       causal graph in the form (from, to, edge). For example, if we are
#'       dealing with a causal graph regarding drug use and cancer, The edge
#'       "Smoking --> Cancer" would be stored as ("Smoking", "Cancer", "-->")
#'       in the edge matrix
#'   }
#'   The valid edges types for non latent variable model graphs
#'   (DAGs, PDAGs, Patterns) are:
#'   \itemize{
#'     \item \code{-->}
#'     \item \code{---}
#'   }
#'   And for latent variable models (PAGs, MAGs):
#'   \itemize{
#'     \item \code{o-o}
#'     \item \code{o->}
#'     \item \code{++>}
#'     \item \code{~~>}
#'     \item \code{<->}
#'   }
#'
#'
#' @return \code{cgraph} returns object of class "causality.graph", or an error
#'   if the graph is invalid.
#' @author Alexander Rix
#' @examples TODO(arix)
#' @references
#'   Spirtes et al. “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#'
#'  Spirtes P. Introduction to causal inference.
#'  Journal of Machine Learning Research. 2010;11(May):1643-62.
#'
#'   Pearl, Judea. Causality. Cambridge university press, 2009.
#' @family causality graph types
cgraph <- function(nodes, edges, validate = T) {
  if(!is.logical(validate))
    stop("validate must take on a logical value")
  adjacencies <- .calculate_adjacencies_from_edges(edges, nodes)

  graph <- structure(
    list(nodes = nodes, adjacencies = adjacencies, edges = edges),
    class = .CGRAPH_CLASS)
  if(validate) {
    if (!is_valid_cgraph(graph))
      stop("Input is not a valid causality graph")
  }
  return(graph)
}

#' @details \code{is_valid_cgraph} checks to see if the input is a valid
#' "causality.graph." Specifally, it checks that there are no duplicate nodes,
#' as well as if the input is simple (no self-loops and is not a multi-graph).
#' @usage is_valid_cgraph(graph)
#' @rdname cgraph
#' @return \code{is_valid_cgraph} returns \code{TRUE} or \code{FALSE} depending
#'   on whether or not the input is valid
is_valid_cgraph <- function(graph) {
  # check to make sure it has valid fields (in the right order)
  if(!isTRUE(all.equal(c("nodes", "adjacencies","edges"), names(graph)))) {
    message("graph does not contain the appropriate fields")
    return(FALSE)
  }
  if (!(is.character(graph$edges)) & ncol(graph$edges) == 3) {
    message("graph edges is not a character matrix")
    return(FALSE)
  }
  if (!is.character(graph$nodes)) {
    message("graph nodes is not a character array")
    return(FALSE)
  }
  # check to make sure that there are no duplicate nodes
  nodes <- sort(graph$nodes)
  for (i in 1:(length(nodes)-1)) {
    if ( nodes[i] == nodes[i + 1]) {
      message("graph contains duplicate nodes")
      return(FALSE)
    }
  }

  # determine whether the graph is simple (No self-loops, no multi-edges)
  # as well as making sure all nodes in the edges show up in nodes
  n_edges <- nrow(graph$edges)
  parents = list() # this should probably be renamed; unclear
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
    else # if the edge isn't in the list, add it
      parents[[edge[2]]][[edge[1]]] <- 1
    if (!(edge[1] %in% graph$nodes) || (!edge[2] %in% graph$nodes)) {
      message("graph contains nodes that are not in the node list")
      return(FALSE)
    }
  }
  return(TRUE)
}



# adjs <- .calculate_adjacencies_from_edges(graph$edges, graph$nodes)
# for(node in names(adjs)) {
#   calculated_node_adjs <- adjs[[node]]
#   listed_node_adjs     <- graph$adjacencies[[node]]
#   intersection         <- intersect(calculated_node_adjs, listed_node_adjs)
#   if (!isTRUE(all.equal(intersection, listed_node_adjs)))
#     stop("adjacencies do not not match the nodes and edge!")
# }

# these hidden (lol) variables are used to assign (sub)classes to craph objects
.CGRAPH_CLASS  <- c(                     "causality.graph")
.DAG_CLASS     <- c("causality.dag"    , "causality.graph")
.PDAG_CLASS    <- c("causality.pdag"   , "causality.graph")
.PATTERN_CLASS <- c("causality.pattern", "causality.graph")
.PAG_CLASS     <- c("causality.pag"    , "causality.graph")
# Edge types currently used in Causality Graphs
.DIRECTED       <- "-->"
.UNDIRECTED     <- "---"
.PLUSPLUS       <- "++>"
.SQUIGGLE       <- "~~>"
.CIRCLEDIRECTED <- "o->"
.CIRCLECIRCLE   <- "o-o"
.BIDIRECTED     <- "<->"

# edges that show up in pags
.LATENT_EDGE_TYPES    <- c(.DIRECTED, .SQUIGGLE, .PLUSPLUS, .CIRCLEDIRECTED,
                           .CIRCLECIRCLE)
#edges that show up in pdags
.NONLATENT_EDGE_TYPES <- c(.DIRECTED, .UNDIRECTED)



# Casusality Graph is.* Functions ----------------------------------------------
 #' @usage is.cgraph(graph)
 #' @details \code{is.cgraph} tests whether or not an object has the class
 #'   causality.graph
 #'   @return \code{is.cgraph} returns \code{TRUE} or \code{FALSE}.
 #' @rdname cgraph
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
    stop("input is not a causality.graph")
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

# Causality Graph as.dag Functions ---------------------------------------------

as.dag <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if(is.dag(cgraph))
    return(cgraph)
}

as.dag.causality.graph <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")

  if (is.nonlatent(cgraph)) {
    if (!is.cyclic(cgraph)) {
      if (is.directed(cgraph)) {
        class(cgraph) <- .DAG_CLASS
        return(cgraph)
      }
      else { # we have a a pdag
       dag <- .dag_from_pdag(cgraph)
       if (is.null(dag))
         warning("Unable to coerce input to causality.dag")
       return(dag)
      }
    }
    else { # cylcic, so we can't coerce it
      warning("Unable to coerce input to causality.dag")
      return(NULL)
    }
  }
  else if (is.latent(cgraph)) {
    stop("Not implemented")
  }
  else {
    stop("Unrecognized graph! Can't coerce!")
  }
}

as.dag.causality.pdag <- function(cgraph) {
  if (!is.pdag(cgraph))
    stop("input is not a causality.graph")

  dag <- .dag_from_pdag(cgraph)
  if (is.null(dag)) {
    warning("Unable to coerce input to causality.dag")
  }
  return(dag)
}

as.dag.causality.pattern <- function(cgraph) {
  if (!is.pattern(cgraph))
    stop("input is not a causality.pattern")
  return(.dag_from_pattern(cgraph))
}

as.dag.causality.pag <- function(cgraph) {
  if (!is.pag(cgraph))
    stop("input is not a causality.pag")
  stop("Not implemented")
}

# Causality Graph as.pattern Functions -----------------------------------------

as.pattern <- function(cgraph) {
  if (!is.cgraph(cgraph))
    stop("input is not a cgraph")
  if (is.pattern(cgraph))
    return(cgraph)
}

as.pattern.causality.dag <- function(cgraph) {
  if (!is.dag(cgraph))
    stop("input is not a dag")
  return(.dag_to_pattern(cgraph))
}

as.pattern.causality.pdag <- function(cgraph) {
  if (!is.pdag(cgraph))
    stop("input is not a pdag")
    return(.dag_from_pdag(cgraph))

}

as.pattern.causality.pag <- function(cgraph) {
  if (!is.pag(cgraph))
    stop("Input is not a pag")
  stop("Not Implemented")
}

as.pattern.causality.graph <- function(cgraph) {
  if (is.nonlatent(cgraph)) {
    if (!is.cyclic(cgraph)) {
      dag <- .dag_from_pdag(cgraph)
      if (is.null(dag)) {
        stop("Input cannot be converted to a pattern")
      }
      else
        return(.dag_to_pattern(dag))
    }
    else {
      stop("input contains a cycle, so it cannot be converted to a pattern")
    }
  }
}

# Causality Graph as.pdag Functions --------------------------------------------

as.pdag <- function(cgraph) {
  if(!is.cgraph(cgraph))
    stop("Input is not a causality graph")
  if(is.pdag(cgraph))
    return(cgraph)

  if (is.nonlatent(cgraph)) {
    if (!is.cyclic(cgraph)) {
      class(cgraph) <- .PDAG_CLASS
      return(cgraph)
    }
  }
  else
    stop("input contains a cycle, so it cannot be coerced to pdag")
}

as.pdag.causality.dag <- function(cgraph) {
  if(!is.dag(cgraph))
    stop("Input is not a causality dag")
  class(cgraph) = .PDAG_CLASS
  return(cgraph)
}

as.pdag.causality.dag <- function(cgraph) {
  if(!is.pattern(cgraph))
    stop("Input is not a causality pattern")
  class(cgraph) = .PDAG_CLASS
  return(cgraph)
}

as.pdag.causality.pag <- function(cgraph) {
  if(!is.pag(cgraph))
    stop("Input is not a causality pag")

    stop("not implemented")
}

# Causality Graph as.pag Functions ---------------------------------------------
as.pag <- function(cgraph) {
  stop("not implemented")
}




# Causality Graph as.cgraph Functions ------------------------------------------

#' @details \code{as.cgraph} is an S3 generic that attempts to convert a not
#' causality.cgraph to a "causalality.graph". It currently supports turning "bn"
#' objects and r-causal objects to "causality.graphs".
#' @usage as.cgraph(graph)
#' @rdname cgraph
as.cgraph <- function(graph) {
    UseMethod("as.cgraph")
}

as.cgraph.causality.graph <- function(graph) {
  return(graph)
}

as.cgraph.default <- function(graph) {
  if (is_valid_cgraph(graph)) {
    class(graph) <- .CGRAPH_CLASS
    return(graph)
  }
  else
    stop("Cannot coerce input to causality.graph")
}

# rcausal uses different classes for each algorithm, this makes it necessary to
# create this 'dummy' function to handle converting the algorithm output to
# causality
.as.cgraph.rcausal <- function(graph) {

  edges <- graph$edges
  new_edges <- matrix("", nrow = length(edges), ncol = 3)

  for (i in 1:length(edges)) {
    edge <- strsplit(edges[i], " ")[[1]]
    new_edges[i, 1] <- edge[1]
    new_edges[i, 2] <- edge[3]
    if (length(edge) == 3)
      new_edges[i, 3] <- edge[2]
    else {
      # tetrad use X1 --> X2 nl pd/dd to mark unconfounded paths in pags
      # I think this is stupid, so we use ++>/ ~~> for dd/pd
      if(edge[5] == "dd")
        new_edges[i, 3] <- .PLUSPLUS # ++>
      else
        new_edges[i, 3] <- .SQUIGGLE # ~~>
    }
  }
  cgraph <- cgraph(graph$nodes, new_edges)

  return(cgraph)
}

# tetrad sucks. this exists to support the generic as.cgraph function
as.cgraph.fges <- as.cgraph.fges.discrete <- as.cgraph.fges.mixed   <-
  as.cgraph.gfci <- as.cgraph.gfci.discrete <- as.cgraph.gfci.mixed <-
  as.cgraph.pc   <- as.cgraph.cpc           <- as.cgraph.pcstable   <-
  as.cgraph.cpcstable <- .as.cgraph.rcausal

as.cgraph.bn <- function(graph) {
  if (!(class(graph) == "bn"))
    stop("Input is not of class bn!")

  names <- names(graph$nodes)
  # get the edges
  edges <- cbind(unname(graph$arcs), "-->")
  return(cgraph(names, edges))
}