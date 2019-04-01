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
#' @param validate logical value to determine whether or not to check to see
#'   if the cgraph is valid before returning it. Default is \code{TRUE}
#' @param graph A graph to coerced or tested
#' @details A causality-graph consists of three things
#'   \itemize{
#'     \item nodes: a character vector of the nodes of the in the causal graph
#'     \item adjacencies: a list of character vectors that contain the
#'       adjacencies of each node. This is calculated when a cgraph is created.
#'     \item edges: a \eqn{m x 3} character matrix which represents the edges in
#'       a causal graph in the form (from, to, edge). For example, if we are
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
#'     \item \code{++>} (in Tetrad this is known as --> dd nl)
#'     \item \code{~~>} (in Tetrad this is known as --> pd nl)
#'     \item \code{<->}
#'   }
#'
#' @return \code{cgraph} returns object of class "causality.graph", or an error
#'   if the graph is invalid.
#' @author Alexander Rix
#' @examples
#' nodes <- c("X1", "X2", "X3", "X4")
#' edges <- matrix(c("X1", "X2", "-->",
#'                   "X3", "X2", "-->",
#'                   "X4", "X1", "---",
#'                   "X4", "X3", "-->",
#'                   "X4", "X2", "-->"), ncol = 3, byrow = T)
#' graph <- cgraph(nodes, edges)
#'
#' # cgraph defaults to validate = TRUE, but you can check validity by calling
#' is_valid_cgraph(graph)
#'
#' # you can coerce graphs from package \code{bnlearn} to causality.graphs
#' \dontrun{
#'   library(bnlearn)
#'   sachs <- as.cgraph(mmhc(sachs.df))
#' }
#' @references
#'   Spirtes et al. “Causation, Prediction, and Search.”, Mit Press,
#'   2001, p. 109.
#'
#'   Spirtes P. Introduction to causal inference.
#'   Journal of Machine Learning Research. 2010;11(May):1643-62.
#'
#'   Pearl, Judea. Causality. Cambridge university press, 2009.
#' @seealso
#' Other causality classes: \code{\link{dag}}, \code{\link{pattern}}
#' coercing non causality graphs to causality.graphs : \code{\link{as.cgraph}}
#' @export
cgraph <- function(nodes, edges, validate = TRUE)
{
    if (!is.character(nodes))
        stop("nodes is not a character vector.")
    if (!is.character(edges))
        stop("edges is not a character vector.")

    if (!is.matrix(edges)) {
        if (length(edges) %% 3 == 0)
            edges <- as.matrix(edges, ncol = 3 , byrow = T)
        else
            stop ("edges cannot be converted to a n x 3 character matrix.")
    }
    if (dim(edges)[2] != 3)
        stop("graph edges does not have three columns.")
    for (edge in edges[,3])
        if (!(edge %in% EDGES))
            stop(sprintf("Unrecognized edge type %s\n", edge))
    if (!is.logical(validate))
        stop("validate must take on a logical value")
    adjacencies <- .calculate_adjacencies(edges, nodes)
    graph <- structure(list(nodes = nodes, adjacencies = adjacencies,
                            edges = edges), class = .CGRAPH_CLASS)
    if (validate && !is_valid_cgraph(graph))
        stop("Input is not a valid causality graph")
    return(graph)
}

#' @details \code{is_valid_cgraph} checks to see if the input is a valid
#'   "causality.graph." Specifically, it checks that there are no duplicate
#'    nodes, self-loops, or multiple edges between pairs of nodes.
#' @usage is_valid_cgraph(graph)
#' @rdname cgraph
#' @return \code{is_valid_cgraph} returns \code{TRUE} or \code{FALSE} depending
#'   on whether or not the input is valid.
#' @export
is_valid_cgraph <- function(graph)
{
    # check to make sure it has valid fields (in the right order)
    if (!isTRUE(all.equal(c("nodes", "adjacencies", "edges"), names(graph)))) {
        warning("graph does not contain the appropriate fields")
        return(FALSE)
    }
    if (!is.character(graph$edges) || !is.matrix(graph$edges)) {
        warning("graph edges is not a character matrix.")
        return(FALSE)
    }
    if (dim(graph$edges)[2] != 3) {
        warning("graph edges does not have three columns.")
        return(FALSE)
    }
    if (!is.character(graph$nodes)) {
        warning("graph nodes is not a character array.")
        return(FALSE)
    }
    # check to make sure that there are no duplicate nodes
    if (length(unique(graph$nodes)) < length(graph$nodes)) {
        warning("graph contains duplicate nodes.")
        return(FALSE)
    }
    # determine whether the graph is simple (No self-loops, no multi-edges)
    # as well as making sure all nodes in the edges show up in nodes
    nedges <- nrow(graph$edges)
    parents <- list()
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

# edges that show up in PAGs ~~>, ++>, o->, o-o, <->
.LATENT_EDGE_TYPES <- c(.SQUIGGLE, .PLUSPLUS, .CIRCLEDIRECTED, .CIRCLECIRCLE,
                                   .BIDIRECTED
                       )
# edges that show up in PDAGs: -->, ---
.NONLATENT_EDGE_TYPES <- c(.DIRECTED, .UNDIRECTED)

# edges of the type -->, ~~>, ++>, o->
.DIRECTED_EDGE_TYPES <- c(.DIRECTED, .SQUIGGLE, .PLUSPLUS, .CIRCLEDIRECTED)

.EDGE_TYPES <- c(.LATENT_EDGE_TYPES, .DIRECTED_EDGE_TYPES)
# Casusality Graph is ----------------------------------------------------------
#' @usage is.cgraph(graph)
#' @details \code{is.cgraph} tests whether or not an object has the class
#'   causality.graph
#' @return \code{is.cgraph} returns \code{TRUE} or \code{FALSE}.
#' @rdname cgraph
#' @export
is.cgraph <- function(graph)
{
    if (.CGRAPH_CLASS %in% class(graph))
        return(TRUE)
    else
        return(FALSE)
}

# Casusality Graph summary -----------------------------------------------------
#' @usage \\method{summary}{causality.graph}(object, ...)
#' @param object A causality.graph
#' @param ... additional (unused) arguments to pass to \code{summary}
#' @details \code{summary} provides basic summary statistics about \code{graph},
#'   like average degree, max degree, number of directed/undirected eges etc.
#' @rdname cgraph
#' @export
summary.causality.graph <- function(object, ...)
{
    if (!is.cgraph(object))
        stop("object is not a causality.graph!")
    summary <- list()
    summary$n.nodes <- length(object$nodes)
    summary$n.edges <- nrow(object$edges)
    summary$n.directed.edges   <- sum(object$edge[,3] %in% .DIRECTED_EDGE_TYPES)
    summary$n.undirected.edges <- summary$n.edges - summary$n.directed.edges
    summary$average.degree     <- 2 * summary$n.edges / summary$n.nodes
    summary$max.degree    <- max(unlist(lapply(object$adjacencies, length)))
    summary$max.indegree  <- max(unlist(lapply(parents(object), length)))
    summary$max.outdegree <- max(unlist(lapply(children(object), length)))
    return(summary)
}

# Causality Graph as.cgraph Functions -----------------------------------------
#' Coerce a graph to a Causality Graph
#' @param graph A (non causality.graph) graph you'd like to attempt to convert
#'   into a causality.graph
#' @details \code{as.cgraph} is an S3 generic that attempts to convert a not
#'   causality.cgraph to a "causalality.graph". It currently supports turning
#'   "bn" objects and rcausal graphs into causality graphs.
#' @usage as.cgraph(graph)
#' @return \code{as.cgraph} returns a causality graph object, or NULL.
#' @seealso See \code{\link{cgraph}} for the documentation of object
#' @export
as.cgraph <- function(graph)
{
    UseMethod("as.cgraph")
}

#' @rdname as.cgraph
#' @export
as.cgraph.causality.graph <- function(graph)
{
    return(graph)
}

#' @rdname as.cgraph
#' @export
as.cgraph.default <- function(graph)
{
    if (isTRUE(all.equal(names(graph), c("graph", "nodes", "edges"))))
        return(as.cgraph.rcausal(graph))
    if (is_valid_cgraph(graph)) {
        class(graph) <- .CGRAPH_CLASS
        return(graph)
    }
    else
        warning("Cannot coerce input to causality.graph, returning NULL")
    return(NULL)
}

#' @rdname as.cgraph
#' @export
as.cgraph.bn <- function(graph)
{
    if (class(graph) != "bn")
        stop("graph is not of class bn.")
    nodes <- names(graph$nodes)
    edges <- c()
    for (node in nodes) {
        parents  <- graph[["nodes"]][[node]][["parents"]]
        children <- graph[["nodes"]][[node]][["children"]]
        nbr      <- graph[["nodes"]][[node]][["nbr"]]
        spouses  <- setdiff(nbr, c(parents, children))
        for (parent in graph[["nodes"]][[node]][["parents"]])
        edges <- c(edges, c(parent, node, .DIRECTED))
        for (spouse in spouses) {
            if (node < spouse)
                edges <- c(edges, c(spouse, node, .UNDIRECTED))
        }
    }
    return(cgraph(nodes, matrix(edges, ncol = 3, byrow = T)))
}

as.cgraph.rcausal <- function(graph)
{
    edges <- graph$edges
    new_edges <- matrix("", length(edges), 3)
    for (i in 1:length(edges)) {
        edge <- strsplit(edges[i], " ")[[1]]
        new_edges[i, 1] <- edge[1]
        new_edges[i, 2] <- edge[3]
        if (length(edge) == 3)
            new_edges[i, 3] <- edge[2]
        else {
            # tetrad uses X1 --> X2 nl pd/dd to mark unconfounded paths in pags
            # causality uses ++>/ ~~> for dd/pd
            if (edge[5] == "dd")
                new_edges[i, 3] <- .PLUSPLUS # ++>
            else
                new_edges[i, 3] <- .SQUIGGLE # ~~>
            }
    }
    cgraph <- cgraph(graph$nodes, new_edges)
    return(cgraph)
}
