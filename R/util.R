# get the skeleton from the edge representation
# loop over the nodes
.calculate_adjacencies_from_edges <- function(edges, nodes) {
  n_edges <- nrow(edges)
  adjacency <- lapply(nodes, function(node) {
    neighborhood <- c()
    # loop over the edges
    for (i in 1:n_edges) {
      # if a node is in an edge, find its partner (neighbor)
      if( node %in% edges[i, 1:2]) {
        neighbor <- edges[i, c(node != edges[i,1:2], F)]
        if (!(neighbor %in% neighborhood))
          neighborhood <- c(neighborhood, neighbor)
      }
    }
    return(neighborhood)
  }
  )
  names(adjacency) <- nodes
  return(adjacency)
}

# I'm pretty sire this code is garbage (arix)
.prepare_cgraph_for_call <- function(cgraph, nodes, edges, adjacencies) {
  hash <- list()
  for (i in 1:length(cgraph$nodes))
    hash[[cgraph$nodes[[i]]]] <- i - 1

  if(edges) {
    for (i in 1:nrow(cgraph$edges)) {
      cgraph$edges[i, 1] <- hash[[cgraph$edges[i, 1]]]
      cgraph$edges[i, 2] <- hash[[cgraph$edges[i, 2]]]
      cgraph$edges[i, 3] <-
      switch(cgraph$edges[i, 3],
      # it is important that these edge/number pairs match up with edgetypes.h
             "-->" = 1,
             "---" = 2,
             "++>" = 3,
             "~~>" = 4,
             "o->" = 5,
             "o-o" = 6,
             "<->" = 7)
    }
    nc <- ncol(cgraph$edges)
    nr <- nrow(cgraph$edges)
    cgraph$edges <- as.integer(cgraph$edges)
    dim(cgraph$edges) <- c(nr, nc)
  }
  if(adjacencies) {
    cgraph$adjacencies <- lapply(cgraph$adjacencies, function(x) {
      as.integer(unname(unlist(hash)[x]))
    })
  }
  if(nodes)
    cgraph$nodes <- unname(unlist(hash))
  return(cgraph)
}
