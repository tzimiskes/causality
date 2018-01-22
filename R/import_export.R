bn_learn_to_cgraph <- function(bn_graph) {
  # converts a learned graph of bn-class to a grpah of cgraph-class
  if (!(class(bn_graph) == "bn")) stop("Input is not of class bn!")

  names <- names(bn_graph$nodes)
  # get the skeleton
  skeleton <- lapply(nm, function(x) {
    bn_graph$nodes[[x]]$nbr
  }
  )
  names(skeleton) <- names
  # get the edges
  edges <- cbind(unname(bn_graph$arcs), "-->")
  return(cgraph(names, skeleton, edges))
}


import_from_tetrad_file <- function(file, type = NULL) {

  # this function imports a tetrad graph
  # parse the tetrad file as a list of character vectors
  tmp_file <- read_lines(file)
  # get the names of the nodes from
  nodes <- strsplit(tmp_file[2], ",")[[1]]
  # strip out first 4 lines and last line
  tmp_file <- tmp_file[5:( length(tmp_file) - 1)]
  # remove the number of the edge
  tmp_file <- sub("[0-9]+.", "", tmp_file)
  # prep the lines so the edge matrix can be filled in easily
  tmp_file <- strsplit(trimws(tmp_file), " ")
  # get the number of edges
  n_edges <- length(tmp_file)
  # instantiant edge matrix
  edges <- matrix(nrow = n_edges, ncol = 3)
  # fill it in!
  for (i in 1:n_edges) {
    # if edge is of type <--, change it to --> and earn the user
    if(tmp_file[[i]][2] == "<--") {
      warning(sprintf("edge %i is of type '<--' in %s; converting to type '-->", i, file) )
      edges[i, 1] <- tmp_file[[i]][3]
      edges[i, 2] <- tmp_file[[i]][1]
      edges[i, 3] <- "-->"
    } else {
      edges[i, 1] <- tmp_file[[i]][1]
      edges[i, 2] <- tmp_file[[i]][3]
      edges[i, 3] <- tmp_file[[i]][2]
    }
  }
  # get the skeleton from the edge representation
  # loop over the nodes
  skeleton <- lapply(nodes, function(node) {
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
  names(skeleton) <- nodes
  tmp_cgraph <- cgraph(nodes, skeleton, edges)

  if(validate_cgraph(tmp_cgraph) == FALSE)
    stop("imported graph is not a valid cgraph object")

  # if subclass field isn't NULL, then add the subclass to class(tmp_cgraph).
  # coerce to subclass to character.
  if (!is.null(type)) {
    class(tmp_cgraph) <- c(as.character(type), class(tmp_cgraph))
  }
  return(tmp_cgraph)
}