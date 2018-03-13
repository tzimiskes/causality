aggregate_graphs <- function(cgraphs, raw = FALSE, no_pags = TRUE) {
  if(!is.list(cgraphs))
    stop("dags is not as list")
  if (length(cgraphs) == 1)
    stop("dags is of length 1")
  if(!is.logical(raw))
    stop("raw does not take on a logical value")

  base <- cgraphs[[1]]
  # see if all the graphs have the EXACT same nodes
  same_nodes <- lapply(cgraphs, function(cgraph) {
    isTRUE(all.equal(base$nodes, cgraph$nodes))
  })
  same_nodes <- isTRUE(all.equal(unlist(same_nodes), rep(T, length(cgraphs))))
  if (!same_nodes)
    stop("Not all the graphs have the same nodes")

  cgraphs <- lapply(cgraphs, function(cgraph) {
    .prepare_cgraph_for_call(cgraph, F, T, T)
  })

  table <- .Call("c_dag_to_rbt", cgraphs)
  print(table)
  table <- as.data.frame(table)

  cgraph <- cgraphs[[1]]
  table[[1]] <- cgraph$nodes[table[[1]]]
  table[[2]] <- cgraph$nodes[table[[2]]]
  if (ncol(table) == 5) {
    names(table) <- c("node1", "node2", "<--", "-->", "---")
    if(raw == FALSE) {
      table[[3]] <- table[[3]]/length(cgraphs)
      table[[4]] <- table[[4]]/length(cgraphs)
      table[[5]] <- table[[5]]/ length(cgraphs)
    }
  }
  else {
    # TODO(arix)
  }
  output <- list(nodes = cgraph$nodes, table = table)
  class(output) <- c("aggregated-pdags")
  return(output)
}

vote <- function(agg_pdags, threshold = .5, method = c("plurality", "majority",
                  "relative_majority", "square_relative_majority"))
{
  plurality <- function(x) {
    max <- max(x)
    if (length(max) > 1)
      return(0)
    else
      return(which(max, x))
  }

  majority <- function(x) {
    for(i in 1:length(x) ) {
      if (x[i] > .5)
        return(i)
    }
    return(0)
  }

  relative_majority <- function(x) {
    for (i in 1:length(x)) {
      if (x[i] > sum(x[-i]))
        return(i)
    }
    return(0)
  }

  square_relative_majority <- function(x) {
    for (i in 1:length(x)) {
      if (x[i]^2 > sum(x[-i]^2))
        return(i)
    }
    return(0)
  }

  method <- match.arg(method)

  voting_method <- switch (method,
                           "plurality"                = plurality,
                           "majority"                 = majority,
                           "relative_majority"        = relative_majority,
                           "square_relative_majority" = square_relative_majority
  )

  calculate_edge <- function(src, dst, x) {
    # these need to be chars because R is dumb
    return(switch (as.character(x),
                   "0" = c(src, dst, "---"),
                   "1" = c(dst, src, "-->"),
                   "2" = c(src, dst, "-->"),
                   "3" = c(src, dst, "---")
    ))
  }

  df <- agg_pdags$table
  df <- df[ifelse ( rowSums(df[,3:5]) > threshold , T , F),]
  nodes <- agg_pdags$nodes
  n_edges <- nrow(df)
  if (n_edges == 0) {
    warning("Threshold too high, resulting graph is empty! Returning NA")
    return(NA)
  }
  edges <- matrix(data = "", nrow = n_edges, ncol = 3)
  for (i in 1:n_edges) {
    edges[i,] <- calculate_edge(df[i,1], df[i,2], voting_method(df[i, 3:5]))
  }
  adjacencies <- .calculate_adjacencies_from_edges(edges, nodes)
  return(cgraph(nodes, adjacencies, edges))

}
