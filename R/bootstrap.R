aggregate_graphs <- function(dags, raw = FALSE, no_pags = TRUE) {
  if(!is.list(dags))
    stop("dags is not as list")
  if (length(dags) == 1)
    stop("dags is of length 1")
  if(!is.logical(raw))
    stop("raw does not take on a logical value")

  base <- dags[[1]]
  for(dag in dags) {
    if (!isTRUE(all.equal(base$names, dag$nodes)))
      stop('at least one of the dags contains a different names
           field than the others')
  }

  dags <- lapply(dags, function(dag) {
  if(!is.cgraph(dag)) {
    stop("input dag is not of type cgraph")
  }
  # creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(dag$nodes))
    hash[[dag$nodes[[i]]]] <- i - 1
  for (i in 1:nrow(dag$edges)) {
    dag$edges[i,1] <- hash[[dag$edges[i,1]]]
    dag$edges[i,2] <- hash[[dag$edges[i,2]]]
    if (dag$edges[i, 3] == "-->")
      dag$edges[i, 3] <- 1
    else if (dag$edges[i, 3] == "---")
      dag$edges[i, 3] <- 2
  }
  nc <- ncol(dag$edges)
  nr <- nrow(dag$edges)
  dag$edges <- as.integer(dag$edges)
  dim(dag$edges) <- c(nr, nc)
  return(dag)
  })
  time <- system.time(table <- .Call("c_dag_to_rbt", dags))

  table <- as.data.frame(table)
  dag <- dags[[1]]
  table[[1]] <- dag$nodes[table[[1]]]
  table[[2]] <- dag$nodes[table[[2]]]
  if (ncol(table) == 5) {
    names(table) <- c("node1", "node2", "<--", "-->", "---")
    if(raw == FALSE) {
    table[[3]] <- table[[3]]/length(dags)
    table[[4]] <- table[[4]]/length(dags)
    table[[5]] <- table[[5]]/ length(dags)
    }
  }
  else {
    # TODO(aritable)
  }
  print(time)
  output <- list(names = dag$nodes, table = table)
  class(output) <- c("aggregated-pdags")
  return(output)
}

foo <- function(agg_pdags, threshold) {

  df <- agg_pdags$table
  df <-df[which(df$'<--' + df$"---" +
                         df$"-->" >= threshold), names(df)]
  nodes <- agg_pdags$nodes
  n_edges <- nrow(df)
  edges <- matrix(data = "", nrow = n_edges, ncol = 3)
  for (i in 1:n_edges) {
    if(df[i, 3] > df[i, 4] + df[i, 5]) {
    edges[i , 1] <- df[i, 2]
    edges[i , 2] <- df[i, 1]
    edges[i , 3] <- "-->"
    }
    else if(df[i, 4] > df[i, 3] + df[i, 5]) {
      edges[i , 1] <- df[i, 1]
      edges[i , 2] <- df[i, 2]
      edges[i , 3] <- "-->"
    } else {
      edges[i , 1] <- df[i, 1]
      edges[i , 2] <- df[i, 2]
      edges[i , 3] <- "---"
    }
  }
  adjacencies <- .calculate_adjcanencies_from_edges(edges, nodes)
  return(cgraph(nodes, adjacencies, edges))
}

