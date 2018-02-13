aggregate_graphs <- function(dags, raw = FALSE, no_pags = TRUE) {
  if(!is.list(dags))
    stop("dags is not as list")
  if (length(dags) == 1)
    stop("dags is of length 1")
  if(!is.logical(raw))
    stop("raw does not take on a logical value")

  base <- dags[[1]]
  for(dag in dags) {
    if (!isTRUE(all.equal(base$nodes, dag$nodes)))
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
  table <- .Call("c_dag_to_rbt", dags)

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
  output <- list(names = dag$nodes, table = table)
  class(output) <- c("aggregated-pdags")
  return(output)
}

vote <- function(agg_pdags, threshold = .5, method = c("plurality", "majority",
                  "relative_majority", "sq_relative_majority"))
  {

  plurality <- function(x) {
    return(match(max(x), x))
  }

  majority <- function(x) {
    index <- x[x > .5]
    if (length(index) == 0)
      return(0)
    else
    return(index)
  }

  relative_majority <- function(x) {
    for (i in 1:length(x)) {
      if (x[i] > sum(x[-i]))
        return(i)
    }
    return(0)
  }

  square_relative_majority <- function(x, y, z) {
    for (i in 1:length(x)) {
      if (x[i]^2 > sum(x[-i]^2))
        return(i)
      }
    return(0)
  }

  method <- match.arg(method)

  voting_method <-
    switch (method,
      "plurality"                = plurality,
      "majority"                 = majority,
      "relative_majority"        = relative_majority,
      "square_relative_majority" = square_relative_majority
    )
  calculate_edge <- function(src, dst, x) {
    # these need to be chars because R is dumb
    switch (x,
      "0" = c(src, dst, "---"),
      "1" = c(dst, src, "-->"),
      "2" = c(src, dst, "-->"),
      "3" = c(src, dst, "---")
    )
  }

  df <- agg_pdags$table
  df <- df[ifelse ( rowMeans(df[,3:5]) > threshold , T , F),]
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
  adjacencies <- .calculate_adjcanencies_from_edges(edges, nodes)
  return(cgraph(nodes, adjacencies, edges))
}

