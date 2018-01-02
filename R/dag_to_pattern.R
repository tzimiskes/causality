dag_to_pattern <- function(dag) {
  if (!("dag" %in% class(dag)))
    stop("Input is not a dag!")
  if(("pattern" %in% class(dag)))
    stop("Input is already a pattern!")

  top_order <- .topoligical_sort(dag)
  ordered_edges <- .order_edges(dag, top_sort)



  #find-compelled
}

# Topological sorting algorithm is the one described in CLRS;
.topoligical_sort <- function(dag) {
  # generate the children of each node
  children <- list()
  for (i in 1:length(dag$edges[, 1])) {
    edge <- dag$edges[i,]
    if (edge[3] == "-->")
      children[[edge[1]]] <- c(children[[edge[1]]], edge[2])
    if (edge[3] == "<--")
      children[[edge[2]]] <- c(children[[edge[2]]], edge[1])
  }

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

.order_edges  <- function(dag, top_order) {
  ordered_edges <- matrix()
  return(ordered_edges)
}