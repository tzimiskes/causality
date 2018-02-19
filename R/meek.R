orient <- function(pdag) {
  #  if(!is.pdag(pdag))
  #    stop("Input is not a pdag!")
  nodes <- pdag$nodes
  edges <- pdag$edges

  n_edges <- nrow(edges)
  parents <- list()
  for (i in 1:nrow(edges)) {
    parents[[edges[i, 2]]][[edges[i, 1]]] <- edges[i, 3]
  }


  for (i in 1:n_edges) {
    parent <- edges[i, 1]
    child  <- edges[i, 2]
    if (edges[i, 3] == "---") {
      # look for chains
      for (grandparent in parents[[parent]]) {
        print(grandparent)
        if (parents[[parent]][[grandparent]] == "-->") {
          edges[i, 3] <- "-->"
          next
        }
      }
      # look for anti-cycles
      for (parent_of_child in parents[[child]]) {

        if (parents[[child]][[parent_of_child]] == "-->") {
          print(foo)
          for (grandparent in parents[[parent_of_child]])
            if (parents[[parent_of_child]][[grandparent]] == "-->") {
              edges[i, 3] <- "-->"
              next
            }
        }
      }
      # look for
    }
  }

  # look

  return(edges)
}

pick_dag_from_pdag <- function(pdag) {
  if(!is.pdag(pdag))
    stop("Input is not a pdag!")
}

bingo <- function(dag) {
  # creating a "hash table" makes the next operation faster
  hash <- list()
  for (i in 1:length(dag$nodes)) {
    hash[[dag$nodes[[i]]]] <- i - 1
  }
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

  print(dag)
  return(.Call("meek_rules", dag))

}