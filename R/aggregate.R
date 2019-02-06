#' @export
aggregate_graphs <- function(cgraphs, method = c("frequentist", "bayesian"),
                                      df = NULL)
{
  if(!is.list(cgraphs))
    stop("dags is not as list")
  if (length(cgraphs) == 1)
    stop("dags is of length 1")

  base <- cgraphs[[1]]
  # see if all the graphs have the EXACT same nodes
  same_nodes <- lapply(cgraphs, function(cgraph) {
    isTRUE(all.equal(base$nodes, cgraph$nodes))
  })
  same_nodes <- isTRUE(all.equal(unlist(same_nodes), rep(T, length(cgraphs))))
  if (!same_nodes)
    stop("Not all the graphs have the same nodes")

  method <- match.arg(method)
  bs.weights <- rep(0, length(cgraphs))
  if (method == "frequentist") {
    bs.weights <- rep(1, length(cgraphs))
  }
  if (method == "bayesian") {
    df <- as.data.frame(lapply(df, function(x) { (x - mean(x))/stats::sd(x) }))
    for (i in 1:length(cgraphs)) {
      graph <- as.dag(cgraphs[[i]])
      bs.weights[i] <- score(graph, df)
    }
    bs.weights <- exp(-.5*(bs.weights - min(bs.weights)))
  }
  bs.weights <- round(bs.weights, digits = 5)
  cgraphs <- lapply(cgraphs, function(cgraph) {
    .prepare_cgraph_for_call(cgraph, F, T, T)
  })

  table <- .Call("cf_aggregate_cgraphs", cgraphs, bs.weights)
  table <- as.data.frame(table)

  cgraph <- cgraphs[[1]]
  table[[1]] <- cgraph$nodes[table[[1]]]
  table[[2]] <- cgraph$nodes[table[[2]]]
    names(table) <- c("node1","node2", "<--", "---", "-->", "<~~",
                      "~~>", "<++", "++>","<-o", "o->", "<->", "o-o")

  table <- table[, c(T, T, colSums(table[, -(1:2)]) != 0)]

  output <- list(nodes = cgraph$nodes, table = table)
  class(output) <- c("aggregated-cgraphs")
  return(output)
}

#' @export
vote <- function(aggregated.cgraphs) {
  nodes     <- aggregated.cgraphs$nodes
  table     <- aggregated.cgraphs$table
  table$" " <- 1 - rowSums(table[, -(1:2)])

  arrows <- names(table[, -(1:2)])[max.col(table[, -(1:2)])]

  edge <- function(node1, node2, arrow) {
    if (arrow == "<--")
      return(c(node2, node1, "-->"))
    if (arrow == "<~~")
      return(c(node2, node1, "~~>"))
    if (arrow == "<++")
       return(c(node2, node1, "++>"))
    if (arrow == "<-o")
      return(c(node2, node1, "o->"))
    if (arrow == " ")
      return(NULL)
    return(c(node1, node2, arrow))
  }
  edges <- c()
  for (i in 1:length(arrows))
    edges <- rbind(edges, edge(table[i, 1], table[i, 2], arrows[i]))
  return(cgraph(nodes, edges))
}
