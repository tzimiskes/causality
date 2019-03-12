#' Read and write causality graphs
#'
#' \code{write_causality_graph} writes a causality graph to file
#'
#' \code{read_causality_graph} reads a causality graph from file
#' @param file The file you wish to read/write \code{graph} from/to
#' @param graph The graph you wish to save.
#' @note \code{file} will be output as a TETRAD compatible graph, and file must
#' be a TETRAD compatible graph if you wish to read it into R.
#' @examples
#' \dontrun{write_causality_graph(file = "sachs", sachs.dag)}
#' # Will throw an error if PATH_TO_FILE is invalid
#' \dontrun{graph <-read_causality_graph(file = "PATH_TO_FILE")}
#' @return \code{read_causality_graph} returns an object of class
#'     "causality.graph". An error will be thrown if there does not exist a
#'      valid path to file, graph is not a graph, or the file you wish to read
#'      does not contain a graph in TETRAD format.
#' @author Alexander Rix
#' @name causality-IO
#' @aliases NULL
NULL

#' @rdname causality-IO
#' @export
write_causality_graph <- function(file, graph) {
  if (!is.cgraph(graph))
    stop("graph must be a causality graph!")
  if (file.exists(file))
    warning(sprintf("File \"%s\" already exists; overwriting...\n", file))
  write("Graph Nodes:", file = file, append = F)
  cat(graph$nodes, file = file, append = T, sep = ",")
  write('\n', file = file, append = T)
  write("Graph Edges:", file = file, append = T)
  for (i in 1:nrow(graph$edges)) {
    x <- graph$edges[i, 1]
    y <- graph$edges[i, 2]
    e <- graph$edges[i, 3]
    write(sprintf("%i. %s %s %s", i, x, e, y), file = file, append = T)
  }
}

#' @rdname causality-IO
#' @export
read_causality_graph <- function(file) {
   if (!file.exists(file))
     stop("Cannot find file!")
     tmp <- readLines(file)
     if (tmp[1] != "Graph Nodes:")
        stop("file does not contain a compatible graph")
     nodes <- unlist(strsplit(tmp[2], split = ","))
     nodes <- sort(nodes)
     if (tmp[4] != "Graph Edges:")
        stop("file does not contain a compatible graph")
    tmp <- tmp[-(1:4)]
     edges <- c()
     # process the file one line at a time
     edges <- c()
     line <- tmp[1]
     while (!is.na(line) && line != "") {
         line <- sub("[0-9]+\\. ", "", line)
         edge <- unlist(strsplit(line, split = " "))
         if (is.na(sum(match(c(edge[1], edge[3]), nodes)))) {
            print(line)
            stop ("file contains a malformed causality graph")
         }
        edges <- c(edges, c(edge[1], edge[3], edge[2]))
        tmp  <- tmp[-1]
        line <- tmp[1]
     }
     cgraph(nodes, matrix(edges, ncol = 3, byrow = T))
}
