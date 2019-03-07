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
#
# import_from_tetrad_file <- function(file, type = "cgraph", sort = F) {
#
#   # this function imports a tetrad graph
#   # parse the tetrad file as a list of character vectors
#   tmp_file <- read_lines(file)
#   # get the names of the nodes from
#   nodes <- strsplit(tmp_file[2], ",")[[1]]
#   if (sort)
#     nodes <- sort(nodes)
#   # strip out first 4 lines and last line
#   tmp_file <- tmp_file[5:( length(tmp_file) - 1)]
#   # remove the number of the edge
#   tmp_file <- sub("[0-9]+.", "", tmp_file)
#   # prep the lines so the edge matrix can be filled in easily
#   tmp_file <- strsplit(trimws(tmp_file), " ")
#   # get the number of edges
#   n_edges <- length(tmp_file)
#   # instantiant edge matrix
#   edges <- matrix(nrow = n_edges, ncol = 3)
#   # fill it in!
#   for (i in 1:n_edges) {
#     # if edge is of type <--, change it to --> and warn the user
#     if(tmp_file[[i]][2] == "<--") {
#       warning(sprintf("edge %i is of type '<--' in %s; converting to type '-->", i, file) )
#       edges[i, 1] <- tmp_file[[i]][3]
#       edges[i, 2] <- tmp_file[[i]][1]
#       edges[i, 3] <- "-->"
#     } else {
#       edges[i, 1] <- tmp_file[[i]][1]
#       edges[i, 2] <- tmp_file[[i]][3]
#       edges[i, 3] <- tmp_file[[i]][2]
#     }
#   }
#
#
#
#   tmp_cgraph <- cgraph(nodes, edges)
#   # check to see if input is a legal cgraph object
#   if(is_valid_cgraph(tmp_cgraph) == FALSE)
#     stop("imported graph is not a valid cgraph object")
#   # if type filed isn't empty, attempt to parse it as the proposed type
#   if(!is.null(type)) {
#     if(type == "dag") {
#       tmp_dag <- tryCatch(as.dag(tmp_cgraph), error = function(e) {message(paste(e)); NULL})
#       if(is.null(tmp_dag))
#          stop(sprintf("imported object is not a valid dag,
#                       so it cannot be imported as a dag.
#                       call the function with type = NULL to import it"
#                       ))
#       else
#         return(tmp_dag)
#     }
#     if(type == "cgraph") {
#       return(tmp_cgraph)
#     }
#   }
#   else
#     return(tmp_cgraph)
# }
