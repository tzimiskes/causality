#include "headers/causality.h"
#include "headers/edgetypes.h"
const char* DIRECTED_STR      = "-->";
const char* UNDIRECTED_STR    = "---";
const char* PLUSPLUSARROW_STR = "++>";
const char* SQUIGGLEARROW_STR = "~~>";
const char* CIRCLEARROW_STR   = "o->";
const char* CIRCLECIRCLE_STR  = "o-o";
const char* BIDIRECTED_STR    = "<->";

int match_edge(const char* edge_str);
int match_node(const char* node, const char** nodes, int n_nodes);

int* calculate_edges_ptr(SEXP Graph) {

  SEXP Edges = PROTECT(VECTOR_ELT(Graph, EDGES));
  SEXP Nodes = PROTECT(VECTOR_ELT(Graph, NODES));

  int n_nodes         = length(Nodes);
  const char**  nodes = malloc(n_nodes*sizeof(char*));
  // make a table so we can easily refer to the nodes
  for(int i = 0; i < n_nodes; ++i) {
    nodes[i] = CHAR(STRING_ELT(Nodes, i));
  }


  int n_row = nrows(Edges);
  int n_col = ncols(Edges);

  int* edges_ptr    = malloc(n_row*n_col*sizeof(int));

  for(int i = 0; i < 2*n_row; ++i)
    edges_ptr[i] = match_node(CHAR(STRING_ELT(Edges, i)), nodes, n_nodes);
  for(int i = 2*n_row; i < 3*n_row; ++i)
    edges_ptr[i] = match_edge(CHAR(STRING_ELT(Edges, i)));

  free(nodes);

  UNPROTECT(2);
  return(edges_ptr);
}

int match_node(const char* node, const char** nodes, int n_nodes) {
  for(int i = 0 ; i < n_nodes; ++i) {
    if(!strcmp(nodes[i], node))
      return i;
  }
  error("Failed to match node\n"); /* This should never happen */
}

int match_edge(const char* edge_str) {
  if(!strcmp(edge_str, DIRECTED_STR))
    return DIRECTED;
  if(!strcmp(edge_str, UNDIRECTED_STR))
    return UNDIRECTED;
  if(!strcmp(edge_str, PLUSPLUSARROW_STR))
    return PLUSPLUSARROW;
  if(!strcmp(edge_str, SQUIGGLEARROW_STR))
    return SQUIGGLEARROW;
  if(!strcmp(edge_str, CIRCLEARROW_STR))
    return CIRCLEARROW;
  if(!strcmp(edge_str, CIRCLECIRCLE_STR))
    return CIRCLECIRCLE;
  if(!strcmp(edge_str, BIDIRECTED_STR))
    return BIDIRECTED;

  error("Unrecognized edge type!"); /* This will probably never happen */
}

