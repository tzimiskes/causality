#include"headers/causality.h"

SEXP ccf_sort_wrapper(SEXP Graph) {

  int* edges_ptr = calculate_edges_ptr(Graph);

  int* sorted_nodes_ptr = ccf_sort(n_nodes, edges_ptr);

  SEXP Nodes = PROTECT(VECTOR_ELT(Graph, NODES));

  int n_nodes         = length(Nodes);
  const char**  nodes = malloc(n_nodes*sizeof(char*));
  // make a table so we can easily refer to the nodes
  for(int i = 0; i < n_nodes; ++i) {
    nodes[i] = CHAR(STRING_ELT(Nodes, i));
  }
  SEXP output = PROTECT(allocVector(STRSXP, n_nodes));

  for(int i = 0; i < n_nodes; ++i)
    SET_STRING_ELT(output, i, mkChar(nodes[sorted_nodes_ptr[i]]));

  UNPROTECT(2);

  return(output);
}