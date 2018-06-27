#include"headers/causality.h"
#include"headers/cmpct_cg.h"
#include"headers/int_linked_list.h"
#include"headers/edgetypes.h"
#include"headers/int_a_stack.h"

// macros used in topological sort
#define UNMARKED 0
#define MARKED 1
#define TEMPORARY -1

// macros for dag_to_pattern
#define UNKNOWN -1
#define COMPELLED 1
#define REVERSABLE 2

int* ccf_sort(int n_node, int n_edges, const int* restrict edges);

SEXP ccf_sort_wrapper(SEXP Graph) {
  int* edges_ptr = calculate_edges_ptr(Graph);
  // grab the R structure that holds the Nodes (Char* vector) of the Graph
  SEXP Nodes           = PROTECT(VECTOR_ELT(Graph, NODES));
  int n_nodes          = length(Nodes);
  // generate a hash so we can quickly generate the final output
  const char **  nodes = malloc(n_nodes*sizeof(char*));
  // make a table so we can easily refer to the nodes
  for(int i = 0; i < n_nodes; ++i)
    nodes[i] = CHAR(STRING_ELT(Nodes, i));
  // grab the number of the Edges in the Graph from the Edge matrix
  int n_edges = nrows(VECTOR_ELT(Graph, EDGES));
  // get the topological sort of the Graph
  int* sorted_nodes_ptr = ccf_sort(n_nodes, n_edges, edges_ptr);
  SEXP output;
  /*
   * ccf_sort returns NULL if graph doesn't have a sort. In that case, we return
   * R_NilValue (aka R's version of NULL)
   */
  if(sorted_nodes_ptr == NULL)
    output = PROTECT(R_NilValue); /* Is this ok? */
  else {
    output = PROTECT(allocVector(STRSXP, n_nodes));
    // transfer C level sort to R level
    for(int i = 0; i < n_nodes; ++i)
      SET_STRING_ELT(output, i, mkChar(nodes[sorted_nodes_ptr[i]]));

  }
  // free memory
  free(sorted_nodes_ptr);
  free(nodes);
  // free Unprotect R memeory from the garbage collector
  UNPROTECT(2);
  return(output);
}

