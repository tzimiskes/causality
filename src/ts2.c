#include"headers/causality.h"
#include"headers/cmpct_cg.h"
#include"headers/int_linked_list.h"
#include"headers/edgetypes.h"
#include"headers/int_a_stack.h"

// macros used in topological sort
#define UNMARKED 0
#define MARKED 1
#define TEMPORARY -1
#define ERROR -1
// macros for dag_to_pattern

int* ccf_sort(int n_nodes, int n_edges, const int* restrict edges);



/*
 * ccf_sort_wrapper takes in an R object, proccesses it down to the C level
 * and then runs C level sort on this lower level representation. In then takes
 * the output of ccf_sort and turns it back into an R object.
 */
SEXP ccf_sort_wrapper(SEXP Graph) {
  int * edges_ptr = calculate_edges_ptr(Graph);
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
  /* ccf_sort returns NULL if graph doesn't have a sort. In that case,
   * we return R_NilValue (aka R's version of NULL) */
  SEXP output;
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

// The following two functions implement the topological sort
// algorithm as found in CLRS
/*

int visit(int i, int* restrict marked, int * restrict n_marked, )

void visit(int i,
           int* restrict marked,
           int* restrict n_marked,
           ill_ptr* children,
           restrict int_a_stack_ptr stack_ptr) {

  if(marked[i] == TEMPORARY)
    error("dag contains a cycle, so the input is not actually a dag.");
  else if(marked[i] == UNMARKED) {
    marked[i] = TEMPORARY;
    ill_ptr parent = children[i];
    while(parent != NULL) {
      if(ill_value(parent) == DIRECTED)
        visit(ill_key(parent), marked, n_marked, children, stack_ptr);
      parent = ill_next(parent);
    }
    marked[i] = MARKED;
    (*n_marked)++;
    int_a_stack_push(stack_ptr, i);
  }
}
*/

int* ccf_sort(int n_nodes, int n_edges, const int* restrict edges_ptr) {

  cmpct_cg_ptr cg = create_cmpct_cg(n_nodes, n_edges);
  fill_in_cmpct_cg(cg, edges_ptr, by_children);
  // we no longer need edges

  // create an array to signify whether or not a node has been marked,
  // in accordance with the algorithm in CLRS.
  // 0 means UNMARKED, so calloc is called
  int* marked  = calloc(n_nodes, sizeof(int));
  int* sort    = malloc(n_nodes*sizeof(int));
  // this is also pretty much from CLRS
  int n_marked = 0;
  int node     = 0;
  while(n_marked < n_nodes) {
    if(!marked[node])
      n_marked = visit(node, marked, children, sort);
    else
      node++;
  }

  // copy the contents of the stack pointer to order_ptr
  free_cmpct_cg(cg);
  free(marked);
  //unprotect order
  UNPROTECT(1);
  return(sort);
}
