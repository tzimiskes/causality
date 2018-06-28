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

int* ccf_sort(int n_nodes, int n_edges, const int* restrict edges);

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

int* ccf_sort(int n_nodes, int n_edges, int* restrict edges_ptr) {

  // the hash table stores the children of each node

  ill_ptr* children = create_ptr_to_ill_ptr(n_nodes);

  // grab the edge matrix and number of edges

  // fill in the hash table
  for(int i = 0; i < n_edges; ++i) {
    // matrices are stored as 1d arrays in R, with column major ordering
    int parent = edges_ptr[i];
    int child = edges_ptr[i + n_edges];
    int edge = edges_ptr[i+ 2*n_edges];
    children[parent] = ill_insert(children[parent], child, edge);
  }
  // we no longer need edges
  UNPROTECT(1);
  edges_ptr = NULL;

  // create a stack to store the topological order
  int_a_stack_ptr stack_ptr = int_a_stack_instantiate(n_nodes);

  // instantiate the topological order. It will be returned at the end of the
  // function, so it is declared as a SEXP.
  // it will be filled in by the results of stack_ptr
  SEXP order = PROTECT(allocVector(INTSXP, n_nodes));
  int* restrict order_ptr = INTEGER(order);

  // create an array to signify whether or not a node has been marked,
  // in accordance with the algorithm in CLRS.
  // 0 means UNMARKED, so calloc is called
  int* const marked = calloc(n_nodes, sizeof(int));

  // this is also pretty much from CLRS
  int n_marked = 0;
  int index = 0;
  while(n_marked < n_nodes) {
    if(marked[index] == UNMARKED)
      // need to pass the adress of n_marked since it is not a pointer
      visit(index, marked, &n_marked, children, stack_ptr);
    else
      index++;
  }

  // copy the contents of the stack pointer to order_ptr
  int* restrict stack_contents_ptr =  int_a_stack_get_stack(stack_ptr);
  memcpy(order_ptr, stack_contents_ptr, n_nodes*sizeof(int));

  // free all the malloc'd memory
  free(stack_ptr);
  for(int i = 0; i < n_nodes; ++i)
    ill_free(children[i]);
  free(children);
  free(marked);
  //unprotect order
  UNPROTECT(1);
  return(order);
}
