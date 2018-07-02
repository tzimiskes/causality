#include <setjmp.h> /* for error handling */

#include "headers/causality.h"
#include "headers/cmpct_cg.h"
#include "headers/int_linked_list.h"
#include "headers/edgetypes.h"

// macros used in ccf_sort
#define UNMARKED 0
#define MARKED 1
#define TEMPORARY -1

int * ccf_sort(int n_nodes, const ill_ptr * restrict children);
void visit(const int node,
           int * restrict marked,
           int * restrict n_unmarked,
           const ill_ptr* children,
           int * restrict sort);

/* In order to account for the possibility that the input may contain a cycle,
 * we need to use longjmp/setjmp to implement error handling that will
 * unwind the call stack created by visit() in a safe way.
 */
static jmp_buf FAIL_STATE;
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

  // grab the number of the Edges in the Graph from the Edge matrix
  int n_edges = nrows(VECTOR_ELT(Graph, EDGES));
  // get the topological sort of the Graph
  cmpct_cg_ptr cg    = create_cmpct_cg(n_nodes, n_edges);
  fill_in_cmpct_cg(cg, edges_ptr,ill_insert2, BY_CHILDREN);
  ill_ptr * children = get_cmpct_cg_parents(cg);
  int * sorted_nodes_ptr = ccf_sort(n_nodes, children);
  /* ccf_sort returns NULL if graph doesn't have a sort. In that case,
   * we return R_NilValue (aka R's version of NULL) */
  SEXP output;
  if(sorted_nodes_ptr == NULL) {
    output = PROTECT(R_NilValue); /* Is this ok? */
  }
  else {
    // allocate memory for the output
    output = PROTECT(allocVector(STRSXP, n_nodes));
    // convert C level output to R level output
    for(int i = 0; i < n_nodes; ++i)
      SET_STRING_ELT(output, i, STRING_ELT(Nodes, sorted_nodes_ptr[i]));
  }
  // free memory
  free(sorted_nodes_ptr);
  free_cmpct_cg(cg);
  // unprotect R memory from the garbage collector
  UNPROTECT(2);
  return(output);
}

/* ccf_sort
 *
 *
 */
int* ccf_sort(int n_nodes, const ill_ptr * restrict children) {
  /* create an array to signify whether or not a node has been marked,
   * in accordance with the algorithm in CLRS.
   * 0 means UNMARKED, so calloc is called
   */
  int * marked        = calloc(n_nodes, sizeof(int));
  if(marked == NULL)
    error("Failed to allocate memory for marked in ccf_sort!\n");
  /* sort contains the topological order of the graph */
  int * sort          = malloc(n_nodes*sizeof(int));
  if(sort == NULL)
    error("Failed to allocate memory for sort in ccf_sort!\n");
  // this is also pretty much from CLRS
  int n_unmarked      = n_nodes;
  int node            = 0;
  /* setjmp(FAIL_STATE) == 0 the first time this is called.
   * longjmping here from visit will have setjmp(FAIL_STATE) == 1
   */
  if(!setjmp(FAIL_STATE)) {
    /* Pick an unmarked node and do a breadth first search on it. If the node is
     * marked, go to the next node and try again
     */
    while(n_unmarked > 0) {
      if(!marked[node])
        visit(node, marked, &n_unmarked, children, sort);
      else
        node++;
    }
  }
  else {
    /* FAIL_STATE: Graph contains a cycle, so visiting activated a longjmp here,
     * unwinding the call stack. Now, we can deallocate the
     * memory allocated to sort and then set sort to NULL. (NULL implies that a
     * valid topological sort of the input doesn't exist).
     */
    free(sort);
    sort = NULL;
  }
  // free malloc'd memory
  free(marked);
  //unprotect order
  return(sort);
}

/* visit recursively looks through the children of the node, looking for cycles
 * if a cycle is found, a longjmp is performed back to ccf_sort, where cleanup
 * occurs. Otherwise, once it has been determined that there are no cylces, the
 * node is permentantly marked, the node is pushed onto the topological sort,
 * n_unmarked is decremented, and the function terminates.
 */
void visit(const int node,
           int * restrict marked,
           int * restrict n_unmarked,
           const ill_ptr* children,
           int * restrict sort)
  {
  if(marked[node] == TEMPORARY) {
  /* Cycle exists; perform longjmp to terminate ccf_sort */
    longjmp(FAIL_STATE, 1);
  }
  else if(marked[node] == UNMARKED) {
    marked[node] = TEMPORARY;
    ill_ptr child = children[node];
    while(child != NULL) {
      if(ill_value(child) == DIRECTED)
        visit(ill_key(child), marked, n_unmarked, children, sort);
      child = ill_next(child);
    }
    marked[node]      = MARKED;
    /* n_unmarked lets us know how many nodes are unmarked, and we can also
     * use it to fill in the sort quickly by using it for the array index */
    (*n_unmarked)    -= 1;
    sort[*n_unmarked] = node;
  }
}
