/* Author: Alexander Rix
 * sort.c Impements a function to perform a topological sort on a a graph.
 * There are two main functions ccf_sort_wrapper, which is acts as an interface
 * between R and ccf_sort, which is the Causality C Function that actually
 * performs the sorting. ccf_sort_wrapper returns R;s version of NULL is there
 * is an error, or a R charecter vector which contain the nodes of the graph
 * sorted by their topological ordering. ccf_sort returns a NULL pointer, or a
 * pointer to an interger which contains the C version of the node ordering
 */

#include <setjmp.h> /* for error handling */

#include <causality.h>
#include <cgraph.h>
#include <int_linked_list.h>
#include <edgetypes.h>

/* macros used in ccf_sort */
#define UNMARKED 0
#define MARKED 1
#define TEMPORARY -1

static void visit(const int node,
                  int * restrict marked,
                  int * restrict n_unmarked,
                  const ill_ptr * children,
                  int * restrict sort);

/* In order to account for the possibility that the input may contain a cycle,
 * we need to use longjmp/setjmp to implement error handling that will
 * unwind the call stack created by visit() in a safe way. */
static jmp_buf FAIL_STATE;

/* ccf_sort_wrapper takes in an R object, proccesses it down to the C level
 * and then runs C level sort on this lower level representation. In then takes
 * the output of ccf_sort and turns it back into an R object. */
SEXP ccf_sort_wrapper(SEXP Graph) {
  int * edges       = calculate_edges_ptr(Graph);
  int n_nodes       = length(VECTOR_ELT(Graph,NODES));
  /* grab the number of the Edges in the Graph */
  int n_edges       = nrows(VECTOR_ELT(Graph, EDGES));
  cgraph_ptr cg_ptr = create_cgraph(n_nodes);
  fill_in_cgraph(cg_ptr, n_edges, edges);
  free(edges);
  /* ccf_sort returns NULL if graph doesn't have a sort. In that case,
   * we return R_NilValue (aka R's version of NULL) */
  int * sorted_nodes = ccf_sort(cg_ptr);
  free_cgraph(cg_ptr);
  if(sorted_nodes == NULL) {
    return R_NilValue;
  }
  /* grab the R structure that holds the Nodes (Char* vector) of the Graph */
  SEXP Nodes  = PROTECT(VECTOR_ELT(Graph, NODES));
  /* allocate memory for the output */
  SEXP Output = PROTECT(allocVector(STRSXP, n_nodes));
  /* convert C level output to R level output */
  for(int i = 0; i < n_nodes; ++i)
    SET_STRING_ELT(Output, i, STRING_ELT(Nodes, sorted_nodes[i]));
  free(sorted_nodes);
  UNPROTECT(2);
  return(Output);
}

/* ccf_sort implements a topological sort by using a breadth first search as
 * described in CLRS. This function takes in a C level representation
 * (currently an integer linked list) of the edge list of an causality.graph,
 * and returns a pointer to the sorted (C level representation) of the nodes */
int * ccf_sort(cgraph_ptr cg_ptr) {
  int n_nodes        = cg_ptr->n_nodes;
  /* create an array to signify whether or not a node has been marked,
   * in accordance with the algorithm in CLRS.
   * 0 means UNMARKED, so calloc is called */
  int * marked        = calloc(n_nodes, sizeof(int));
  if(marked == NULL)
    error("Failed to allocate memory for marked in ccf_sort!\n");
  /* sort contains the topological order of the graph */
  int * sort          = malloc(n_nodes * sizeof(int));
  if(sort == NULL)
    error("Failed to allocate memory for sort in ccf_sort!\n");
  /* this is also pretty much from CLRS */
  /* setjmp(FAIL_STATE) == 0 the first time this is called.
   * longjmping here from visit will have setjmp(FAIL_STATE) == 1 */
  if(!setjmp(FAIL_STATE)) {
    /* Pick an unmarked node and do a breadth first search on it. If
     * the node is marked, go to the next node and try again */
     ill_ptr * children = cg_ptr->children;
     int stack_index    = n_nodes;
     for(int i = 0; i < n_nodes; ++i) {
       if(!marked[i])
         visit(i, marked, &stack_index, children, sort);
     }
  }
  else {
    /* FAIL_STATE: Graph contains a cycle, so visiting activated a longjmp here,
     * unwinding the call stack. Now, we can deallocate the
     * memory allocated to sort and then set sort to NULL. (NULL implies that a
     * valid topological sort of the input doesn't exist). */
    free(sort);
    sort = NULL;
  }
  free(marked);
  return sort;
}

/* visit: recursively look through the children of the node, looking for cycles
 * if a cycle is found, a longjmp is performed back to ccf_sort, where cleanup
 * occurs. Otherwise, once it has been determined that there are no cylces, the
 * node is permentantly marked, the node is pushed onto the topological sort,
 * stack_index is decremented, and the function terminates. */
static void visit(const int node,
                  int * restrict marked,
                  int * restrict stack_index,
                  const ill_ptr* children,
                  int * restrict sort)
  {
  if(marked[node] == TEMPORARY) {
  /* Cycle exists; perform longjmp to ccf_sort so we can terminate the sort */
    longjmp(FAIL_STATE, 1);
  }
  else if(marked[node] == UNMARKED) {
    marked[node]  = TEMPORARY;
    ill_ptr child = children[node];
    while(child != NULL) {
      visit(ill_key(child), marked, stack_index, children, sort);
      child = ill_next(child);
    }
    marked[node]       = MARKED;
    /* n_unmarked lets us know how many nodes are unmarked, and we can also
     * use it to fill in the sort quickly by using it for the array index */
    (*stack_index)    -= 1;
    sort[*stack_index] = node;
  }
}
