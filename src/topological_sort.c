#ifndef _R
#define _R
#include<R.h>
#include<Rinternals.h>
#endif
#include<stdlib.h>

#include"int_linked_list.h"
#include"int_a_stack.h"
// macros used in topological sort
#define UNMARKED 0
#define MARKED 1
#define TEMPORARY -1
// macro when inserting elements without a defined value into a linked list
#define EMPTY -1

// macros for use in order_edges
#define UNORDERED 0
#define ORDERED 1


#define UNKNOWN 1
#define COMPELLED 0
#define REVERSABLE -1


int lookup(int parent, int child, int_ll_ptr* hash_table);
void visit(const int i, int* const marked, int* const n_marked,
           const int_ll_ptr* const children, int_a_stack_ptr stack_ptr);
SEXP c_topological_sort(SEXP dag);
SEXP c_order_edges(SEXP dag);

// The following  two functions implement the topological sort algorithm as found in CLRS
void visit(const int i, int* const marked, int* const n_marked,
           const int_ll_ptr* const children, int_a_stack_ptr stack_ptr) {

  if(marked[i] == TEMPORARY)
    error("dag contains a cycle, so the input is not actually a dag.");
  else if(marked[i] == UNMARKED) {
    marked[i] = TEMPORARY;
    int_ll_ptr parent = children[i];
    while(parent != NULL) {
      visit(int_ll_key(parent), marked, n_marked, children, stack_ptr);
      parent = int_ll_next(parent);
    }
    marked[i] = MARKED;
    ++(*n_marked);
    int_a_stack_push(stack_ptr, i);
  }
}

SEXP c_topological_sort(SEXP dag) {
  // this grabs the number of nodes (aka length(dag$names))
  int n = length(VECTOR_ELT(dag, 0));
  // the hash table stores the children of each node
  // This implementation uses a naive hash function for the first parents
  // set of children is represented as a linked list
  // I guess red black trees could be used instead,
  // but I'm skeptical of the performance advantages in this case
  int_ll_ptr* const hash_table = malloc(n*sizeof(int_ll_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.\n");
  for(int i = 0; i < n; ++i) {
    hash_table[i] = NULL;
  }

  // grab the edge matrix and number of edges
  SEXP edges = PROTECT(VECTOR_ELT(dag, 2));
  const int n_edges = nrows(edges);
  int* edges_ptr = INTEGER(edges);

  // fill in the hash table
  for(int i = 0; i < n_edges; ++i) {
    // matrices are stored as 1d arrays in R, with column major ordering
    int parent = edges_ptr[i];
    int child = edges_ptr[i + n_edges];
    if(hash_table[parent] == NULL)
      hash_table[parent] = int_ll_instantiate(child, EMPTY);
    else
      int_ll_insert(hash_table[parent], child, EMPTY);
  }
  // we no longer need edges
  UNPROTECT(1);
  edges_ptr = NULL;

  // create an array to signify whether or not a node has been marked,
  // in accordance with the algorithm in CLRS
  // create a stack to store the topological order
  int_a_stack_ptr stack_ptr = int_a_stack_instantiate(n);

  // instantiate the topological order. It will be returned at the end of the
  // function, so it is declared as a SEXP.
  // it will be filled in by the results of stack_ptr
  SEXP order = PROTECT(allocVector(INTSXP, n));
  int* order_ptr = INTEGER(order);

  // set the initial values in unmarked
  int* const marked = malloc(n*sizeof(int));
  for(int i = 0; i < n; ++i)
    marked[i] = UNMARKED;
  // this is also pretty much from CLRS
  int n_marked = 0;
  int index = 0;
  while(n_marked < n) {
    if(marked[index] == UNMARKED)
      // need to pass n_marked by reference since it is not a pointer
      visit(index, marked, &n_marked, hash_table, stack_ptr);
    else
      index++;
  }
  // transfer the contents of the stack into the SEXP order
  int* stack = int_a_stack_get_stack(stack_ptr);
  for(int i = 0; i <n; ++i)
    order_ptr[i] = stack[i];
  // free all the malloc'd memory
  int_a_stack_free(stack_ptr);
  free(stack_ptr);
  for(int i = 0; i < n; ++i)
    int_ll_free(hash_table[i]);
  free(hash_table);
  free(marked);
  //unprotect order
  UNPROTECT(1);
  return(order);
}

SEXP c_order_edges(SEXP dag) {
  const int n = length(VECTOR_ELT(dag, 0));

  // get the topological order
  SEXP top_order = PROTECT(c_topological_sort(dag));
  int* top_order_ptr = INTEGER(top_order);

  //  top_order_hash will faciliate faster searching through the topological
  //  order. the key of top order is the index, and the value is the value...
  //  however, in this function, we are frequentlly given the value and
  //  asked for the key
  int* const top_order_hash = malloc(n*sizeof(int));
  if(top_order_hash == NULL)
    error("Failed to allocate memory for top_order_hash.");
  for(int i = 0; i < n; ++i)
    top_order_hash[top_order_ptr[i]] = i;
  // grab the edge matrix and number of edges
  SEXP ordered_edges = PROTECT(duplicate(VECTOR_ELT(dag, 2)));
  const int n_edges = nrows(ordered_edges);
  int* ordered_edges_ptr = INTEGER(ordered_edges);

  // the hash table stores the parents of each node
  // This implementation uses a naive hash function for the first parent
  // set of parents is represented as a linked list
  // I guess red black trees could be used instead,
  // but I'm skeptical of the performance advantages in this case
  int_ll_ptr* const parents = malloc(n*sizeof(int_ll_ptr));
  if(parents == NULL)
    error("Failed to allocate pointer for parents.\n");
  for(int i = 0; i < n; ++i)
    parents[i] = NULL;

  // fill in the hash table. entries are in descending topological order
  for(int i = 0; i < n_edges; ++i) {
    int parent = ordered_edges_ptr[i];
    int child = ordered_edges_ptr[i + n_edges];
    if(parents[child] == NULL) {
      parents[child] = int_ll_instantiate(parent, top_order_hash[parent]);
    }
    else {
      int_ll_insert_by_value(parents[child], parent, top_order_hash[parent]);
    }
  }
  // now reorder the edge matrix according to the order
  int order = 0;
  for (int i = 0; i < n; ++i) {
    int node = top_order_ptr[i];
    int_ll_ptr parent = parents[node];
    while(parent != NULL) {
      ordered_edges_ptr[order]           = int_ll_key(parent);
      ordered_edges_ptr[order + n_edges] = node;
      order++;
      parent = int_ll_next(parent);
    }
  }

  free(top_order_hash);
  for(int i = 0; i < n; ++i)
    int_ll_free(parents[i]);
  free(parents);
  UNPROTECT(2);
  return(ordered_edges);
}

SEXP c_dag_to_pattern(SEXP dag) {
 return(dag);
}