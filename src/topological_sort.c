#ifndef _R
#define _R
#include<R.h>
#include<Rinternals.h>
#endif
#include<stdlib.h>

#include"int_linked_list.h"
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

// The following  two functions implement the topological sort algorithm as found in CLRS
void visit(R_xlen_t i, R_xlen_t* marked, R_xlen_t* n_marked, int_ll_ptr* children, SEXP order) {

  if(marked[i] == TEMPORARY)
    error("dag contains a cycle, so the input is not actually a dag.");
  else if(marked[i] == UNMARKED) {
    marked[i] = TEMPORARY;
    int_ll_ptr parent = children[i];
    while(parent != NULL) {
      visit(int_ll_key(parent), marked, n_marked, children, order);
      parent = int_ll_next(parent);
    }
    marked[i] = MARKED;
    ++(*n_marked);
    // put the now permanently marked node at the head of the topological order
    // but, we need to find the head. it is defined as the last EMPTY (ie, -1) entry in order,
    // if you start from 0, or as in here, the first empty entry is you start at the back
    int* order_ptr = INTEGER(order);
    for(int j = length(order) - 1; j >= 0; --j) {
      if(order_ptr[j] == EMPTY) {
        order_ptr[j] = i;
        // once we find the head, we can break out of the loop
        break;
      }
    }
  }
}

SEXP c_topological_sort(SEXP dag) {
  // this grabs the number of nodes (aka length(dag$names))
  R_xlen_t n = length(VECTOR_ELT(dag, 0));
  // the hash table stores the children of each node
  // This implementation uses a naive hash function for the first parents
  // set of children is represented as a linked list
  // I guess red black trees could be used instead,
  // but I'm skeptical of the performance advantages in this case
  int_ll_ptr* hash_table = NULL;
  hash_table = malloc(n*sizeof(int_ll_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.\n");
  for(R_xlen_t i = 0; i < n; ++i) {
    hash_table[i] = NULL;
  }

  // grab the edge matrix and number of edges
  SEXP edges = PROTECT(VECTOR_ELT(dag, 2));
  R_xlen_t n_edges = nrows(edges);
  int* edges_ptr = INTEGER(edges);

  // fill in the hash table
  for(R_xlen_t i = 0; i < n_edges; ++i) {
    R_xlen_t parent = edges_ptr[i];
    R_xlen_t child = edges_ptr[i + n_edges];
    if(hash_table[parent] == NULL) {
      hash_table[parent] = int_ll_instantiate(child, EMPTY);
    }
    else {
      int_ll_insert(hash_table[parent], child, EMPTY);
    }
  }
  //we no longer need edges
  UNPROTECT(1);
  edges_ptr = NULL;

  //create an array to signify whether or not a node has been marked, in acoordance with the algorithm in CLRS
  R_xlen_t* marked = (R_xlen_t*) malloc(n*sizeof(R_xlen_t));
  // instantiate the topological order. It will be returned at the end of the function, so it is declared as a SEXP
  SEXP order = PROTECT(allocVector(INTSXP, n));
  // set the initial values in both arrays
  for(R_xlen_t i = 0; i < n; ++i) {
    marked[i] = UNMARKED;
    INTEGER(order)[i] = EMPTY;
  }
  // this is also pretty much from CLRS
  R_xlen_t n_marked = 0;
  R_xlen_t index = 0;
  while(n_marked < n) {
    if(marked[index] == UNMARKED)
      visit(index, marked, &n_marked, hash_table, order);
    else
      ++index;
  }

  // free all the malloc'd memory
  for(R_xlen_t i = 0; i < n; ++i)
    int_ll_free(hash_table[i]);
  free(hash_table);
  free(marked);
  //unprotect order
  UNPROTECT(1);
  return(order);
}

SEXP order_edges(SEXP dag, int_ll_ptr* parents) {
  R_xlen_t n = length(VECTOR_ELT(dag,0));
  SEXP top_order = PROTECT(c_topological_sort(dag));
  int* top_order_ptr = INTEGER(top_order);

  for(int i = 0; i <n; ++i)
    Rprintf("%i\n", top_order_ptr[i]);
  // this will faciliate faster searching through the topological order.
  // the key of top order is the index, and the value is the value...
  // however, in this function, we are frequentlly given the value and asked for the key
  int* top_order_hash = malloc(n*sizeof(int));
  if(top_order_hash == NULL)
    error("Failed to allocate memory for top_order_hash!\n");
  for(int i = 0; i < n; ++i)
    top_order_hash[top_order_ptr[i]] = i;

  SEXP edges = PROTECT(duplicate(VECTOR_ELT(dag, 2)));
  R_xlen_t n_edges = nrows(edges);
  int* edges_ptr = INTEGER(edges);

  SEXP edge_order = PROTECT(allocVector(INTSXP, n_edges));
  int order = 0;
  for(int i = 0; i < n; ++i) {
    // get the lowest ordered node
    R_xlen_t node = top_order_ptr[i];
    // get the number of parents of the lowest ordered node
    R_xlen_t n_parents = int_ll_size(parents[node]);

    int n_marked = 0;
    // while there are unordered edges
    while(n_marked < n_parents) {
      // find the first unordered edge
      int_ll_ptr parent = parents[node];
      while(int_ll_value(parent) == ORDERED)
        parent = int_ll_next(parent);
      R_xlen_t max = int_ll_key(parent);
      // find the highest ordered node
      while(int_ll_next(parent) != NULL) {
         int_ll_ptr next = int_ll_next(parent);
        if(int_ll_value(next) == UNORDERED && top_order_hash[int_ll_key(next)] > top_order_hash[max])
          max = int_ll_key(parent);
        parent = next;
      }

     ++n_marked;
      int_ll_set_value(parent, ORDERED);
      edges_ptr[order + 0*n_edges] = max;
      edges_ptr[order + 1*n_edges] = node;
      ++order;
    }
  }
  Rprintf("Exiting order_edges\n");
  free(top_order_hash);

  UNPROTECT(3);
  return(edge_order);
}

SEXP c_dag_to_pattern(SEXP dag) {
  // this grabs the number of nodes (aka length(dag$names))
  R_xlen_t n = length(VECTOR_ELT(dag, 0));
  // the hash table stores the children of each node
  // This implementation uses a naive hash function for the first parents
  // set of children is represented as a linked list
  // I guess red black trees could be used instead,
  // but I'm skeptical of the performance advantages in this case
  int_ll_ptr* parents = NULL;
  parents = malloc(n*sizeof(int_ll_ptr));
  if(parents == NULL)
    error("Failed to allocate pointer for parents.\n");
  for(R_xlen_t i = 0; i < n; ++i) {
    parents[i] = NULL;
  }

  // grab the edge matrix and number of edges
  SEXP edges = PROTECT(VECTOR_ELT(dag, 2));
  R_xlen_t n_edges = nrows(edges);
  int* edges_ptr = INTEGER(edges);

  // fill in the hash table
  for(R_xlen_t i = 0; i < n_edges; ++i) {
    R_xlen_t parent = edges_ptr[i];
    R_xlen_t child = edges_ptr[i + n_edges];
    if(parents[child] == NULL) {
      parents[child] = int_ll_instantiate(parent, UNORDERED);
    }
    else {
      int_ll_insert(parents[child], parent, UNORDERED);
    }
  }
  //we no longer need edges
  UNPROTECT(1);
  edges_ptr = NULL;

  SEXP ordered_edges = PROTECT(order_edges(dag, parents));
  int* ordered_edges_ptr = INTEGER(ordered_edges);
  UNPROTECT(1);

  int n_unknown = n_edges;
  while(n_unknown > 0) {
    int index = 0;
    while(lookup(ordered_edges_ptr[index], ordered_edges_ptr[index + n_edges], parents) != UNKNOWN)
      ++index;

  int parent = ordered_edges_ptr[index];
  int child = ordered_edges_ptr[index + n_edges];

  }

  return(ordered_edges);
}

int lookup(int parent, int child, int_ll_ptr* hash_table) {
  int_ll_ptr tmp = hash_table[child];
  while(int_ll_key(tmp) != parent)
    tmp = int_ll_next(tmp);
return(int_ll_value(tmp));
}