#include<R.h>
#include<Rinternals.h>
#include<stdlib.h>

//used when setting up R_xlen_t* marked marked up
#define PERMANENT 1
#define UNMARKED 0
#define TEMPORARY -1
// used when setting SEXP ordered up and the hash_table
#define EMPTY -1

// this defines a pointer to Linked_list_node
typedef struct ll_node* LL_ptr;
// definition of each node in a linked list
typedef struct ll_node {
  R_xlen_t name;
  LL_ptr next;
} Linked_List_Node;

// when we want to add a child to its parent, we pass in the pointer to the 0th parent element (eg hasb_table[0]) and do a traversal till we get to the end
void insert(LL_ptr root, R_xlen_t name) {
  // if its the end of the list or the first element( which is initialized to -1), insert here
  if(root->next == NULL) {
    LL_ptr tmp = (LL_ptr) malloc(sizeof(LL_ptr));
    if(tmp != NULL) {
    tmp->name = name;
    tmp->next = NULL;
    root->next = tmp;
    } else {
      error("Cannot allocate pointer!");
    }
    // if it isn't the end, go to the next
  } else
      insert(root->next, name);
}

// This following topological sort algorithm is found in CLRS

void visit(R_xlen_t i, R_xlen_t* marked, R_xlen_t* n_marked, LL_ptr* children, SEXP order) {
  if(marked[i] == TEMPORARY)
    error("dag contains a cycle, so the input is not actually a dag.");
  else if(marked[i] == UNMARKED) {
    marked[i] = TEMPORARY;
    LL_ptr parent = children[i];
    while(parent != NULL) {
      visit(parent->name, marked, n_marked, children, order);
      parent = parent->next;
    }
    marked[i] = PERMANENT;
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
  // This implementation usesa naive hash function for the first parents
  // set of children is represented as a linked list
  LL_ptr* hash_table = NULL;
  hash_table = (LL_ptr*) malloc(n*sizeof(LL_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.\n");
  for(R_xlen_t i = 0; i < n; ++i) {
    hash_table[i] = (LL_ptr) malloc(sizeof(LL_ptr));
    hash_table[i]->name = EMPTY;
    hash_table[i]->next = NULL;
  }
  // grab the edge matrix and number of edges
  SEXP edges = PROTECT(VECTOR_ELT(dag, 2));
  R_xlen_t n_edges = nrows(edges);
  int* edges_ptr = INTEGER(edges);
  // fill in the hash table
  for(R_xlen_t i = 0; i < n_edges; ++i) {
    R_xlen_t parent = edges_ptr[i];
    R_xlen_t child = edges_ptr[i + n_edges];
    if(hash_table[parent]->name == EMPTY)
      hash_table[parent]->name = child;
    else
      insert(hash_table[parent], child);
  }
  //we no longer need edges
  UNPROTECT(1);
  edges_ptr = NULL;
  for(R_xlen_t i = 0; i< n; ++i){
    LL_ptr tmp = hash_table[i];
    while(tmp != NULL) {
      tmp = tmp->next;
    }
  }
  //create an arrow signify whether or not a node has been marked, in acoordance with the algorithm in CLRS
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
  for(R_xlen_t i = 0; i < n; ++i){
    LL_ptr root = hash_table[i];
    while(root != NULL) {
      LL_ptr next = root->next;
      free(root);
      root = next;
    }
  }
  free(hash_table);
  free(marked);
  //unprotect order
  UNPROTECT(1);
  return(order);
}
