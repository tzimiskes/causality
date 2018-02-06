#include"headers/rapi.h"

#include<stdlib.h>
#include<string.h>

#include"headers/int_linked_list.h"
#include"headers/int_a_stack.h"

// macros used in topological sort
#define UNMARKED 0
#define MARKED 1
#define TEMPORARY -1
// macro when inserting elements without a defined value into a linked list
#define EMPTY -1
// macros for dag_to_pattern
#define UNKNOWN -1
#define COMPELLED 1
#define REVERSABLE 2

// Hash table to store (node1, node2, value)
// This implementation uses a naive hash function for the first key
// second key is represented by linked list
// I guess red black trees could be used instead,
// but I'm skeptical of the performance advantages in this case

int_ll_ptr* make_int_ll_hash_table(const int n);

// These two functions implement the topological sort as described in CLRS
// topological sort returns a SEXP because there's an R fuction which
// the user can call to get the topoligical sort

SEXP c_topological_sort(SEXP dag);
void visit(const int i,
           int* const restrict marked,
           int* const restrict n_marked,
           const int_ll_ptr* const restrict children,
           int_a_stack_ptr restrict stack_ptr
          );

// These two functions (along with the above) implement the algorithm that
// converts DAGs to Patterns (aka CPDAGs). The Algorithm is due to Chickering
// c_dag_to_pattern returns the "patterned" edge list to R,
// so its return type is SEXP

int_ll_ptr* order_edges(SEXP dag, SEXP top_sort, const int n_nodes);
SEXP c_dag_to_pattern(SEXP dag);

// The following two functions implement the topological sort
// algorithm as found in CLRS
void visit(const int i,
           int* const restrict marked,
           int* const restrict n_marked,
           const int_ll_ptr* const restrict children,
           const restrict int_a_stack_ptr stack_ptr) {

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
  (*n_marked)++;
  int_a_stack_push(stack_ptr, i);
  }
}

SEXP c_topological_sort(SEXP dag) {

  const int n_nodes = length(VECTOR_ELT(dag, 0));
  // the hash table stores the children of each node
  int_ll_ptr* const restrict children = make_int_ll_hash_table(n_nodes);

  // grab the edge matrix and number of edges
  SEXP edges = PROTECT(VECTOR_ELT(dag, 2));
  const int n_edges = nrows(edges);
  const int* restrict edges_ptr = INTEGER(edges);

// fill in the hash table
  for(int i = 0; i < n_edges; ++i) {
    // matrices are stored as 1d arrays in R, with column major ordering
    int parent = edges_ptr[i];
    int child = edges_ptr[i + n_edges];
    if(children[parent] == NULL)
      children[parent] = int_ll_instantiate(child, EMPTY);
    else
      int_ll_insert(children[parent], child, EMPTY);
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
      // need to pass n_marked by reference since it is not a pointer
      visit(index, marked, &n_marked, children, stack_ptr);
    else
      index++;
  }

  // copy the contents of the stack pointer to order_ptr
  int* restrict stack_contents_ptr =  int_a_stack_get_stack(stack_ptr);
  memcpy(order_ptr, stack_contents_ptr, n_nodes*sizeof(int));

  // free all the malloc'd memory
  int_a_stack_free(stack_ptr);
  free(stack_ptr);
  for(int i = 0; i < n_nodes; ++i)
    int_ll_free(children[i]);
  free(children);
  free(marked);
  //unprotect order
  UNPROTECT(1);
  return(order);
}

int_ll_ptr* order_edges(SEXP dag, SEXP top_order, const int n_nodes) {

  // get the topological order pointer
  const int* const top_order_ptr = INTEGER(top_order);

  //  top_order_hash will faciliate faster searching through the topological
  //  order. the key of top order is the index, and the value is the value...
  //  however, in this function, we are frequentlly given the value and
  //  asked for the key
  int* const top_order_hash = malloc(n_nodes*sizeof(int));
  if(top_order_hash == NULL)
    error("Failed to allocate memory for top_order_hash.");
  for(int i = 0; i < n_nodes; ++i)
    top_order_hash[top_order_ptr[i]] = i;

  // grab the edge matrix and number of edges
  SEXP edges                 = PROTECT(VECTOR_ELT(dag, 2));
  const int n_edges          = nrows(edges);
  const int* const edges_ptr = INTEGER(edges);

  // the hash table stores the parents of each node
  // set of parents is represented as a linked list
  int_ll_ptr* const parents = make_int_ll_hash_table(n_nodes);

  // fill in the hash table. entries are added in descending topological order
  for(int i = 0; i < n_edges; ++i) {
    int parent = edges_ptr[i          ];
    int child  = edges_ptr[i + n_edges];
    if(parents[child] == NULL) {
      parents[child] = int_ll_instantiate(parent, top_order_hash[parent]);
    }
    else {
      int_ll_insert_by_value(parents[child], parent, top_order_hash[parent]);
    }
  }
  // free all the malloc'd memory
  free(top_order_hash);
  UNPROTECT(1);
  return(parents);
}

SEXP c_dag_to_pattern(SEXP dag) {
  // get the number of nodes
  const int n_nodes = length(VECTOR_ELT(dag, 0));

  // get the topological order
  SEXP top_order                 = PROTECT(c_topological_sort(dag));
  const int* const top_order_ptr = INTEGER(top_order);

  // get the parent list of each node from the function order edges
  int_ll_ptr* parents = order_edges(dag, top_order, n_nodes);

  // order edges sets the value parameter for each edge, so we need to
  // change the value for everything to UNKNOWN
  for(int i = 0; i < n_nodes; ++i) {
    int_ll_ptr tmp_ptr = parents[top_order_ptr[i]];
    while(tmp_ptr != NULL) {
      int_ll_set_value(tmp_ptr, UNKNOWN);
      tmp_ptr = int_ll_next(tmp_ptr);
    }
  }

  for(int i = 0; i < n_nodes; ++i) {
    // by lemma 5 in Chickering, all the incident edges on child are unknown
    // so we don't need to check to see its unordered
    // look at the edges that go into the node 'child'

    const int child             = top_order_ptr[i];
    int_ll_ptr node_parents_ptr = parents[child];

    // if there are incident edges into child, run steps 5-8 of the algorithm.
    // if chil has no icident edges, go to the next node in the
    // topological order
    if(node_parents_ptr != NULL) {

      const int parent = int_ll_key(node_parents_ptr);
      int_ll_ptr grandparent_ptr = parents[parent];

      // for each grandparent where grandparent -> parent is compelled
      // check to see if w forms the chain grandparent -> parent -> child
      // or shielded collider grandparent -> parent, parent -> child,
      // and grandparent -> child
      while(grandparent_ptr != NULL) {

        if (int_ll_value(grandparent_ptr) == COMPELLED) {
          int_ll_ptr coparents_ptr = node_parents_ptr;
          // unsure if this is the correct name
          int chain = 1;
          while(coparents_ptr != NULL) {
            if(int_ll_key(coparents_ptr) == int_ll_key(grandparent_ptr)) {
              // the triple forms a shielded collider so execute step 7
              int_ll_set_value(coparents_ptr, COMPELLED);
              chain = 0;
              break;
            }
            else
              coparents_ptr = int_ll_next(coparents_ptr);
          }

          // the triple forms a chain so execute step 6
          if(chain) {
            coparents_ptr = node_parents_ptr;
            while(coparents_ptr != NULL) {
              int_ll_set_value(coparents_ptr, COMPELLED);
              coparents_ptr = int_ll_next(coparents_ptr);
            }
            // jump to the end of the for loop
            goto JMP_TO_EOFL;
          }
        }
        // if step 7 is executed, goto the next grandparent
        grandparent_ptr = int_ll_next(grandparent_ptr);

      }

      // now, we need to search for z, where z -> y, x != z,
      // and z is not a parent of x. That is, an unshielded collider
      int unshielded_collider = 0;

      // by starting at the second parent (might not exist),
      // we avoid the need to check to see if z = x
      // STEP 7.5: look for an unshielded collider
      int_ll_ptr coparent_ptr = int_ll_next(node_parents_ptr);

      while(coparent_ptr != NULL && !unshielded_collider) {
        // reset grandparents pointer
        grandparent_ptr = parents[parent];
        if(grandparent_ptr == NULL) {
          unshielded_collider = 1;
          break;
        }
        while(grandparent_ptr != NULL) {
          if(int_ll_key(coparent_ptr) == int_ll_key(grandparent_ptr)) {
            unshielded_collider = 1;
            break;
          }
          else
            grandparent_ptr = int_ll_next(grandparent_ptr);
        }
        coparent_ptr = int_ll_next(coparent_ptr);
      }
      // STEP 8, if there is one, label all incident edges compelled
      if(unshielded_collider) {
        while(node_parents_ptr != NULL) {
          int_ll_set_value(node_parents_ptr, COMPELLED);
          node_parents_ptr = int_ll_next(node_parents_ptr);
        }
      }
      // STEP 9, label all unknown edges reversable
      else {
        while(node_parents_ptr != NULL) {
          if(int_ll_value(node_parents_ptr) == UNKNOWN)
            int_ll_set_value(node_parents_ptr, REVERSABLE);
          node_parents_ptr = int_ll_next(node_parents_ptr);
        }
      }
    }
    // JUMP TO END OF FOR LOOP
    JMP_TO_EOFL:{};
  }
  // we will be writing the pattern's edges to a copy of the dag's edgelist
  SEXP edges           = PROTECT(duplicate(VECTOR_ELT(dag, 2)));
  const int n_edges    = nrows(edges);
  int* const edges_ptr = INTEGER(edges);

  // transfer the contents of parents to matrix form compatible
  // with the structure of cgraph objects in R
  int index = 0;
  for(int i = 0; i < n_nodes; ++i) {
    int_ll_ptr node_parents_ptr = parents[i];
    while(node_parents_ptr != NULL) {
      edges_ptr[index            ] = int_ll_key(node_parents_ptr);
      edges_ptr[index + n_edges  ] = i;
      edges_ptr[index + 2*n_edges] = int_ll_value(node_parents_ptr);
      index++;
      node_parents_ptr = int_ll_next(node_parents_ptr);
    }
  }

  // free malloc'd memory
  for(int i = 0; i < n_nodes; ++i)
    int_ll_free(parents[i]);
  free(parents);
  // unprotect top_order and edges
  UNPROTECT(2);
  return(edges);
}
// helper function, nothing interesting going on
int_ll_ptr* make_int_ll_hash_table(const int n) {
  int_ll_ptr* hash_table = malloc(n*sizeof(int_ll_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.");
  for(int i = 0; i < n; ++i)
    hash_table[i] = NULL;
  return(hash_table);
}