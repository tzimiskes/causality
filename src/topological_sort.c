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


// These two functions implement the topological sort as described in CLRS
// topological sort returns a SEXP because there's an R fuction which
// the user can call to get the topoligical sort

SEXP cf_topological_sort(SEXP dag);

void visit(const int i,
           int* const restrict marked,
           int* const restrict n_marked,
           const ill_ptr* const restrict children,
           int_a_stack_ptr restrict stack_ptr
);


// These two functions (along with the above) implement the algorithm that
// converts DAGs to Patterns (aka CPDAGs). The Algorithm is due to Chickering
// c_dag_to_pattern returns the "patterned" edge list to R,
// so its return type is SEXP

cmpct_cg_ptr order_edges(SEXP dag, SEXP top_sort, const int n_nodes);
SEXP cf_dag_to_pattern(SEXP dag);

// The following two functions implement the topological sort
// algorithm as found in CLRS
void visit2(const int i,
           int* const restrict marked,
           int* const restrict n_marked,
           const ill_ptr* const restrict children,
           const restrict int_a_stack_ptr stack_ptr) {

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

SEXP cf_topological_sort(SEXP dag) {
  const int n_nodes = length(VECTOR_ELT(dag, NODES));
  // the hash table stores the children of each node

  ill_ptr* const restrict children = create_ptr_to_ill_ptr(n_nodes);

  // grab the edge matrix and number of edges
  SEXP edges = PROTECT(VECTOR_ELT(dag, EDGES));
  const int n_edges = nrows(edges);


  const int* restrict edges_ptr = INTEGER(edges);

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
      visit2(index, marked, &n_marked, children, stack_ptr);
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

cmpct_cg_ptr order_edges(SEXP dag, SEXP top_order, const int n_nodes) {

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
  SEXP edges                 = PROTECT(VECTOR_ELT(dag, EDGES));
  const int n_edges          = nrows(edges);
  int* edges_ptr = INTEGER(edges);

  // fill in the hash table. entries are added in descending topological order
  int* edges_ptr_offset = edges_ptr + 2*n_edges;
  for(int i = 0; i < n_edges; ++i)
    edges_ptr_offset[i] = top_order_hash[edges_ptr[i]];

  cmpct_cg_ptr cg = create_cmpct_cg(n_nodes, n_edges);
  fill_in_cmpct_cg(cg, edges_ptr, ill_insert_by_value, BY_PARENTS);


  // free all the malloc'd memory
  free(top_order_hash);
  UNPROTECT(1);
  return(cg);
}

SEXP cf_dag_to_pattern(SEXP dag) {
  // get the number of nodes
  const int n_nodes = length(VECTOR_ELT(dag, NODES));

  // get the topological order
  SEXP top_order                 = PROTECT(cf_topological_sort(dag));
  const int* const top_order_ptr = INTEGER(top_order);

  // get the parent list of each node from the function order edges
  cmpct_cg_ptr cg = order_edges(dag, top_order, n_nodes);

  ill_ptr* parents = get_cmpct_cg_parents(cg);

  // order edges sets the value parameter for each edge, so we need to
  // change the value for everything to UNKNOWN
  for(int i = 0; i < n_nodes; ++i) {
    ill_ptr tmp_ptr = parents[i];
    while(tmp_ptr != NULL) {
      ill_set_value(tmp_ptr, UNKNOWN);
      tmp_ptr = ill_next(tmp_ptr);
    }
  }

  for(int i = 0; i < n_nodes; ++i) {
    // by lemma 5 in Chickering, all the incident edges on y are unknown
    // so we don't need to check to see its unordered
    // look at the edges that go into y

    const int y     = top_order_ptr[i];
    // parents of y
    ill_ptr poy_ptr = parents[y];

    // if there are incident edges into y, run steps 5-8 of the algorithm.
    // if y has no incident edges, go to the next node in the
    // topological order
    if(poy_ptr != NULL) {

      const int x        = ill_key(poy_ptr);
      ill_ptr pox_ptr    =  parents[x];

      // for each parent of x, w, where w -> x is compelled
      // check to see if w forms the chain w -> x -> y
      // or shielded collider w -> x -> y,
      // and w -> x
      while(pox_ptr != NULL) {

        if (ill_value(pox_ptr) == COMPELLED) {
          const int w = ill_key(pox_ptr);
          // parents of y duplicate
          ill_ptr poy_dup_ptr = poy_ptr;

          int chain = 1;
          while(poy_dup_ptr != NULL) {
            if(ill_key(poy_dup_ptr) == w) {
              // the triple forms a shielded collider so execute step 7
              // set w -> y compelled
              ill_set_value(poy_dup_ptr, COMPELLED);
              chain = 0;
              break;
            }
            else
              poy_dup_ptr = ill_next(poy_dup_ptr);
          }

          // if the triple forms a chain so execute step 6
          if(chain) {
            // reset poy_dup
            poy_dup_ptr = poy_ptr;
            while(poy_dup_ptr != NULL) {
              ill_set_value(poy_dup_ptr, COMPELLED);
              poy_dup_ptr = ill_next(poy_dup_ptr);
            }
            // jump to the end of the for loop
            goto JMP_TO_EOFL;
          }
        }
        // if step 7 is executed, goto the next grandparent
        pox_ptr = ill_next(pox_ptr);
      }
      // now, we need to search for z, where z -> y, x != z,
      // and z is not a parent of x. That is, an unshielded collider

      // by starting at the second parent (might not exist),
      // we avoid the need to check to see if z = x
      // STEP 7.5: look for an unshielded collider
      int unshielded_collider = 0;
      ill_ptr poy_dup_ptr = parents[y];
      while(poy_dup_ptr != NULL) {
        if(ill_key(poy_dup_ptr) != x) {
          const int z = ill_key(poy_dup_ptr);
          // reset parents of x
          if(!adjacent_in_cg(cg, x, z)) {
            unshielded_collider = 1;
            goto STEP_89;
          }
        }
        poy_dup_ptr = ill_next(poy_dup_ptr);
      }

      STEP_89: {};
      // STEP 8, if there is one, label all incident edges compelled
      if(unshielded_collider) {
        while(poy_ptr != NULL) {
          ill_set_value(poy_ptr, COMPELLED);
          poy_ptr = ill_next(poy_ptr);
        }
      }
      // STEP 9, label all unknown edges reversable
      else {
        while(poy_ptr != NULL) {
          if(ill_value(poy_ptr) == UNKNOWN)
            ill_set_value(poy_ptr, REVERSABLE);
          poy_ptr = ill_next(poy_ptr);
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
    ill_ptr node_parents_ptr = parents[i];
    while(node_parents_ptr != NULL) {
      edges_ptr[index            ] = ill_key(node_parents_ptr);
      edges_ptr[index + n_edges  ] = i;
      edges_ptr[index + 2*n_edges] = ill_value(node_parents_ptr);
      index++;
      node_parents_ptr = ill_next(node_parents_ptr);
    }
  }


  free_cmpct_cg(cg);
  // unprotect top_order and edges
  UNPROTECT(2);
  return(edges);
}
