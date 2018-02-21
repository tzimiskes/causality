#include"headers/rapi.h"
#include"headers/int_linked_list.h"

#define FORWARD 1 // -->
#define UNDIRECTED 2 // ---


#define TRUE 1
#define FALSE 0


inline int meek123(const int node1, const int node2, int_ll_ptr* parents,
                   int* edges_ptr, const int i, const int n_edges);
inline int meek3(const int grandparent, const int node1, const int node2,
                 int_ll_ptr* parents, int* edges_ptr, const int i,
                 const int n_edges);
inline int meek4(const int node1, const int node2, int_ll_ptr* parents,
                 int* edges_ptr, const int i, const int n_edges) {
  return 1;
}


// helper function, nothing interesting going on
static int_ll_ptr* make_int_ll_hash_table(const int n) {
  int_ll_ptr* hash_table = malloc(n*sizeof(int_ll_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.");
  for(int i = 0; i < n; ++i)
    hash_table[i] = NULL;
  return(hash_table);
}

SEXP meek_rules(SEXP pdag) {
  // create hash table
  const int n_nodes   = length(VECTOR_ELT(pdag, 0));
  int_ll_ptr* parents = make_int_ll_hash_table(n_nodes);
  // get edge matrix
  SEXP edges        = PROTECT(duplicate(VECTOR_ELT(pdag, 2)));
  int* edges_ptr    = INTEGER(edges);
  const int n_edges = nrows(edges);

  // fill in the parents hash table
  for(int i = 0; i < n_edges; ++i) {
    const int node1  = edges_ptr[i];
    const int node2  = edges_ptr[i + n_edges];
    const int edge   = edges_ptr[i + 2*n_edges];
    parents[node2] = int_ll_insert(parents[node2], node1, edge);
  }
  int changes_occur;
  do {
    changes_occur = 0;
    for(int i = 0; i < n_edges; ++i) {

      const int node1 = edges_ptr[i];
      const int node2 = edges_ptr[i + n_edges];
      const int edge  = edges_ptr[i + 2*n_edges];

      if(edge == UNDIRECTED) {
        // apply meek rules 1 and 2 at the same time (its faster)
        changes_occur = meek123(node1, node2, parents, edges_ptr, i, n_edges);
        // if we oriented the edge we can goto the end of the loop now
        // ie skip the reversed case/ meek rule 4
        if(changes_occur)
          goto JMP_EOL;
        // we now need to consider the case which node1 and node2 are reversed
        changes_occur = meek123(node2, node1, parents, edges_ptr, i, n_edges);
        if(changes_occur)
          goto JMP_EOL;

        changes_occur = meek4(node2, node1, parents, edges_ptr, i, n_edges);
        if(changes_occur)
          goto JMP_EOL;
        changes_occur = meek4(node1, node2, parents, edges_ptr, i, n_edges);
        if(changes_occur)
          goto JMP_EOL;
      }
      JMP_EOL:{}
    }
  } while(changes_occur);
  // free malloc'd memory
  for(int i = 0; i < n_nodes; ++i)
    int_ll_free(parents[i]);
  free(parents);

  UNPROTECT(1);
  return(edges);
}
inline int meek123(const int node1, const int node2, int_ll_ptr* parents,
                   int* edges_ptr, const int i, const int n_edges)
{
  // parents of parents
  int_ll_ptr node1_parents_ptr = parents[node1];

  // look for grandparent --> node1
  while(node1_parents_ptr != NULL) {
    if(int_ll_value(node1_parents_ptr) == FORWARD) {
      const int node1_parent = int_ll_key(node1_parents_ptr);

      // check to see if node1_parent and node2 are adjacent

      // first we need to look for the edges node1_parent -- node2
      // or node1_parent --> node2
      int_ll_ptr node2_parents_ptr = parents[node2];
      while(node2_parents_ptr != NULL) {
        if(int_ll_key(node2_parents_ptr) == node1_parent) {
          if(int_ll_value(node2_parents_ptr) == FORWARD)
            return FALSE; /* can't orient */
          else /* we have node1_parent -- node2; try meek rule3 */
            return meek3(node1_parent, node1, node2, parents,
                            edges_ptr, i, n_edges);
        }
        node2_parents_ptr = int_ll_next(node2_parents_ptr);
      }

      // now check for node2 --> node1_parent, or node2 -- node1_parent
      int_ll_ptr node1_grandparents_ptr = parents[node1_parent];
      while(node1_grandparents_ptr != NULL) {
        if(int_ll_key(node1_grandparents_ptr) == node2) {
          // if node2 --> node1_parent; orient node2 --> node1
          if(int_ll_value(node1_grandparents_ptr) == FORWARD) {
            edges_ptr[i            ] = node2;
            edges_ptr[i + n_edges  ] = node1;
            edges_ptr[i + 2*n_edges] = FORWARD;
            //TODO(arix) rework insert ll to make it easy to insert correct node
            // as well as delete node
            return TRUE;
          } else
            return meek3(node1_parent, node1, node2, parents,
                         edges_ptr, i, n_edges);
        }
        node1_grandparents_ptr = int_ll_next(node1_grandparents_ptr);
      }
      // node1_parent and node2 are not adjacent, so we have a a chain
      edges_ptr[i + 2*n_edges] = FORWARD;
      return TRUE;
    }
    //TODO(arix) rework insert ll to make it easy to insert correct node
    // as well as delete node
    node1_parents_ptr = int_ll_next(node1_parents_ptr);
  }
  return FALSE;
}




inline int meek3(const int grandparent, const int node1, const int node2, int_ll_ptr* parents,
                 int* edges_ptr, const int i, const int n_edges) {
  /*
    int_ll_ptr pop_ptr = parents[node1];
    // index through the parents of parents (pop_ptr) and
    // look for the chain node1 <-- erich -- node2
    while(pop_ptr != NULL) {
      // find node1 <-- erich and make sure erich != grandparent
      if(int_ll_value(pop_ptr) == FORWARD &&
         int_ll_key(pop_ptr) != grandparent)
      {
        const int erich = int_ll_key(pop_ptr);
        int_ll_ptr poe_ptr = parents[erich];
        // look for erich -- node2
        while(poe_ptr != NULL) {
          if(int_ll_key(poe_ptr) == node2 &&
             int_ll_value(poe_ptr) == UNDIRECTED)
          {
            int_ll_ptr poe_ptr_cpy = int_ll_next(poe_ptr);
            // found erich -- node2. Now, we just need to make sure
            // erich and grandparent are not adjacent
            while(poe_ptr_cpy != NULL) {
              if(int_ll_key(poe_ptr_cpy) == grandparent)
                return UNORIENTED;
              int_ll_ptr poe_ptr_cpy = int_ll_next(poe_ptr_cpy);
            }
            edges_ptr[i            ] = node2;
            edges_ptr[i + n_edges  ] = node1;
            edges_ptr[i + 2*n_edges] = FORWARD;
            return ORIENTED;
          }
        }
      }
      pop_ptr_cpy = int_ll_next(pop_ptr_cpy);
    }
  }
   */
  return TRUE;
}

