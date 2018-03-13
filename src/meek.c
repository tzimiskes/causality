#include"headers/causality.h"
#include"headers/edgetypes.h"
#include"headers/int_linked_list.h"


#define ORIENT 2
#define FLIP 1
#define UNORIENTABLE 0


static int meek12(const int node1, const int node2, int_ll_ptr* parents,
           SEXP adjacencies);
static int meek34(const int node1, const int node2, const int node3,
                  const int node3_edge, int_ll_ptr* parents,
                 SEXP adjacencies);


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
  SEXP edges          = PROTECT(duplicate(VECTOR_ELT(pdag, 2)));
  int* edges_ptr      = INTEGER(edges);
  const int n_edges   = nrows(edges);
  // get adjacencies
  SEXP adjacencies    = PROTECT(VECTOR_ELT(pdag, 1));

  // fill in the parents hash table
  for(int i = 0; i < n_edges; ++i) {
    const int node1  = edges_ptr[i            ];
    const int node2  = edges_ptr[i + n_edges  ];
    const int edge_type   = edges_ptr[i + 2*n_edges];
    parents[node2] = int_ll_insert(parents[node2], node1, edge_type);
  }

  int changes_occur;
  do {
    changes_occur = 0;
    for(int i = 0; i < n_edges; ++i) {
      const int node1  = edges_ptr[i            ];
      const int node2  = edges_ptr[i + n_edges  ];
      const int edge_type   = edges_ptr[i + 2*n_edges];
      int orient;
      if(edge_type == ET_UNDIRECTED) {

        orient = meek12(node1, node2, parents, adjacencies);
        if(orient == ORIENT) {
          edges_ptr[i + 2*n_edges] = ET_FORWARD;
          int_ll_set_value(parents[node2], ET_FORWARD);
          changes_occur = 1;
        }
        if(orient == FLIP) {
          edges_ptr[i            ] = node2;
          edges_ptr[i + n_edges  ] = node1;
          edges_ptr[i + 2*n_edges] = ET_FORWARD;
          int_ll_delete(parents[node2], node1);
          int_ll_insert(parents[node1], node2, ET_FORWARD);
          changes_occur = 1;
        }
        if(orient == UNORIENTABLE) {
          orient = meek12(node2, node1, parents, adjacencies);
          if(orient == ORIENT) {
            edges_ptr[i            ] = node2;
            edges_ptr[i + n_edges  ] = node1;
            edges_ptr[i + 2*n_edges] = ET_FORWARD;
            int_ll_delete(parents[node2], node1);
            int_ll_insert(parents[node1], node2, ET_FORWARD);
            changes_occur = 1;
          }
          if(orient == FLIP) {
            edges_ptr[i + 2*n_edges] = ET_FORWARD;
            int_ll_set_value(parents[node2], ET_FORWARD);
            changes_occur = 1;
          }
        }
      }
    }
  } while(changes_occur);
  // free malloc'd memory
  for(int i = 0; i < n_nodes; ++i)
    int_ll_free(parents[i]);
  free(parents);

  UNPROTECT(2);
  return(edges);
}

static int meek12(const int node1, const int node2, int_ll_ptr* parents,
                  SEXP adjacencies)
{
  int_ll_ptr node1_parents = parents[node1];
  // look for node1_parent (node3) --> node1
  while(node1_parents != NULL) {
    if(int_ll_value(node1_parents) == ET_FORWARD) {
      // if node2 has only one adjacent, we can orient
      if(length(VECTOR_ELT(adjacencies, node2)) == 1)
        return ORIENT;
      break;
    }
    node1_parents = int_ll_next(node1_parents);
  }
  if(node1_parents == NULL)
    return UNORIENTABLE;

  const int node3 = int_ll_key(node1_parents);
  // we need to check to see if node2 and node3 are adjacent
  // first we want to try to see if node2 --> node3, which is meek rule two
  int_ll_ptr node3_parents = parents[node3];
  while(node3_parents != NULL) {
    if(int_ll_key(node3_parents) == node2) {
      if(int_ll_value(node3_parents) == ET_FORWARD)
        return FLIP; /* ie node2 --> node1 */
      else {
        Rprintf("execute 34\n");
        return meek34(node1, node2, node3, ET_UNDIRECTED, parents,
                      adjacencies);
      }
    }
    node3_parents = int_ll_next(node3_parents);
  }
  // now that we've checked the parents of node3, we need to check the
  // parents of node2 to look for node3 --> node2, or node3 -- node2
  int_ll_ptr node2_parents = parents[node2];
  while(node2_parents != NULL) {
    if(int_ll_key(node2_parents) == node3) {
      return meek34(node1, node2, node3, int_ll_value(node2_parents), parents,
                    adjacencies);
    }
    node2_parents = int_ll_next(node2_parents);
  }
  // node2 and node3 are not adjacent, so we can return orient
  return ORIENT;
}

static int meek34(const int node1, const int node2, const int node3,
                  const int node3_edge, int_ll_ptr* parents,
                  SEXP adjacencies)
  {
  if(node3_edge == ET_UNDIRECTED && length(VECTOR_ELT(adjacencies, node1)) > 2) {
    // first, lets try to apply meek rule 3
    // look for node4 --> node1
    int_ll_ptr node1_parents = parents[node1];
    while(node1_parents != NULL) {
      if(int_ll_value(node1_parents) == ET_FORWARD &&
         int_ll_key(node1_parents) != node3)
      {
        const int node4 = int_ll_key(node1_parents);
       // now, we need to check to see if node3 and node 4 are not adjacent
        SEXP node3_adjs = PROTECT(VECTOR_ELT(adjacencies, node3));
        const int n_adjs = length(node3_adjs);
        int* adj_ptr = INTEGER(node3_adjs);
        // we use the adjacency list to check whether or not node3 and node4 are
        // non adjacent
        int non_adjacent = 1;
        for(int i = 0; i < n_adjs; ++i) {
          if(adj_ptr[i] == node4) {
            non_adjacent = 0;
            break;
          }
        }
        UNPROTECT(1);
        if(non_adjacent) {
          // now we need to check to see if node4 -- node2
          int_ll_ptr node2_parents = parents[node2];
          while(node2_parents != NULL) {
            if(int_ll_value(node2_parents) == ET_UNDIRECTED &&
             int_ll_key(node2_parents) == node4)
            {
              return FLIP;
            }
            node2_parents = int_ll_next(node2_parents);
          }
          // we didn't find it in the parents of node2, so lets look in
          // the parents of node4
          int_ll_ptr node4_parents = parents[node4];
          while(node2_parents != NULL) {
            if(int_ll_value(node4_parents) == ET_UNDIRECTED &&
               int_ll_key(node4_parents) == node2)
            {
              return FLIP;
            }
            node4_parents = int_ll_next(node4_parents);
          }
        }
      }
      node1_parents = int_ll_next(node1_parents);
    }
  }
  // meek rule 4
  // we need to find node4 s.t. node2 -- node4 --> node3
  int_ll_ptr node3_parent = parents[node3];
  while(node3_parent != NULL) {
    if(int_ll_value(node3_parent) == ET_FORWARD) {
      const int node4 = int_ll_value(node3_parent);

      // now, we need to make sure node1 and node4 are not adjacent
      SEXP node1_adjs = PROTECT(VECTOR_ELT(adjacencies, node1));
      const int n_adjs = length(node1_adjs);
      int* adj_ptr = INTEGER(node1_adjs);
      int non_adjacent = 1;
      for(int i = 0; i < n_adjs; ++i) {
        if(adj_ptr[i] == node4) {
          non_adjacent = 0;
          break;
        }
      }
      UNPROTECT(1);
      if(non_adjacent) {
        // now we need to check to see if node4 -- node2
        int_ll_ptr node2_parents = parents[node2];
        while(node2_parents != NULL) {
          if(int_ll_value(node2_parents) == ET_UNDIRECTED &&
             int_ll_key(node2_parents) == node4)
          {
            return FLIP;
          }
          node2_parents = int_ll_next(node2_parents);
        }
        // we didn't find it in the parents of node2, so lets look in
        // the parents of node4
        int_ll_ptr node4_parents = parents[node4];
        while(node2_parents != NULL) {
          if(int_ll_value(node4_parents) == ET_UNDIRECTED &&
             int_ll_key(node4_parents) == node2)
          {
            return FLIP;
          }
          node4_parents = int_ll_next(node4_parents);
        }
      }
    }
    node3_parent = int_ll_next(node3_parent);
  }
  return UNORIENTABLE;
}