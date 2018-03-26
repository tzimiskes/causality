#include"headers/causality.h"
#include"headers/edgetypes.h"
#include"headers/int_linked_list.h"
#include"headers/cmpct_cg.h"

#define ORIENT 2
#define FLIP 1
#define UNORIENTABLE 0

static void fill_in(int_ll_ptr* parents, int_ll_ptr* children,
                    const int n_edges, int* edges_ptr)
{
  for(int i = 0; i < n_edges; ++i) {
    const int node1       = edges_ptr[i            ];
    const int node2       = edges_ptr[i + n_edges  ];
    const int edge_type   = edges_ptr[i + 2*n_edges];
    parents[node2] = int_ll_insert(parents[node2], node1, edge_type);
    children[node1] = int_ll_insert(children[node1], node2, edge_type);
  }
}



static int meek12(const int node1, const int node2, int_ll_ptr* parents,
           int_ll_ptr* children);
static int meek34(const int node1, const int node2, const int node3,
                  const int node3_edge, int_ll_ptr* parents,
                  int_ll_ptr* children);


SEXP meek_rules(SEXP pdag) {
  // create hash table
  const int n_nodes    = length(VECTOR_ELT(pdag, NODES));
  int_ll_ptr* parents  = make_int_ll_hash_table(n_nodes);
  int_ll_ptr* children = make_int_ll_hash_table(n_nodes);


  // get edge matrix
  SEXP edges          = PROTECT(duplicate(VECTOR_ELT(pdag, EDGES)));
  int* edges_ptr      = INTEGER(edges);
  const int n_edges   = nrows(edges);
  // get adjacencies

  cmpct_cg_ptr cg = create_cmpct_cg(n_nodes, n_edges);
  fill_in_cmpct_cg(cg, edges_ptr);
  print_cmpct_cg(cg);
  free_cmpct_cg(cg);
  // fill in the parents hash table
  fill_in(parents, children, n_edges, edges_ptr);

  error("test ovah\n");
  int changes_occur;
  do {
    changes_occur = 0;
    for(int i = 0; i < n_edges; ++i) {
      const int node1       = edges_ptr[i            ];
      const int node2       = edges_ptr[i + n_edges  ];
      const int edge_type   = edges_ptr[i + 2*n_edges];
      int orient;

      if(edge_type == ET_UNDIRECTED) {
        orient = meek12(node1, node2, parents, children);
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
          orient = meek12(node2, node1, parents, children);
          if(orient == ORIENT) {
            edges_ptr[i            ] = node2;
            edges_ptr[i + n_edges  ] = node1;
            edges_ptr[i + 2*n_edges] = ET_FORWARD;
            int_ll_insert(parents[node1], node2, ET_FORWARD);
            int_ll_delete(parents[node2], node1);
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
    R_CheckUserInterrupt();
  } while(changes_occur);
  // free malloc'd memory
  for(int i = 0; i < n_nodes; ++i)
    int_ll_free(parents[i]);
  free(parents);

  UNPROTECT(2);
  return(edges);
}

static int meek12(const int node1, const int node2, int_ll_ptr* parents,
                  int_ll_ptr* children)
{

  int_ll_ptr node1_parents = parents[node1];
  int node3 = -1;
  // look for node1_parent (node3) --> node1
  while(node1_parents != NULL) {
    if(int_ll_value(node1_parents) == ET_FORWARD) {
      node3 = int_ll_key(node1_parents);
      break;
    }
    node1_parents = int_ll_next(node1_parents);
  }

  if(node3 < 0)
    return UNORIENTABLE;

  // we need to check to see if node2 and node3 are adjacent
  // first we want to try to see if node2 --> node3, which is meek rule two
  int_ll_ptr node3_parents = parents[node3];

  while(node3_parents != NULL) {
    if(int_ll_key(node3_parents) == node2) {
      if(int_ll_value(node3_parents) == ET_FORWARD)
        return FLIP; /* ie node2 --> node1 */
      else
        return meek34(node1, node2, node3, ET_UNDIRECTED, parents, children);
    }
    node3_parents = int_ll_next(node3_parents);
  }
  // now that we've checked the parents of node3, we need to check the
  // children to look for node3 --> node2, or node3 -- node2
  int_ll_ptr node3_children = children[node3];
  while(node3_children != NULL) {
    if(int_ll_key(node3_children) == node3) {
      return meek34(node1, node2, node3, int_ll_value(node3_children), parents,
                    children);
    }
    node3_children = int_ll_next(node3_children);
  }
  // node2 and node3 are not adjacent, so we can return orient
  return ORIENT;
}

static int meek34(const int node1, const int node2, const int node3,
                  const int node3_edge, int_ll_ptr* parents,
                  int_ll_ptr* children)
  {
  if(node3_edge == ET_UNDIRECTED) {
    // first, lets try to apply meek rule 3
    // look for node4 --> node1
    int_ll_ptr node1_parents = parents[node1];
    while(node1_parents != NULL) {
      if(int_ll_value(node1_parents) == ET_FORWARD &&
         int_ll_key(node1_parents) != node3)
      {
        const int node4 = int_ll_key(node1_parents);
       // now, we need to check to see if node3 and node 4 are not adjacent



        // we use the adjacency list to check whether or not node3 and node4 are
        // non adjacent
        int non_adjacent = 1;
        int_ll_ptr node4_parents  = parents[node4];
        int_ll_ptr node4_children = children[node4];

        while(node4_parents != NULL) {
          if (int_ll_key(node4_parents) == node3) {
            non_adjacent = 0;
            break;
          }
          node4_parents = int_ll_next(node4_parents);
        }
        // if node3 is not a parent of node4,
        // we need to check to see if node3 is a child
        if(non_adjacent) {
          while(node4_children != NULL) {
            if (int_ll_key(node4_children) == node3) {
              non_adjacent = 0;
              break;
            }
            node4_children = int_ll_next(node4_children);
          }
        }

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
      int non_adjacent = 1;
      // now, we need to make sure node1 and node4 are not adjacent
       /* SEXP node1_adjs = PROTECT(VECTOR_ELT(adjacencies, node1));
      const int n_adjs = length(node1_adjs);
      int* adj_ptr = INTEGER(node1_adjs);
      int non_adjacent = 1;
      for(int i = 0; i < n_adjs; ++i) {
        if(adj_ptr[i] == node4) {
          non_adjacent = 0;
          break;
        }
      }
      */
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