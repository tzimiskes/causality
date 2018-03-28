#include"headers/causality.h"
#include"headers/edgetypes.h"
#include"headers/int_linked_list.h"
#include"headers/cmpct_cg.h"

#define ORIENT 2
#define FLIP 1
#define UNORIENTABLE 0


static int meek1(int node1, int node2, cmpct_cg_ptr cg);
static int meek2(int node1, int node2, cmpct_cg_ptr cg);
static int meek3(int node1, int node2, cmpct_cg_ptr cg);
static int meek4(int node1, int node2, cmpct_cg_ptr cg);

static int apply_rule(int* edges_ptr, int i, int node1, int node2,
                  cmpct_cg_ptr cg, int (*meek_rule) (int, int, cmpct_cg_ptr));

SEXP meek_rules(SEXP pdag) {
  const int n_nodes    = length(VECTOR_ELT(pdag, NODES));

  // get edge matrix
  SEXP edges          = PROTECT(duplicate(VECTOR_ELT(pdag, EDGES)));
  int* edges_ptr      = INTEGER(edges);
  const int n_edges   = nrows(edges);


  cmpct_cg_ptr cg_ptr = create_cmpct_cg(n_nodes, n_edges);
  fill_in_cmpct_cg(cg_ptr, edges_ptr, ill_insert2);
  print_cmpct_cg(cg_ptr);
  int rule_applied;
  do {
    rule_applied = 0;
    for(int i = 0; i < n_edges; ++i) {
      int edge_type = edges_ptr[i + 2*n_edges];
      if(edge_type == UNDIRECTED) {
        int node1 = edges_ptr[i            ];
        int node2 = edges_ptr[i + n_edges  ];
        rule_applied = apply_rule(edges_ptr, i, node1, node2, cg_ptr, meek1);
        if(rule_applied){
          Rprintf("Rule 1 Applied\n");
          print_cmpct_cg(cg_ptr);
          break;
        }
        rule_applied = apply_rule(edges_ptr, i, node1, node2, cg_ptr, meek2);
        if(rule_applied) {
          Rprintf("Rule 2 Applied\n");
          print_cmpct_cg(cg_ptr);
          break;
        }
        rule_applied = apply_rule(edges_ptr, i, node1, node2, cg_ptr, meek3);
        if(rule_applied)
          break;
        rule_applied = apply_rule(edges_ptr, i, node1, node2, cg_ptr, meek4);
        if(rule_applied) {
          Rprintf("Rule 4 Applied\n");
          print_cmpct_cg(cg_ptr);
          break;
        }
      }
    }
     R_CheckUserInterrupt();
  } while(rule_applied);

  // free malloc'd memory
  free_cmpct_cg(cg_ptr);
  UNPROTECT(1);
  return(edges);
}

/* meek rule one:
 * look for chain node3 --> node1 --- node2, where !adj(node3, node2). If so,
 * orient node1 --> node2
 *
 * Reverse case: node3 --> node1, ! adj(node3, node1); orient node2 --> node1
 */
static int meek1(const int node1, const int node2, cmpct_cg_ptr cg) {
  ill_ptr* parents      = get_cmpct_cg_parents(cg);

  /* look for a directed parent of node1 */
  ill_ptr node1_parents = parents[node1];
  while(node1_parents != NULL) {
    if(ill_value(node1_parents) == DIRECTED) {
      int node3 = ill_key(node1_parents);
      /* check to see of node2 and node3 are adjacent */
      if(!nodes_adjacent_in_cg(cg, node2, node3))
        return ORIENT;
    }
    node1_parents = ill_next(node1_parents);
  }
  /* if we are here, we now look at the parents of node2 instead of node1 */
  ill_ptr node2_parents = parents[node2];
  while(node2_parents != NULL) {
    if(ill_value(node2_parents) == DIRECTED) {
      int node3 = ill_key(node2_parents);
      /* check to see of node1 and node3 are adjacent */
      if(!nodes_adjacent_in_cg(cg, node1, node3))
        return FLIP;
    }
    node2_parents = ill_next(node2_parents);
  }
  return UNORIENTABLE;
}

/* meek rule 2: look for node3 such that, node3 --> node1, node2 --> node3. If
 * so, orient node2 --> node1 to prevent a cycle.
 *
 * In the reverse case, look for node3 --> node2, node1 --> node3, so that we
 * orient node1 --> node2
 */
static int meek2(const int node1, const int node2, cmpct_cg_ptr cg) {
  ill_ptr* parents      = get_cmpct_cg_parents(cg);
  ill_ptr node1_parents = parents[node1];
  ill_ptr node2_parents = parents[node2];
  while(node1_parents != NULL) {
    if(ill_value(node1_parents) == DIRECTED) {
      int node3 = ill_key(node1_parents);
      ill_ptr node3_parents = parents[node3];
      while(node3_parents != NULL) {
        if(ill_value(node3_parents) == DIRECTED &&
           ill_key(node3_parents) == node2)
          {
            return FLIP;
          }
        node3_parents = ill_next(node3_parents);
      }
    }
    node1_parents = ill_next(node1_parents);
  }
  /* if we are here, we now look at the parents of node2 instead of node1 */
  while(node2_parents != NULL) {
    if(ill_value(node2_parents) == DIRECTED) {
      int node3 = ill_key(node2_parents);
      ill_ptr node3_parents = parents[node3];
      while(node3_parents != NULL) {
        if(ill_value(node3_parents) == DIRECTED) {
          if(ill_key(node3_parents) == node1)
            return ORIENT;
        }
        node3_parents = ill_next(node3_parents);
      }
    }
    node2_parents = ill_next(node2_parents);
  }
  return UNORIENTABLE;
}


static int meek3(const int node1, const int node2, cmpct_cg_ptr cg) {
  ill_ptr* parents          = get_cmpct_cg_parents(cg);
  ill_ptr node1_parents     = parents[node1];

  ill_ptr node2_parents     = parents[node2];
  // look node3 --> node1


  while(node1_parents != NULL ) {
    if(ill_value(node1_parents) == DIRECTED) {
      int node3 = ill_key(node1_parents);
      // look for node4 --> node1
      ill_ptr node1_parents_cpy = parents[node1];
      while(node1_parents_cpy != NULL) {
        if(ill_value(node1_parents_cpy) == DIRECTED &&
           ill_key(node1_parents_cpy) != node3)
        {
          int node4 = ill_key(node1_parents_cpy);
          // check to see if they are adjacent
          if(!nodes_adjacent_in_cg(cg, node3, node4)) {
            // if they are not, look for node2 -- node3, node2 -- node4
            if(undirected_edge_in_cg(cg, node3, node2) &&
               undirected_edge_in_cg(cg, node4, node2))
            {
              return FLIP;
            }
          }
        }
        node1_parents_cpy = ill_next(node1_parents_cpy);
      }
    }
    node1_parents = ill_next(node1_parents);
  }
  // now we look through the parents of node2 instead of node1
  while(node2_parents != NULL ) {
    if(ill_value(node2_parents) == DIRECTED) {
      int node3 = ill_key(node2_parents);
      // look for node4 --> node1
      ill_ptr node2_parents_cpy = parents[node2];
      while(node2_parents_cpy != NULL) {
        if(ill_value(node2_parents_cpy) == DIRECTED &&
           ill_key(node2_parents_cpy) != node3)
        {
          int node4 = ill_key(node2_parents_cpy);
          // check to see if they are adjacent
          if(!nodes_adjacent_in_cg(cg, node3, node4)) {
            // if they are not, look for node1 -- node3, node1 -- node4
            if(undirected_edge_in_cg(cg, node3, node1) &&
               undirected_edge_in_cg(cg, node4, node1))
            {
              return ORIENT;
            }
          }
        }
        node2_parents_cpy = ill_next(node2_parents_cpy);
      }
    }
    node2_parents = ill_next(node2_parents);
  }
  return UNORIENTABLE;
}


static int meek4(const int node1, const int node2, cmpct_cg_ptr cg) {
  ill_ptr* parents          = get_cmpct_cg_parents(cg);

  ill_ptr node1_parents     = parents[node1];
  while(node1_parents != NULL) {
    if(ill_value(node1_parents) == DIRECTED) {
      int node3 = ill_key(node1_parents);
      if(nodes_adjacent_in_cg(cg, node2, node3)) {
        ill_ptr node3_parents = parents[node3];
        while(node3_parents != NULL) {
            if(ill_value(node3_parents) == DIRECTED) {
              int node4 = ill_key(node3_parents);
              if(undirected_edge_in_cg(cg, node4, node2) &&
                 !nodes_adjacent_in_cg(cg, node1, node4))
              {
                return FLIP;
              }
            }
          node3_parents = ill_next(node3_parents);
        }
      }
    }
    node1_parents = ill_next(node1_parents);
  }
  // now we look through the parents of node2 instead of node1
  ill_ptr node2_parents = parents[node2];
  while(node2_parents != NULL) {
    if(ill_value(node2_parents) == DIRECTED) {
      int node3 = ill_key(node2_parents);
      if(nodes_adjacent_in_cg(cg, node1, node3)) {
        ill_ptr node3_parents = parents[node3];
        while(node3_parents != NULL) {
          if(ill_value(node3_parents) == DIRECTED) {
            int node4 = ill_key(node3_parents);
            if(undirected_edge_in_cg(cg, node4, node1) &&
               !nodes_adjacent_in_cg(cg, node2, node4))
            {
              return ORIENT;
            }
          }
          node3_parents = ill_next(node3_parents);
        }
      }
    }
    node2_parents = ill_next(node2_parents);
  }
  return UNORIENTABLE;
}


static int apply_rule(int* edges_ptr, int i, int node1, int node2,
                  cmpct_cg_ptr cg, int (*meek_rule) (int, int, cmpct_cg_ptr))
{
  int result = meek_rule(node1, node2, cg);

  int n_edges = get_cmpct_cg_n_edges(cg);

  switch(result) {
    case UNORIENTABLE :
      return 0;
    case ORIENT : {
      edges_ptr[i + 2*n_edges] = DIRECTED;
      orient_cmpct_cg_edge(cg, node1, node2);
      return 1;
    }
    case FLIP : {
      edges_ptr[i            ] = node2;
      edges_ptr[i + n_edges  ] = node1;
      edges_ptr[i + 2*n_edges] = DIRECTED;
      Rprintf("filp\n");
      orient_cmpct_cg_edge(cg, node2, node1);
      Rprintf("flip success\n");
      return 1;
    }
  default: return 0; /* this should never happen -- exists to make gcc happy */
  }
}