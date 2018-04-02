#include"headers/causality.h"
#include"headers/edgetypes.h"
#include"headers/int_linked_list.h"
#include"headers/cmpct_cg.h"

#define ORIENT 2
#define FLIP 1
#define UNORIENTABLE 0

/* these are the four meek rules as described by meek(1995). A better discussion
 *  is found in pearl(2009)
 * each rule is described where it is implemented
 * these functions either return ORIENT, FLIP, or UNORIENTABLE
 */
static int meek1(int node1, int node2, cmpct_cg_ptr cg);
static int meek2(int node1, int node2, cmpct_cg_ptr cg);
static int meek3(int node1, int node2, cmpct_cg_ptr cg);
static int meek4(int node1, int node2, cmpct_cg_ptr cg);

/*
 * apply_meek_rule applied the selected meek rule (passed in by function pointer)
 * it returns 1 if the rule was applied, and 0 if not
 */
static int apply_meek_rule(int* edges_ptr, int i, int node1, int node2,
                  cmpct_cg_ptr cg, int (*meek_rule) (int, int, cmpct_cg_ptr));

/*
 * meek_rules take in a PDAG and maximially orients it by repeatedly applying
 * the four meek rules
 * it currently returns an updated edge matrix
 */
SEXP meek_rules(SEXP pdag) {

  int n_nodes    = length(VECTOR_ELT(pdag, NODES));
  SEXP edges     = PROTECT(duplicate(VECTOR_ELT(pdag, EDGES)));
  int* edges_ptr = INTEGER(edges);
  int n_edges    = nrows(edges);

   /*
   * generate underlying causal graph represntation (in this case, a compact
   * causal graph, and fill it in using regular insertion
   */
  cmpct_cg_ptr cg_ptr = create_cmpct_cg(n_nodes, n_edges);
  fill_in_cmpct_cg(cg_ptr, edges_ptr, ill_insert2);
  /*
   * idea for loop: index through the edges of graph, and attempt to apply meek
   * rules to unidrected edges. repeat this process until no rules are applied
   */
  int rule_applied;
  do {
    rule_applied = 0;
    for(int i = 0; i < n_edges; ++i) {
      int edge_type = edges_ptr[i + 2*n_edges];
      // check to see if the edge is undirected
      if(edge_type == UNDIRECTED) {
        // if it is, grab the nodes involved, and attempt to apply meek rules
        int node1 = edges_ptr[i          ];
        int node2 = edges_ptr[i + n_edges];

        rule_applied = apply_meek_rule(edges_ptr, i, node1, node2, cg_ptr, meek1);
        if(rule_applied) /* if a rule is applied successfully, skip the rest */
          goto EOFL;
        rule_applied = apply_meek_rule(edges_ptr, i, node1, node2, cg_ptr, meek2);
        if(rule_applied)
          goto EOFL;
        rule_applied = apply_meek_rule(edges_ptr, i, node1, node2, cg_ptr, meek3);
        if(rule_applied)
          goto EOFL;
        rule_applied = apply_meek_rule(edges_ptr, i, node1, node2, cg_ptr, meek4);
        EOFL : {}
      }
    }
     R_CheckUserInterrupt();
  } while(rule_applied);
  // free malloc'd memory
  free_cmpct_cg(cg_ptr);
  UNPROTECT(1);
  return(edges);
}

/*
 * meek rule one:
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
      if(!adjacent_in_cg(cg, node2, node3))
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
      if(!adjacent_in_cg(cg, node1, node3))
        return FLIP;
    }
    node2_parents = ill_next(node2_parents);
  }
  return UNORIENTABLE;
}

/*
 * meek rule 2: look for node3 such that, node3 --> node1, node2 --> node3. If
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

/*
 * meek rule 3: orient node1 --- node2 into node2 --> node1 when there exists
 * chains node2 --- node3 --> node1 and node2 --- node4 --> node1, with
 * !adj(node3, node4)
 *
 * reverse case: chains node1 --- node3 --> node2 and node1 --- node4 --> node2,
 * with !adj(node3, node4)
 */
static int meek3(const int node1, const int node2, cmpct_cg_ptr cg) {
  ill_ptr* parents          = get_cmpct_cg_parents(cg);

  // look for node3 --> node1
  ill_ptr node1_parents     = parents[node1];
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
          if(!adjacent_in_cg(cg, node3, node4)) {
            // if they are not, look for node2 -- node3, node2 -- node4
            if(edge_undirected_in_cg(cg, node3, node2) &&
               edge_undirected_in_cg(cg, node4, node2))
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
  ill_ptr node2_parents     = parents[node2];
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
          if(!adjacent_in_cg(cg, node3, node4)) {
            // if they are not, look for node1 -- node3, node1 -- node4
            if(edge_undirected_in_cg(cg, node3, node1) &&
               edge_undirected_in_cg(cg, node4, node1))
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

/*
 *  meek4 rule4: orient node1 --> node2 if there is a chain
 * node4 --> node3 --> node2, node1 --- node4, with adj(node3, node1) and
 * !adj(node2, node4)
 */
static int meek4(const int node1, const int node2, cmpct_cg_ptr cg) {
  ill_ptr* parents          = get_cmpct_cg_parents(cg);

  ill_ptr node1_parents     = parents[node1];
  while(node1_parents != NULL) {
    if(ill_value(node1_parents) == DIRECTED) {
      int node3 = ill_key(node1_parents);
      if(adjacent_in_cg(cg, node2, node3)) {
        ill_ptr node3_parents = parents[node3];
        while(node3_parents != NULL) {
            if(ill_value(node3_parents) == DIRECTED) {
              int node4 = ill_key(node3_parents);
              if(edge_undirected_in_cg(cg, node4, node2) &&
                 !adjacent_in_cg(cg, node1, node4))
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
      if(adjacent_in_cg(cg, node1, node3)) {
        ill_ptr node3_parents = parents[node3];
        while(node3_parents != NULL) {
          if(ill_value(node3_parents) == DIRECTED) {
            int node4 = ill_key(node3_parents);
            if(edge_undirected_in_cg(cg, node4, node1) &&
               !adjacent_in_cg(cg, node2, node4))
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

/*
 * apply_meek_rule applied the selected meek rule (passed in by function pointer)
 * it returns 1 if the rule was applied, and 0 if not
 */
static int apply_meek_rule(int* edges_ptr, int i, int node1, int node2,
                  cmpct_cg_ptr cg, int (*meek_rule) (int, int, cmpct_cg_ptr))
{
  int result  = meek_rule(node1, node2, cg);
  int n_edges = get_cmpct_cg_n_edges(cg);

  switch(result) {
    case UNORIENTABLE :
      return 0;
    case ORIENT :
      orient_cmpct_cg_edge(cg, node1, node2); /* update the edge in cg */
  case FLIP : {
      /* flip node1 and node2 */
      edges_ptr[i            ] = node2;
      edges_ptr[i + n_edges  ] = node1;
      orient_cmpct_cg_edge(cg, node2, node1); /* update the edge in cg */
    }
  default : {
    // set the edge tpye to be directed and return
    edges_ptr[i + 2*n_edges] = DIRECTED;
    return 1;
    }
  }
}
