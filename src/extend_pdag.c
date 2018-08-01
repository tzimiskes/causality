#include <causality.h>
#include <cmpct_cg.h>
#include <int_linked_list.h>
#include <edgetypes.h>

typedef struct cll * cll_ptr;

typedef struct cll {
  ill_ptr children;
  ill_ptr parents;
  cll_ptr next;
  cll_ptr prev;
} cll;

int adjacent(cll current_node, int node) {
  ill_ptr parents  = current_node.parents;
  while(parents != NULL) {
    if(ill_key(parents) == node)
      return 1;
    parents = ill_next(parents);
  }
  ill_ptr children = current_node.children;
  while(children != NULL) {
    if(ill_key(children) == node)
      return 1;
    children = ill_next(children);
  }
  return 0;
}

inline int is_sink(cll current_node) {
  return current_node.children == NULL;
}

/* check to see if all undirected parents are adj to all other parents */
int check_condition(cll current_node, cll_ptr nodes) {
  ill_ptr parents = current_node.parents;
  ill_ptr tmp     = parents;
  /* look for undirect parents of the node */
  while(tmp != NULL) {
    if(ill_value(tmp) == UNDIRECTED) {
      int node = ill_key(tmp);
      ill_ptr tmp2 = parents;
      while(tmp2 != NULL) {
        if(ill_key(tmp2) != node) {
          if(!adjacent(nodes[node], ill_key(tmp2)))
            return 0;
        }
        tmp2 = ill_next(tmp2);
      }
    }
    tmp = ill_next(tmp);
  }
  return 1;
}



void remove_node(cll current_node, cll_ptr nodes) {
  int node = &current_node - nodes; /* ptr arithmetic */

/* we need to current node in all of its adjacents */
  ill_ptr parents = current_node.parents;
  while(parents != NULL) {
    int parent = ill_key(parents);
      if(ill_value(parents) == DIRECTED) {
        ill_delete(&(nodes[parent].children), node);
      }
      else {
      /* parent is a undirected parent of node, so node is also an
       * undirected parent of parent */
        ill_delete(&(nodes[parent].parents), node);
      }
    parents = ill_next(parents);
  }

  // unlink the node from the circular linked list

  current_node.prev->next = current_node.next;
  current_node.next->prev = current_node.prev;
}

SEXP ccf_pdx_wrapper(SEXP Pdag) {
  /* TODO */
  return Pdag;
}


void ccf_pdx(cgraph_ptr cg_ptr) {
  int n_nodes = get_cgraph_n_nodes(cg_ptr);
  cgraph_ptr cg_copy_ptr = copy_cgraph(cg_ptr);

  cll_ptr nodes = calloc(n_nodes, sizeof(cll));
  if(nodes == NULL)
    error("Failed to allocate memory for nodes in cf_extend_pdag\n");

    // set up circular linked list
    ill_ptr * parents_copy  = get_cgraph_parents(cg_copy_ptr);
    ill_ptr * children_copy = get_cgraph_children(cg_copy_ptr);
    for(int i = 0; i < n_nodes; ++i) {
      nodes[i].parents   = parents_copy[i];
      nodes[i].children  = children_copy[i];
      nodes[i].next      = nodes + (i + 1) % n_nodes;
      nodes[i].prev      = nodes + (i + n_nodes - 1) % n_nodes;
    }
    cll current_node = nodes[0];
    int n_nodes_checked  = 0;
    int ll_size          = n_nodes;
    /* Comment needed */
    while(ll_size > 0 && n_nodes_checked < ll_size) {
      if(is_sink(current_node) && check_condition(current_node, nodes)) {
        orient_in_cgraph(cg_ptr, &current_node - nodes);
        remove_node(current_node, nodes);
        ll_size--;
        n_nodes_checked = 0;
      }
      else {
        n_nodes_checked++;
      }
      current_node = *(current_node.next);
    }
    // check to see if the algorithm failed to generate an extension
    int failure = ll_size  > 0 ? 1 : 0;

    free(nodes);
    free_cgraph(cg_copy_ptr);
}
