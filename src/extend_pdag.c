#include <causality.h>
#include <cgraph.h>
#include <int_linked_list.h>
#include <edgetypes.h>

typedef struct cll * cll_ptr;

typedef struct cll {
  ill_ptr children;
  ill_ptr parents;
  cll_ptr next;
  cll_ptr prev;
} cll;

inline int is_sink(cll node) {
  ill_ptr children = node.children;
  while (children != NULL) {
    if(ill_value(children) == DIRECTED)
      return 0;
    children = ill_next(children);
  }
  return 1;
}

/* clique checks each undirected parent of current_node if that undirected
 * parent forms a clique with all the other parents of current_node */
static int clique(cll current_node, cgraph_ptr cg_ptr) {
  ill_ptr parents = current_node.parents;
  ill_ptr tmp     = parents;
  /* look for undirect parents of the node */
  while(tmp != NULL) {
    if(ill_value(tmp) == UNDIRECTED) {
      int node     = ill_key(tmp);
      ill_ptr tmp2 = parents;
      while(tmp2 != NULL) {
        if(ill_key(tmp2) != node) {
          if(!adjacent_in_cgraph(cg_ptr, node , ill_key(tmp2)))
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
  /* TODO */
  current_node.prev->next = current_node.next;
  current_node.next->prev = current_node.prev;
}

SEXP ccf_pdx_wrapper(SEXP Pdag) {
    int * edges_ptr        = calculate_edges_ptr(Pdag);
    int n_nodes            = length(VECTOR_ELT(Pdag,NODES));
    int n_edges            = nrows(VECTOR_ELT(Pdag, EDGES));
    cgraph_ptr cg_ptr      = create_cgraph(n_nodes);
    fill_in_cgraph(cg_ptr, n_edges, edges_ptr);
    ccf_pdx(cg_ptr);
    SEXP Dag = PROTECT(duplicate(Pdag));
    recalculate_edges_from_cgraph(cg_ptr, Dag);
    free_cgraph(cg_ptr);
    UNPROTECT(1);
    return Dag;
}

cgraph_ptr ccf_pdx(cgraph_ptr cg_ptr) {
  int n_nodes            = get_cgraph_n_nodes(cg_ptr);
  cgraph_ptr copy_ptr    = copy_cgraph(cg_ptr);
  cll_ptr nodes          = calloc(n_nodes, sizeof(cll));
  if(nodes == NULL)
    error("Failed to allocate memory for nodes in cf_extend_pdag\n");
    // set up circular linked list
    ill_ptr * parents  = get_cgraph_parents(cg_ptr);
    ill_ptr * children = get_cgraph_children(cg_ptr);
    for(int i = 0; i < n_nodes; ++i) {
      nodes[i].parents   = parents[i];
      nodes[i].children  = children[i];
      nodes[i].next      = nodes + (i + 1) % n_nodes;
      nodes[i].prev      = nodes + (i + n_nodes - 1) % n_nodes;
    }
    cll current_node     = nodes[0];
    int n_nodes_checked  = 0;
    int ll_size          = n_nodes;
    /* Comment needed */
    while(ll_size > 0 && n_nodes_checked < ll_size) {
      if(is_sink(current_node) && clique(current_node, cg_ptr)) {
        orient_in_cgraph(copy_ptr, &current_node - nodes);
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
    if(failure) {
      free_cgraph(copy_ptr);
      copy_ptr = NULL;
    }
    free_cgraph(cg_ptr);
    return copy_ptr;
}
