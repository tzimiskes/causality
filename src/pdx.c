#include <causality.h>
#include <cgraph.h>
#include <int_linked_list.h>
#include <edgetypes.h>

#define CLIQUE 1
#define NOCLIQUE 2

typedef struct cll * cll_ptr;
typedef struct cll {
  ill_ptr children;
  ill_ptr parents;
  ill_ptr spouses;
  cll_ptr next;
} cll;

inline int is_sink(cll_ptr node) {
  return node->children == NULL;
}

/* clique checks each undirected parent of current if that undirected
 * parent forms a clique with all the other parents of current */
static int forms_clique(cll_ptr node, cgraph_ptr cg_ptr) {
  ill_ptr spouses = node->spouses;
  /* grab a spouse (undirected adjacent) */
  while(spouses != NULL) {
    if(ill_value(spouses) == UNDIRECTED) {
      int spouse      = ill_key(spouses);
      ill_ptr parents = node->parents;
      /* make sure spouse is adjacent to the parents of node */
      while(parents != NULL) {
        if(!adjacent_in_cgraph(cg_ptr, spouse, ill_key(parents)))
          return NOCLIQUE;
        parents = ill_next(parents);
      }
      /* make sure spouse is adjacent to the other spouses of node */
      ill_ptr tmp = node->spouses;
      while(tmp != NULL) {
        int spouse2 = ill_key(tmp);
        if(spouse2 != spouse && !adjacent_in_cgraph(cg_ptr, spouse, spouse2))
          return NOCLIQUE;
        tmp = ill_next(tmp);
      }
    }
    spouses = ill_next(spouses);
  }
  return CLIQUE;
}

static inline void orient_in_cgraph(cgraph_ptr cg_ptr, int node) {
  ill_ptr spouse = cg_ptr->spouses[node];
  while(spouse) {
    orient_undirected_edge(cg_ptr, spouse->key, node);
    spouse = cg_ptr->spouses[node];
  }
}

void remove_node(cll_ptr current, cll_ptr nodes) {
  int node = current - nodes; /* ptr arithemtic */
  /* delete all listings of node in its parents and spouses */
  ill_ptr parents = current->parents;
  while(parents) {
    ill_delete(&nodes[parents->key].children, node);
    parents = parents->next;
  }
  ill_ptr spouses = current->spouses;
  while(spouses) {
    ill_delete(&nodes[spouses->key].spouses, node);
    spouses = spouses->next;
  }
}

SEXP ccf_pdx_wrapper(SEXP Pdag) {
    int * edges_ptr        = calculate_edges_ptr(Pdag);
    int n_nodes            = length(VECTOR_ELT(Pdag,NODES));
    int n_edges            = nrows(VECTOR_ELT(Pdag, EDGES));
    cgraph_ptr cg_ptr      = create_cgraph(n_nodes);
    fill_in_cgraph(cg_ptr, n_edges, edges_ptr);
    free(edges_ptr);
    cg_ptr = ccf_pdx(cg_ptr);
    if(cg_ptr == NULL) {
        return R_NilValue;
    }
    SEXP Dag = PROTECT(duplicate(Pdag));
    recalculate_edges_from_cgraph(cg_ptr, Dag);
    free_cgraph(cg_ptr);
    UNPROTECT(1);
    return Dag;
}

cgraph_ptr ccf_pdx(cgraph_ptr cg_ptr) {
  int n_nodes            = cg_ptr->n_nodes;
  cgraph_ptr copy_ptr    = copy_cgraph(cg_ptr);
  cll_ptr nodes          = calloc(n_nodes, sizeof(cll));
  if(nodes == NULL)
    error("Failed to allocate memory for nodes in cf_extend_pdag\n");
    // set up circular linked list
    ill_ptr * parents  = cg_ptr->parents;
    ill_ptr * spouses  = cg_ptr->children;
    ill_ptr * children = cg_ptr->children;
    for(int i = 0; i < n_nodes; ++i) {
      nodes[i].parents  = parents[i];
      nodes[i].children = children[i];
      nodes[i].spouses  = spouses[i];
      nodes[i].next     = nodes + (i + 1) % n_nodes;
  }
  cll_ptr current   = nodes;
  cll_ptr prev      = nodes + (n_nodes - 1);
  int n_checked     = 0;
  int ll_size       = n_nodes;
  /* Comment needed */
  while(ll_size > 0 && n_checked < ll_size) {
    if(is_sink(current) && forms_clique(current, cg_ptr)) {
      orient_in_cgraph(copy_ptr, current - nodes);
      remove_node(current, nodes);
      prev->next = current->next;
      ll_size--;
      n_checked = 0;
    }
    else {
      n_checked++;
    }
    prev    = current;
    current = current->next;
  }
  free(nodes);
  free_cgraph(cg_ptr);
  /* check to see if pdx failed to generate an extension. If there is a
   * failure, free the copy_ptr and set it to NULL. */
  int failure = ll_size  > 0 ? 1 : 0;
  if(failure) {
    free_cgraph(copy_ptr);
    copy_ptr = NULL;
  }
  return copy_ptr;
}
