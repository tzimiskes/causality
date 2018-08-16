#include <causality.h>
#include <cgraph.h>
#include <int_linked_list.h>
#include <edgetypes.h>

#define UNKNOWN   -1
#define COMPELLED  1 /* This means directed */
#define REVERSABLE 2 /* This means undirected */

static inline void order_edges(cgraph_ptr cg_ptr, int * sort);
static inline void insertion_sort(ill_ptr list);
static inline void chickering_core(cgraph_ptr cg_ptr, int * sort);

SEXP ccf_chickering_wrapper(SEXP Graph) {
    int * edges       = calculate_edges_ptr(Graph);
    int n_nodes       = length(VECTOR_ELT(Graph, NODES));
    int n_edges       = nrows(VECTOR_ELT(Graph, EDGES));
    cgraph_ptr cg_ptr = create_cgraph(n_nodes);
    fill_in_cgraph(cg_ptr, n_edges, edges);
    free(edges);
    ccf_chickering(cg_ptr);
    SEXP Pattern = PROTECT(duplicate(Graph));
    recalculate_edges_from_cgraph(cg_ptr, Pattern);
    free_cgraph(cg_ptr);
    UNPROTECT(1);
    return Pattern;
}

void ccf_chickering(cgraph_ptr cg_ptr) {
  int * sort_ptr = ccf_sort(cg_ptr);
  order_edges(cg_ptr,     sort_ptr);
  chickering_core(cg_ptr, sort_ptr);
  free(sort_ptr);
  /* we need to recalculate the children after turning it into a pattern.
   * This is because the children in a cgraph currently only contain the
   * true children of the graph (ie no undirected edges) */
}

/* order_edges orders the parents of cg such that the nodes are in descending
 * order according to the sort. */
static inline void order_edges(cgraph_ptr cg_ptr, int * sort) {
  ill_ptr * parents = cg_ptr->parents;
  int n_nodes       = cg_ptr->n_nodes;
  /* can be parallelized */
  for(int i = 0; i < n_nodes; ++i) {
    ill_ptr tmp = parents[i];
    while(tmp != NULL) {
      tmp->value = sort[tmp->key];
      tmp        = tmp->next;
    }
    insertion_sort(parents[i]);
  }
}

/* We need a sorting routine so we can order the edges. Typically, we would use
 * a mergesort routine for linked lists, but I suspect insertion sort will be
 * faster because the average degree of causal graphs is 2-5, and insertion sort
 * is faster than merge sort until we hit 10-50 elements. */
static inline void insertion_sort(ill_ptr list) {
  while(list != NULL) {
    ill_ptr top = list;
    ill_ptr max = list;
    while(top != NULL) {
      if(top->value > max->value)
        max = top;
      top = top->next;
    }
    int list_key   = list->key;
    int list_value = list->value;
    list->key      = max->key;
    list->value    = max->value;
    max->key       = list_key;
    max->value     = list_value;
    list           = list->next;
  }
}

/* This is the core part (i.e, Find-compelled) of Chickering's algorithm
 * to convert DAGs to patterns. */
static inline void chickering_core(cgraph_ptr cg_ptr, int * sort) {
  ill_ptr * parents = cg_ptr->parents;
  int n_nodes       = cg_ptr->n_nodes;
  /* order edges sets the value parameter for each edge, so we need to
   * change the value for everything to UNKNOWN */
  for(int i = 0; i < n_nodes; ++i) {
    ill_ptr tmp_ptr = parents[i];
    while(tmp_ptr != NULL) {
      ill_set_value(tmp_ptr, UNKNOWN);
      tmp_ptr = ill_next(tmp_ptr);
    }
  }
  /* we iterate through the sort to satisfy the max min condition
   * necessary to run this part of the algorithm */
  for(int i = 0; i < n_nodes; ++i) {
    /* by lemma 5 in Chickering, all the incident edges on y are unknown
     * so we don't need to check to see its unordered */
    int y             = sort[i];
    ill_ptr y_parents = parents[y];
    /* if there are incident edges into y, run steps 5-8 of the algorithm.
     * if y has no incident edges, go to the next node in the order */
    if(y_parents != NULL) {
      int x             = ill_key(y_parents);
      ill_ptr x_parents =  parents[x];
      /* for each parent of x, w, where w -> x is compelled
       * check to see if w forms a chain (w -> x -> y)
       * or shielded collider (w -> x -> y and w -> x) */
      while(x_parents != NULL) { /* STEP 5 */
        if(ill_value(x_parents) == COMPELLED) {
          int w       = ill_key(x_parents);
          ill_ptr tmp = y_parents;
          int chain   = 1;
          while(tmp != NULL) {
            /* STEP 7 */
            if(ill_key(tmp) == w) {
              /* the triple forms a shielded collider, so set w -> y compelled
              * and then break out of the loop so we can repeat this for the
              * next parent of x */
              ill_set_value(tmp, COMPELLED);
              chain = 0;
              break;
            }
            tmp = ill_next(tmp);
          }
          /* STEP 6 */
          if(chain) {
            /* reset tmp so we can reiterate through y's parents */
            tmp = y_parents;
            while(tmp != NULL) {
              ill_set_value(tmp, COMPELLED);
              tmp = ill_next(tmp);
            }
            goto EOFL; /* goto end of for loop */
          }
        }
        /* if step 7 is executed, goto the next parent of x */
        x_parents = ill_next(x_parents);
      }
      /* now, we need to search for z, where z -> y, x != z,
       * and z is not a parent of x. That is, an unshielded collider
       * by starting at the second parent (might not exist),
       * we avoid the need to check to see if z = x
       * STEP 7.5: look for an unshielded collider */
      int unshielded_collider = 0;
      ill_ptr tmp = parents[y];
      while(tmp != NULL) {
        if(ill_key(tmp) != x) {
          int z = ill_key(tmp);
          if(!adjacent_in_cgraph(cg_ptr, x, z)) {
            unshielded_collider = 1;
            goto STEP_89;
          }
        }
        tmp = ill_next(tmp);
      }
      STEP_89: {};
      /* STEP 8: if there is an unshielded collider,
       * label all incident edges compelled */
       tmp = parents[y];
      if(unshielded_collider) {
        while(tmp != NULL) {
          ill_set_value(tmp, COMPELLED);
          tmp = ill_next(tmp);
        }
      }
      /* STEP 9, label all unknown edges reversable */
      else {
        while(tmp != NULL) {
          if(ill_value(tmp) == UNKNOWN) {
            unorient_directed_edge(cg_ptr, ill_key(tmp), y);
            tmp = parents[y];
          }
          else
            tmp = ill_next(tmp);
        }
      }
    }
    EOFL:{}; /* End Of For Loop */
  }
}
