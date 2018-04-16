#include "headers/causality.h"
#include "headers/int_linked_list.h"
#include "headers/edgetypes.h"

typedef struct cmpct_cg* cmpct_cg_ptr;
typedef struct cmpct_cg {
  ill_ptr* parents;
  int n_nodes;
  int n_edges;
} cmpct_cg;

cmpct_cg_ptr create_cmpct_cg(int n_nodes, int n_edges) {
  cmpct_cg_ptr cg = malloc(sizeof(cg));
  if (cg == NULL)
    error("Failed to allocate memory for cg pointer in create_cmpct_cg!\n");

  cg->n_nodes = n_nodes;
  cg->n_edges = n_edges;
  /*
   * allocate memory for parents representation and 1 to hold the pointer
   * to the nodes. this saves space in the struct so that its only 16 bytes
   */
  ill_ptr* parents = create_ptr_to_ill_ptr(n_nodes + 1);
  if(parents == NULL)
    error("Failed to allocate memory for parents pointer!\n");
  // allocate nodes pointer and set it to be the last element in parents
  parents[n_nodes] = create_ill_ptr(n_edges);
  if(parents == NULL)
    error("Failed to allocate memory for nodes pointer!\n");
  cg->parents = parents;
  return cg;
}
/*
 * fill_in_cmpct_cg fills in the given cg using edges_ptr and the function
 * pointer to an ill insertion routine
 */
void fill_in_cmpct_cg(cmpct_cg_ptr cg, int* edges_ptr,
                      void (*insert_fp)(ill_ptr*, int, int, int, ill_ptr))
{
  int n_edges      = cg->n_edges;
  int n_edges_2t   = n_edges*2; /* saves some computations */
  ill_ptr* parents = cg->parents;
  ill_ptr nodes    = parents[cg->n_nodes];
  // fill in cg
  for(int i = 0; i < n_edges; ++i) {
    int parent = edges_ptr[i                 ];
    int child  = edges_ptr[i + n_edges       ];
    int edge_type  = edges_ptr[i + n_edges_2t];
    insert_fp(&parents[child], parent, edge_type, i, nodes);
  }
}

void free_cmpct_cg(cmpct_cg_ptr cg) {
  // free nodes
  free(cg->parents[cg->n_nodes]);
  // free parents
  free(cg->parents);
  // free cg
  free(cg);
}

int get_cmpct_cg_n_edges (cmpct_cg_ptr cg) {
  return cg->n_edges;
}

int get_cmpct_cg_n_nodes (cmpct_cg_ptr cg) {
  return cg->n_nodes;
}

ill_ptr* get_cmpct_cg_parents(cmpct_cg_ptr cg) {
  return cg->parents;
}

void print_cmpct_cg(cmpct_cg_ptr cg) {
  for(int i = 0; i < cg->n_nodes; ++i) {
    Rprintf("Parent: %i\n", i);
    ill_print(cg->parents[i]);
  }
}
/*
 * adjacent_in_cg returns 1 if nodes 1 and 2 are adjacent in the cg,
 * and 0 if not
 */
int adjacent_in_cg(cmpct_cg_ptr cg, const int node1, const int node2) {
  // look through the parents of node1 to see if node2 is a parent
  ill_ptr node1_parents = cg->parents[node1];
  while(node1_parents != NULL) {
    if(ill_key(node1_parents) == node2)
    return 1;
    node1_parents = ill_next(node1_parents);
  }
  // look through the parents of node2 to see if node1 is a parent
  ill_ptr node2_parents = cg->parents[node2];
  while(node2_parents != NULL) {
    if(ill_key(node2_parents) == node1)
    return 1;
    node2_parents = ill_next(node2_parents);
  }
  return 0;
}
/*
 * edge_undirected_in_cg returns one if node1 and node2 have an undirected edge
 * between them in the cg, and 0 otherwise
 */
int edge_undirected_in_cg(cmpct_cg_ptr cg, const int node1, const int node2)  {
  ill_ptr node1_parents = cg->parents[node1];
  while(node1_parents != NULL) {
    if(ill_key(node1_parents) == node2)
    return ill_value(node1_parents) == UNDIRECTED;
    node1_parents = ill_next(node1_parents);
  }
  ill_ptr node2_parents = cg->parents[node2];
  while(node2_parents != NULL) {
    if(ill_key(node2_parents) == node1)
    return ill_value(node2_parents) == UNDIRECTED;
    node2_parents = ill_next(node2_parents);
  }
  return 0;
}

/* orient node1 --> node2 */
void orient_cmpct_cg_edge(cmpct_cg_ptr cg, int node1, int node2,
                          void (*insert_fp)(ill_ptr*, int, int, int, ill_ptr))
  {
  ill_ptr* parents = cg->parents;
  // check to see if node1 is already a parent of node2
  ill_ptr edge = ill_search(parents[node2], node1);
  // if edge is not NULL, set the edge type to directed
  if(edge != NULL) {
    ill_set_value(edge, DIRECTED);
  }
  else { /* node1 is a child of node2, so we need to switch the parents */
    ill_ptr nodes         = parents[cg->n_nodes];
    ill_ptr node1_parents = parents[node1];
    int index             = -1;
    // if the first element in node1_parents is node2, remove it and set root to
    // be whatever is supposed to be next
    if(ill_key(node1_parents) == node2) {
      index = (int) (node1_parents - nodes); /* ptr arithmetic -- gives offset */
      // remove the node and set root to be the next node in the list
      cg->parents[node1] = ill_next(parents[node1]);
    }
    // now, we need to search node1_parents for node2, record its index,
    // and then relink up the linked list so node2 can be reused
    else {
      while(node1_parents != NULL) {
        ill_ptr next = ill_next(node1_parents);
        if(ill_key(next) == node2) {
          index = (int) (next - nodes); /* ptr arithmetic */
          ill_set_next(node1_parents, ill_next(next));
          break;
        }
        node1_parents = next;
      }
    }
    insert_fp(&parents[node2], node1, DIRECTED, index, nodes);
  }
}
