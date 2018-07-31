#include "headers/causality.h"
#include "headers/int_linked_list.h"
#include "headers/edgetypes.h"
#include "headers/cgraph.h"


cgraph create_cgraph(int n_nodes) {
  cgraph cg;
  cg.n_nodes  = n_nodes;
  cg.n_edges  = UNDEFINED; /* 0 */
  cg.parents  = create_ptr_to_ill_ptr(n_nodes);
  if(cg.parents == NULL)
    error("Failed to allocate memory for parents in cgraph!\n");
  cg.children = create_ptr_to_ill_ptr(n_nodes);
  if(cg.children == NULL)
    error("Failed to allocate memory for children in cgraph!\n");
  return cg;
}

void fill_in_cgraph(cgraph cg, int n_edges, int * edges_ptr) {
  cg.n_edges          = n_edges;
  int * node1_ptr     = edges_ptr;
  int * node2_ptr     = edges_ptr + n_edges;
  int * edge_type_ptr = edges_ptr + 2 * n_edges;
  for(int i = 0; i < n_edges; ++i) {
    int node1         = node1_ptr[i];
    int node2         = node2_ptr[i];
    int edge_type     = edge_type_ptr[i];
    if(is_directed(edge_type)) {
      cg.children[node1] = ill_insert(cg.children[node1], node2, edge_type);
      cg.parents[node2]  = ill_insert(cg.parents[node2],  node1, edge_type);
    }
    else {
      cg.parents[node1]  = ill_insert(cg.parents[node1], node2, edge_type);
      cg.parents[node2]  = ill_insert(cg.parents[node2], node1, edge_type);
    }
  }
}

void add_node_to_cgraph(cgraph cg, int node1, int node2, int edge_type) {
  if(is_directed(edge_type)) {
    cg.children[node1] = ill_insert(cg.children[node1], node2, edge_type);
    cg.parents[node2]  = ill_insert(cg.parents[node2],  node1, edge_type);
  }
  else {
    cg.parents[node1]  = ill_insert(cg.parents[node1], node2, edge_type);
    // cg.parents[node2]  = ill_insert(cg.parents[node2], node1, edge_type);
  }
}
void free_cgraph(cgraph cg) {
  for(int i = 0; i < cg.n_nodes; ++i) {
    ill_free(cg.parents[i]);
    ill_free(cg.children[i]);
  }
  free(cg.parents);
  free(cg.children);
}

int adjacent_in_cgraph(cgraph cg, int node1, int node2) {
  /* look through the parents of node1 to see if node2 is a parent */
  ill_ptr node1_parents = cg.parents[node1];
  while(node1_parents != NULL) {
    if(ill_key(node1_parents) == node2)
      return 1;
    node1_parents = ill_next(node1_parents);
  }
  /* look through the parents of node2 to see if node1 is a parent */
  ill_ptr node2_parents = cg.parents[node2];
  while(node2_parents != NULL) {
    if(ill_key(node2_parents) == node1)
      return 1;
    node2_parents = ill_next(node2_parents);
  }
  return 0;
}

int edge_undirected_in_cgraph(cgraph cg, int node1, int node2) {
  ill_ptr node1_parents = cg.parents[node1];
  while(node1_parents != NULL) {
    if(ill_key(node1_parents) == node2)
    return ill_value(node1_parents) == UNDIRECTED;
    node1_parents = ill_next(node1_parents);
  }
  ill_ptr node2_parents = cg.parents[node2];
  while(node2_parents != NULL) {
    if(ill_key(node2_parents) == node1)
    return ill_value(node2_parents) == UNDIRECTED;
    node2_parents = ill_next(node2_parents);
  }
  return 0;
}

int edge_directed_in_cgraph(cgraph cg, int parent, int child) {
  ill_ptr children = cg.children[parent];
  while(children != NULL) {
    if(ill_key(children) == parent)
      return ill_value(children) == DIRECTED;
    children = ill_next(children);
  }
  return 0;
}

void print_cgraph(cgraph cg) {
  for (int i = 0; i < cg.n_nodes; ++i) {
    Rprintf("Parents of %i: ", i);
    ill_print(cg.parents[i]);
    Rprintf("Children of  %i: ", i);
    ill_print(cg.children[i]);
  }
}

ill_ptr * get_cgraph_parents(cgraph cg) {
  return cg.parents;
}
ill_ptr * get_cgraph_children(cgraph cg) {
  return cg.children;
}

int get_cgraph_n_nodes(cgraph cg) {
  return cg.n_nodes;
}

int get_cgraph_n_edges(cgraph cg) {
  return cg.n_edges;
}
