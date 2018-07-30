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

void print_cgraph(cgraph cg) {
  for (int i = 0; i < cg.n_nodes; ++i) {
    Rprintf("Parent %i: ", i);
    ill_print(cg.parents[i]);
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
