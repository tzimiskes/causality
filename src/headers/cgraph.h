#ifndef _CGRAPH_H_
#define _CGRAPH_H_

#include <int_linked_list.h>

#define UNDEFINED  0

typedef struct cgraph * cgraph_ptr;
typedef struct cgraph {
  ill_ptr * parents;
  ill_ptr * children;
  int       n_nodes;
  int       n_edges;
} cgraph;

cgraph_ptr create_cgraph(int n_nodes);
void fill_in_cgraph(cgraph_ptr cg_ptr, int n_edges, int * edges_ptr);
void add_node_to_cgraph(cgraph_ptr cg_ptr, int node1, int node2, int edge_type);
void free_cgraph(cgraph_ptr cg_ptr);

void print_cgraph(cgraph_ptr cg_ptr);

ill_ptr * get_cgraph_parents(cgraph_ptr cg_ptr);
ill_ptr * get_cgraph_children(cgraph_ptr cg_ptr);
int get_cgraph_n_nodes(cgraph_ptr cg_ptr);
int get_cgraph_n_edges(cgraph_ptr cg_ptr);

int edge_undirected_in_cgraph(cgraph_ptr cg_ptr, int node1, int node2);
int adjacent_in_cgraph(cgraph_ptr cg_ptr, int node1, int node2);
int edge_directed_in_cgraph(cgraph_ptr cg_ptr, int parent, int child);

#endif
