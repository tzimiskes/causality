#ifndef _CGRAPH_H_
#define _CGRAPH_H_

#include <int_linked_list.h>

#define UNDEFINED  0

typedef struct cgraph * cgraph_ptr;
typedef struct cgraph {
  ill_ptr * parents;
  ill_ptr * spouses;
  ill_ptr * children;
  int       n_nodes;
  int       n_edges;
} cgraph; /* 32 bytes */

cgraph_ptr create_cgraph(int n_nodes);
void fill_in_cgraph(cgraph_ptr cg_ptr, int n_edges, int * edges_ptr);
void add_edge_to_cgraph(cgraph_ptr cg_ptr, int node1, int node2, int edge);
void delete_edge_from_cgraph(cgraph_ptr cg_ptr, int node1, int node2, int edge);
void free_cgraph(cgraph_ptr cg_ptr);
cgraph_ptr copy_cgraph(cgraph_ptr cg_ptr);

void print_cgraph(cgraph_ptr cg_ptr);

int edge_undirected_in_cgraph(cgraph_ptr cg_ptr, int node1, int node2);
int adjacent_in_cgraph(cgraph_ptr cg_ptr, int node1, int node2);
int edge_directed_in_cgraph(cgraph_ptr cg_ptr, int parent, int child);
void orient_undirected_edge(cgraph_ptr cg_ptr, int parent, int child);
void unorient_directed_edge(cgraph_ptr cg_ptr, int parent, int child);
#endif
