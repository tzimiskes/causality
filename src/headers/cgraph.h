#ifndef _CGRAPH_H_
#define _CGRAPH_H_

#include "int_linked_list.h"

#define COMPACT 1
#define NONCOMPACT 0
#define UNDEFINED 0

typedef struct cgraph * cgraph_ptr;
typedef struct cgraph {
  ill_ptr * parents;
  ill_ptr * children;
  int       n_nodes;
  int       n_edges;
} cgraph;

cgraph create_cgraph(int n_nodes);
void fill_in_cgraph(cgraph cg, int n_edges, int * edges_ptr);
void add_node_to_cgraph(cgraph cg, int node1, int node2, int edge_type);
void free_cgraph(cgraph cg);

void print_cgraph(cgraph cg);


ill_ptr * get_cgraph_parents(cgraph cg);
ill_ptr * get_cgraph_children(cgraph cg);
int get_cgraph_n_nodes(cgraph cg);
int get_cgraph_n_edges(cgraph cg);

int edge_undirected_in_cg(cgraph cg, const int node1, const int node2);
int adjacent_in_cg(cgraph cg, const int node1, const int node2);
int edge_directed_in_cg(cgraph cg, const int parent, const int child);
void orient_cmpct_cg_edge(cgraph cg, int node1, int node2,
                          void (*insert_fp)(ill_ptr*, int, int, int, ill_ptr));
#endif
