#include"int_linked_list.h"

#ifndef _CMPCT_CG_
#define _CMPCT_CG_

typedef struct cmpct_cg* cmpct_cg_ptr;
typedef struct cmpct_cg cmpct_cg;

cmpct_cg_ptr create_cmpct_cg(int n_nodes, int n_edges);
void free_cmpct_cg(cmpct_cg_ptr cg);
void fill_in_cmpct_cg(cmpct_cg_ptr cg, int* edges_ptr,
                      void (*insert_fp)(ill_ptr*, int, int, int, ill_ptr));

void print_cmpct_cg(cmpct_cg_ptr cg);

ill_ptr* get_cmpct_cg_parents(cmpct_cg_ptr cg);
int get_cmpct_cg_n_edges (cmpct_cg_ptr cg);
int get_cmpct_cg_n_nodes (cmpct_cg_ptr cg);

int edge_undirected_in_cg(cmpct_cg_ptr cg, const int node1, const int node2);
int adjacent_in_cg(cmpct_cg_ptr cg, const int node1, const int node2);
int edge_directed_in_cg(cmpct_cg_ptr cg, const int parent, const int child);
void orient_cmpct_cg_edge(cmpct_cg_ptr cg, int node1, int node2,
                          void (*insert_fp)(ill_ptr*, int, int, int, ill_ptr));
#endif
