#ifndef CGRAPH_H_
#define CGRAPH_H_

#include "int_linked_list.h"

#define UNDEFINED  0

struct cgraph {
    struct ill **parents;
    struct ill **spouses;
    struct ill **children;
    int       n_nodes;
    int       n_edges;
}; /* 32 bytes */

struct cgraph * create_cgraph(int n_nodes);
void            fill_in_cgraph(struct cgraph *cg, int n_edges, int *edges);
void            add_edge_to_cgraph(struct cgraph *cg, int node1, int node2,
                                                      int edge);
void            delete_edge_from_cgraph(struct cgraph *cg, int node1, int node2,
                                                           int edge);
void            free_cgraph(struct cgraph *cg);
struct cgraph * copy_cgraph(struct cgraph *cg);
void            print_cgraph(struct cgraph *cg);
int             edge_undirected_in_cgraph(struct cgraph *cg, int x,
                                                             int y);

int             identical_in_cgraphs(struct cgraph *cg1, struct cgraph *cg2,
                                                         int node);
int             adjacent_in_cgraph(struct cgraph *cg, int x, int y);
int             edge_directed_in_cgraph(struct cgraph *cg, int parent,
                                                           int child);
void            orient_undirected_edge(struct cgraph *cg, int parent,
                                                          int child);
void            unorient_directed_edge(struct cgraph *cg, int parent,
                                                          int child);
#endif
