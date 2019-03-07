#ifndef CGRAPH_H_
#define CGRAPH_H_

#include <cgraph/edge_list.h>

struct cgraph {
    struct edge_list **parents;
    struct edge_list **spouses;
    struct edge_list **children;
    int       n_nodes;
    int       n_edges;
}; /* 32 bytes */

struct cgraph * create_cgraph(int n_nodes);
struct cgraph * copy_cgraph(struct cgraph *cg);
void free_cgraph(struct cgraph *cg);
void add_edge_to_cgraph(struct cgraph *cg, int x, int y, short edge);
void delete_edge_from_cgraph(struct cgraph *cg, int x, int y, short edge);
void orient_undirected_edge(struct cgraph *cg, int x, int y);
void unorient_directed_edge(struct cgraph *cg, int x, int y);
void print_cgraph(struct cgraph *cg);
int edge_undirected_in_cgraph(struct cgraph *cg, int x, int y);
int edge_directed_in_cgraph(struct cgraph *cg, int x, int y);
int adjacent_in_cgraph(struct cgraph *cg, int x, int y);
int identical_in_cgraphs(struct cgraph *cg1, struct cgraph *cg2, int node);
#endif
