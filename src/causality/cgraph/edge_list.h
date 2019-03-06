#ifndef EDGE_LIST_H
#define EDGE_LIST_H

#define UNTAGGED 0

/* definition of each node in a linked list */
struct edge_list {
    int   node;
    short edge;
    short tag;
    struct edge_list *next;
} /* 16 bytes */;

void insert_edge(struct edge_list **root, int node, short edge, short tag);
void remove_edge(struct edge_list **root, int node);
void free_edge_list(struct edge_list *root);
void print_edge_list(struct edge_list *root);
struct edge_list * copy_edge_list(struct edge_list *root);
struct edge_list * search_edge_list(struct edge_list *root, int node);
int size_edge_list(struct edge_list *root);
int identical_edge_lists(struct edge_list *e1, struct edge_list *e2);
#endif /* edge_list.h */
