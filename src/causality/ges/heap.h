#ifndef HEAP_H
#define HEAP_H

struct ges_heap {
    int                   max_size;
    int                   size;
    int                  *indices;
    double               *score_diffs;
    struct ges_operator  *ops;
    struct ges_operator **ops_ptrs;
};

void free_heap(struct ges_heap *hp);
void build_heap(struct ges_heap *hp);
void insert_heap(struct ges_heap *hp, struct ges_operator *op);
void remove_heap(struct ges_heap *hp, int node);
struct ges_heap * create_heap(int max_size, struct ges_operator *ext_ops);
struct ges_operator * peek_heap(struct ges_heap *hp);
void print_heap(struct ges_heap *hp);
#endif
