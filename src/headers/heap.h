#ifndef HEAP_H
#define HEAP_H

struct ges_heap {
    int                   max_size;
    int                   size;
    struct ges_operator **ext_op_ptrs;
    struct ges_operator  *ext_ops;
    int                  *indices;
};

struct ges_heap * create_heap(const int max_size, struct ges_operator  *ext_ops);
void          free_heap(struct ges_heap *hp);
void          build_heap(struct ges_heap *hp);
struct ges_operator * peek_heap(struct ges_heap *hp);
void          insert_heap(struct ges_heap *hp, struct ges_operator *op);
void          remove_heap(struct ges_heap *hp, int node);
#endif
