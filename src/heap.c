#include <stdlib.h>
#include <float.h>

#include "headers/causality.h"
#include "headers/heap.h"

struct ges_heap * create_heap(const int max_size, struct ges_operator *ext_ops)
{
    struct ges_heap *hp = malloc(sizeof(struct ges_heap));
    hp->max_size    = max_size;
    hp->size        = max_size;
    hp->ext_ops     = ext_ops;
    hp->ext_op_ptrs = malloc(max_size * sizeof(struct ges_operator *));
    hp->indices     = malloc(max_size * sizeof(int));
    if (!hp->ext_op_ptrs || !hp->ext_ops) {
        CAUSALITY_ERROR("Failed to allocate memory for heap!\n");
        return NULL;
    }
    for (int i = 0; i < max_size; ++i)
        hp->ext_op_ptrs[i] = hp->ext_ops + i;
    return hp;
}

void free_heap(struct ges_heap *hp)
{
    free(hp->ext_op_ptrs);
    free(hp->indices);
    free(hp);
}

static int parent(int i)
{
    return (i - 1) / 2;
}

static int left(int i)
{
    return i * 2 + 1;
}

static int right(int i)
{
    return i * 2 + 2;
}

static inline void swap(struct ges_heap *hp, int i, int j)
{
    struct ges_operator *p = hp->ext_op_ptrs[i];
    hp->ext_op_ptrs[i]     = hp->ext_op_ptrs[j];
    hp->ext_op_ptrs[j]     = p;
    int ext_i = hp->ext_op_ptrs[i] - hp->ext_ops;
    int ext_j = hp->ext_op_ptrs[j] - hp->ext_ops;
    int k     = hp->indices[ext_i];
    hp->indices[ext_i] = hp->indices[ext_j];
    hp->indices[ext_j] = k;
}

/* todo make non recursive */
static inline void min_heapify(struct ges_heap *hp, int i)
{
    int l   = left(i);
    int r   = right(i);
    int min = i;
    if (l < hp->size && (hp->ext_op_ptrs[l]->score_diff <
                            hp->ext_op_ptrs[min]->score_diff))
        min = l;
    if (r < hp->size && (hp->ext_op_ptrs[r]->score_diff <
                            hp->ext_op_ptrs[min]->score_diff))
        min = r;
    if (i != min) {
        swap(hp, i, min);
        min_heapify(hp, min);
    }
}

void build_heap(struct ges_heap *hp)
{
    hp->size = hp->max_size;
    for (int i = 0; i < hp->size; ++i) {
        hp->ext_op_ptrs[i] = hp->ext_ops + i;
        hp->indices[i]     = i;
    }
    for (int i = (hp->size - 1) / 2; i >= 0; --i)
        min_heapify(hp, i);
}

static void pop_heap(struct ges_heap *hp)
{
    hp->ext_op_ptrs[0] = hp->ext_op_ptrs[hp->size - 1];
    int ext_i = hp->ext_op_ptrs[0] - hp->ext_ops;
    hp->indices[ext_i] = 0;
    hp->size      -= 1;
    min_heapify(hp, 0);
}

struct ges_operator *peek_heap(struct ges_heap *hp)
{
    if (hp->size < 1)
        return NULL;
    return hp->ext_op_ptrs[0];
}

static void decrease_key(struct ges_heap *hp, int i, double score_diff)
{
    hp->ext_op_ptrs[i]->score_diff = score_diff;
    while (i > 0 && (hp->ext_op_ptrs[i]->score_diff <
                        hp->ext_op_ptrs[parent(i)]->score_diff)) {
        swap(hp, parent(i), i);
        i = parent(i);
    }
}

void insert_heap(struct ges_heap *hp, struct ges_operator *op)
{
    double score_diff = op->score_diff;
    op->score_diff = DBL_MAX;
    hp->ext_op_ptrs[hp->size] = op;
    int ext_i =  op - hp->ext_ops;
    hp->indices[ext_i] = hp->size;
    decrease_key(hp, hp->size, score_diff);
    hp->size += 1;
}

/* delete the entry of a node through the heap */
void remove_heap(struct ges_heap *hp, int node)
{
    decrease_key(hp, hp->indices[node], -DBL_MAX);
    pop_heap(hp);
}
