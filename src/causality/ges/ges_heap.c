#include <stdlib.h>
#include <float.h>

#include <causality.h>

#include <ges/ges_internal.h>

struct ges_heap * create_heap(int max_size, struct ges_operator *ext_ops)
{
    struct ges_heap *heap = calloc(1, sizeof(struct ges_heap));
    if (!heap)
        goto ERR;
    heap->max_size = max_size;
    heap->size = max_size;
    heap->ops  = ext_ops;
    heap->ops_ptrs = malloc(max_size * sizeof(struct ges_operator *));
    heap->indices  = malloc(max_size * sizeof(int));
    heap->score_diffs = malloc(max_size * sizeof(double));
    if (!heap->score_diffs || !heap->ops_ptrs || !heap->indices)
        goto ERR;
    if (0) {
        ERR:
        CAUSALITY_ERROR("Failed to allocate memory for GES heap.\n");
        if (heap)
            free_heap(heap);
        heap = NULL;
    }
    return heap;
}

void free_heap(struct ges_heap *heap)
{
    free(heap->ops_ptrs);
    free(heap->indices);
    free(heap->score_diffs);
    free(heap);
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

static void swap(struct ges_heap *heap, int i, int j)
{
    struct ges_operator *op = heap->ops_ptrs[i];
    double score_diff       = heap->score_diffs[i];
    int abs_i               = heap->ops_ptrs[i] - heap->ops;
    int abs_j               = heap->ops_ptrs[j] - heap->ops;
    int k                   = heap->indices[abs_i];
    heap->ops_ptrs[i]    = heap->ops_ptrs[j];
    heap->score_diffs[i] = heap->score_diffs[j];
    heap->ops_ptrs[j]    = op;
    heap->score_diffs[j] = score_diff;
    heap->indices[abs_i] = heap->indices[abs_j];
    heap->indices[abs_j] = k;
}

static void min_heapify(struct ges_heap *heap, int i)
{
    int l   = left(i);
    int r   = right(i);
    int min = i;
    if (l < heap->size && (heap->score_diffs[l] < heap->score_diffs[min]))
        min = l;
    if (r < heap->size && (heap->score_diffs[r] < heap->score_diffs[min]))
        min = r;
    if (i != min) {
        swap(heap, i, min);
        min_heapify(heap, min);
    }
}

void build_heap(struct ges_heap *heap)
{
    heap->size = heap->max_size;
    struct ges_operator * ops = heap->ops;
    for (int i = 0; i < heap->size; ++i) {
        heap->indices[i]     = i;
        heap->ops_ptrs[i]    = &ops[i];
        heap->score_diffs[i] = ops[i].score_diff;
    }
    for (int i = (heap->size - 1) / 2; i >= 0; --i)
        min_heapify(heap, i);
}

static void pop_heap(struct ges_heap *heap)
{
    heap->size      -= 1;
    struct ges_operator *op = heap->ops_ptrs[heap->size];
    heap->ops_ptrs[0] = op;
    heap->indices[op->y] = 0;
    heap->score_diffs[0] = op->score_diff;
    min_heapify(heap, 0);
}

struct ges_operator * peek_heap(struct ges_heap *heap)
{
    if (heap->size < 1)
        return NULL;
    return heap->ops_ptrs[0];
}

static void decrease_key(struct ges_heap *heap, int i, double score_diff)
{
    heap->score_diffs[i] = score_diff;
    while (i > 0 && (heap->score_diffs[i] < heap->score_diffs[parent(i)])) {
        swap(heap, parent(i), i);
        i = parent(i);
    }
}

void insert_heap(struct ges_heap *heap, struct ges_operator *op)
{
    heap->indices[op->y] = heap->size;
    heap->ops_ptrs[heap->size]    = op;
    heap->score_diffs[heap->size] = DBL_MAX;
    decrease_key(heap, heap->size, op->score_diff);
    heap->size += 1;
}

/* delete the entry of a node through the heap */
void remove_heap(struct ges_heap *heap, int node)
{
    decrease_key(heap, heap->indices[node], -DBL_MAX);
    pop_heap(heap);
}

void print_heap(struct ges_heap *heap)
{
    CAUSALITY_PRINT("heap contents\n");
    for (int i = 0; i < heap->size; ++i)
        CAUSALITY_PRINT("%i --> %i; %f; %i\n", heap->ops_ptrs[i]->xp,
            heap->ops_ptrs[i]->y, heap->score_diffs[i],
            heap->indices[heap->ops_ptrs[i]->y]);
    CAUSALITY_PRINT("---\n");
}
