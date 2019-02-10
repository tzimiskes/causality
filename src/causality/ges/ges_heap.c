#include <stdlib.h>
#include <float.h>

#include <causality.h>

#include <ges/ges_internal.h>

struct ges_heap * create_heap(int max_size, struct ges_operator *ext_ops)
{
    struct ges_heap *hp = calloc(1, sizeof(struct ges_heap));
    if (hp == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory for heap structure!\n");
        return NULL;
    }
    hp->max_size    = max_size;
    hp->size        = max_size;
    hp->ops         = ext_ops;
    hp->ops_ptrs    = malloc(max_size * sizeof(struct ges_operator *));
    if (hp->ops_ptrs == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory for op_ptrs!\n");
        free_heap(hp);
        return NULL;
    }
    hp->indices = malloc(max_size * sizeof(int));
    if (hp->indices == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory for indices!\n");
        free_heap(hp);
        return NULL;
    }
    hp->score_diffs = malloc(max_size * sizeof(double));
    if (hp->score_diffs == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory for score_diffs!\n");
        free_heap(hp);
        return NULL;
    }
    return hp;
}

void free_heap(struct ges_heap *hp)
{
    free(hp->ops_ptrs);
    free(hp->indices);
    free(hp->score_diffs);
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

static void swap(struct ges_heap *hp, int i, int j)
{
    struct ges_operator *op = hp->ops_ptrs[i];
    double score_diff       = hp->score_diffs[i];
    int ext_i               = hp->ops_ptrs[i] - hp->ops;
    int ext_j               = hp->ops_ptrs[j] - hp->ops;
    int k                   = hp->indices[ext_i];
    hp->ops_ptrs[i]    = hp->ops_ptrs[j];
    hp->score_diffs[i] = hp->score_diffs[j];
    hp->ops_ptrs[j]    = op;
    hp->score_diffs[j] = score_diff;
    hp->indices[ext_i] = hp->indices[ext_j];
    hp->indices[ext_j] = k;
}

static void min_heapify(struct ges_heap *hp, int i)
{
    int l   = left(i);
    int r   = right(i);
    int min = i;
    if (l < hp->size && (hp->score_diffs[l] < hp->score_diffs[min]))
        min = l;
    if (r < hp->size && (hp->score_diffs[r] < hp->score_diffs[min]))
        min = r;
    if (i != min) {
        swap(hp, i, min);
        min_heapify(hp, min);
    }
}

void build_heap(struct ges_heap *hp)
{
    hp->size = hp->max_size;
    struct ges_operator * ops = hp->ops;
    for (int i = 0; i < hp->size; ++i) {
        hp->indices[i]     = i;
        hp->ops_ptrs[i]    = &ops[i];
        hp->score_diffs[i] = ops[i].score_diff;
    }
    for (int i = (hp->size - 1) / 2; i >= 0; --i)
        min_heapify(hp, i);
}

static void pop_heap(struct ges_heap *hp)
{
    hp->size      -= 1;
    struct ges_operator *op = hp->ops_ptrs[hp->size];
    hp->ops_ptrs[0]         = op;
    hp->indices[op->y]      = 0;
    hp->score_diffs[0]      = op->score_diff;
    min_heapify(hp, 0);
}

struct ges_operator * peek_heap(struct ges_heap *hp)
{
    if (hp->size < 1)
        return NULL;
    return hp->ops_ptrs[0];
}

static void decrease_key(struct ges_heap *hp, int i, double score_diff)
{
    hp->score_diffs[i] = score_diff;
    while (i > 0 && (hp->score_diffs[i] < hp->score_diffs[parent(i)])) {
        swap(hp, parent(i), i);
        i = parent(i);
    }
}

void insert_heap(struct ges_heap *hp, struct ges_operator *op)
{
    hp->indices[op->y] = hp->size;
    hp->ops_ptrs[hp->size]    = op;
    hp->score_diffs[hp->size] = DBL_MAX;
    decrease_key(hp, hp->size, op->score_diff);
    hp->size += 1;
}

/* delete the entry of a node through the heap */
void remove_heap(struct ges_heap *hp, int node)
{
    decrease_key(hp, hp->indices[node], -DBL_MAX);
    pop_heap(hp);
}

void print_heap(struct ges_heap *hp)
{
    CAUSALITY_PRINT("heap contents\n");
    for (int i = 0; i < hp->size; ++i)
        CAUSALITY_PRINT("%i --> %i; %f; %i\n", hp->ops_ptrs[i]->xp, hp->ops_ptrs[i]->y,
                                      hp->score_diffs[i],
                                      hp->indices[hp->ops_ptrs[i]->y]);
    CAUSALITY_PRINT("end\n");
}
