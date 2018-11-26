#include <stdlib.h>
#include <float.h>

#include "headers/causality.h"
#include "headers/heap.h"

struct heap * create_heap(const int max_size, const void *ext_data_loc,
                                              const int ext_data_size)
{
    struct heap *hp = calloc(1, sizeof(struct heap));
    hp->max_size      = max_size;
    hp->size          = 0;
    hp->ext_data_size = ext_data_size;
    hp->ext_data_loc  = ext_data_loc;
    hp->keys          = malloc(max_size * sizeof(double));
    hp->data          = malloc(max_size * sizeof(void *));
    hp->indices       = malloc(max_size * sizeof(int));
    if (!hp->keys || !hp->data || !hp->data)
        CAUSALITY_ERROR("Failed to allocate memory for heap!\n");
    return hp;
}

void free_heap(struct heap *hp)
{
    free(hp->keys);
    free(hp->data);
    free(hp->indices);
    free(hp);
}

static inline int parent(int i)
{
    return (i - 1)/2;
}

static inline int left(int i)
{
    return i * 2 + 1;
}

static inline int right(int i)
{
    return i * 2 + 2;
}

static inline void swap(struct heap *hp, int i, int j)
{
    double d = hp->keys[i];
    void  *p = hp->data[i];
    hp->keys[i]    = hp->keys[j];
    hp->data[i]    = hp->data[j];
    hp->keys[j]    = d;
    hp->data[j]    = p;

    int ext_i = (char *) hp->data[i] - (char *) hp->ext_data_loc;
    ext_i /= hp->ext_data_size;
    int ext_j = (char *) hp->data[j] - (char *) hp->ext_data_loc;
    ext_j /= hp->ext_data_size;
    int t = hp->indices[ext_i];
    hp->indices[ext_i] = hp->indices[ext_j];
    hp->indices[ext_j] = t;
}

/* todo make non recursive */
static inline void min_heapify(struct heap *hp, int i)
{
    int l   = left(i);
    int r   = right(i);
    int min = i;
    if (l < hp->size && (hp->keys[l] < hp->keys[min]))
        min = l;
    if (r < hp->size && hp->keys[r] < hp->keys[min])
        min = r;
    if (i != min) {
        swap(hp, i, min);
        min_heapify(hp, min);
    }
}

void build_heap(struct heap *hp)
{
    hp->size = hp->max_size;
    for (int i = (hp->size - 1)/2; i >= 0; --i)
        min_heapify(hp, i);
}

static inline void pop_heap(struct heap *hp)
{
    hp->keys[0] = hp->keys[hp->size - 1];
    hp->data[0] = hp->data[hp->size - 1];

    int ext_i = (char *) hp->data[0] - (char *) hp->ext_data_loc;
    ext_i /= hp->ext_data_size;
    hp->indices[ext_i] = 0;
    hp->size      -= 1;
    min_heapify(hp, 0);
}

void *peek_heap(struct heap *hp)
{
    if (hp->size < 1)
        return NULL;
    return hp->data[0];
}

void *extract_heap(struct heap *hp, double *ds)
{
    void *p;
    if (hp->size < 1)
        return NULL;

    *ds = hp->keys[0];
    p   = hp->data[0];
    pop_heap(hp);
    return p;
}

void decrease_key(struct heap *hp, int i, double key)
{
    hp->keys[i] = key;
    while (i > 0 && (hp->keys[i] < hp->keys[parent(i)])) {
        swap(hp, parent(i), i);
        i = parent(i);
    }
}

void insert_heap(struct heap *hp, double ds, void *p)
{
    hp->keys[hp->size] = DBL_MAX;
    hp->data[hp->size] = p;
    int ext_i = (char *) p - (char *) hp->ext_data_loc;
    ext_i /= hp->ext_data_size;
    hp->indices[ext_i] = hp->size;
    decrease_key(hp, hp->size, ds);
    hp->size += 1;
}

/* delete the entry of a node through the heap */
void remove_heap(struct heap *hp, int node)
{
    decrease_key(hp, hp->indices[node], -DBL_MAX);
    pop_heap(hp);
}
