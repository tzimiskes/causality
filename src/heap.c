#include <causality.h>
#include <float.h>
#include <heap.h>

heap *create_heap(int heap_size)
{
    heap *hp = malloc(sizeof(heap));
    hp->max_size = heap_size;
    hp->size     = 0;
    hp->dscores  = malloc(heap_size * sizeof(double));
    hp->records  = malloc(heap_size * sizeof(void *));
    hp->indices  = malloc(heap_size * sizeof(int));
    return hp;
}

void free_heap(heap * hp)
{
    free(hp->dscores);
    free(hp->records);
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

static inline void swap(heap *hp, int i, int j)
{
    double d = hp->dscores[i];
    void  *p = hp->records[i];
    int    k = hp->indices[i];
    /* swap */
    hp->dscores[i] = hp->dscores[j];
    hp->records[i] = hp->records[j];
    hp->indices[i] = hp->indices[j];
    hp->dscores[j] = d;
    hp->records[j] = p;
    hp->indices[j] = k;
}

/* todo make non recursive */
static inline void min_heapify(heap *hp, int i)
{
    int l   = left(i);
    int r   = right(i);
    int min = i;
    if(l < hp->size && (hp->dscores[l] < hp->dscores[min]))
        min = l;
    if(r < hp->size && hp->dscores[r] < hp->dscores[min])
        min = r;
    if(i != min) {
        swap(hp, i, min);
        min_heapify(hp, min);
    }
}

void build_heap(heap *hp)
{
    hp->size = hp->max_size;
    for(int i = (hp->size - 1)/2; i >= 0; --i)
        min_heapify(hp, i);
}

static inline void pop_heap(heap *hp)
{
    hp->dscores[0] = hp->dscores[hp->size -1];
    hp->records[0] = hp->records[hp->size -1];
    hp->indices[0] = hp->indices[hp->size -1];
    hp->size      -= 1;
    min_heapify(hp, 0);
}

void *extract_heap(heap *hp, double *ds)
{
    void *p;
    if(hp->size < 1)
        return NULL;

    *ds = hp->dscores[0];
    p   = hp->records[0];
    pop_heap(hp);
    return p;
}

void decrease_key(heap *hp, int i, double key)
{
    hp->dscores[i] = key;
    while(i > 0 && (hp->dscores[parent(i)] > hp->dscores[i])) {
        swap(hp, parent(i), i);
        i = parent(i);
    }
}

void insert_heap(heap *hp, double ds, void *p, int node)
{
    hp->dscores[hp->size] = DBL_MAX;
    hp->records[hp->size] = p;
    hp->indices[node]     = hp->size;
    decrease_key(hp, hp->size, ds);
    hp->size += 1;
}

/* delete the entry of a node through the heap */
void remove_heap(heap * hp, int node)
{
    decrease_key(hp, hp->indices[node], DBL_MIN);
    pop_heap(hp);
}
