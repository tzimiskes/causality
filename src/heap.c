#include <causality.h>
#include <float.h>
#include <heap.h>

heap * create_heap(int heap_size) {
  heap * hp = malloc(sizeof(heap));
  hp->max_size = heap_size;
  hp->size     = 0;
  hp->dscores   = malloc(heap_size * sizeof(double));
  hp->records  = malloc(heap_size * sizeof(void *));
  hp->indices  = malloc(heap_size * sizeof(int));
  return hp;
}

void free_heap(heap * hp) {
  free(hp->dscores);
  free(hp->records);
  free(hp->indices);
  free(hp);
}

static inline int parent(int i) {
  return (i - 1)/2;
}

static inline int left(int i) {
  return i * 2 + 1;
}

static inline int right(int i) {
  return i * 2 + 2;
}

static inline void swap(heap h, int i, int j) {
  double d = h.dscores[i];
  void * p = h.records[i];
  int k = h.indices[i];
  /* swap */
  h.dscores[i]  = h.dscores[j];
  h.records[i] = h.records[j];
  h.indices[i] = h.indices[j];
  h.dscores[j]  = d;
  h.records[j] = p;
  h.indices[j] = k;
}

/* todo make non recursive */
static inline void min_heapify(heap h, int i) {
  int l   = left(i);
  int r   = right(i);
  int min = i;
  if(l < h.size && (h.dscores[l] < h.dscores[min]))
    min = l;
  if(r < h.size && h.dscores[r] < h.dscores[min])
    min = r;
  if(i != min) {
    swap(h, i, min);
    min_heapify(h, min);
  }
}

/* probably not working */
void build_heap(heap * hp) {
  hp->size = hp->max_size;
  heap h   = *hp;
  for(int i = (h.size - 1)/2; i >= 0; --i)
    min_heapify(h, i);
}

static inline void pop_heap(heap *  hp) {
  hp->size--;
  heap h = *hp;
  h.dscores[0]  = h.dscores[h.size];
  h.records[0] = h.records[h.size];
  h.indices[0] = h.indices[h.size];
  min_heapify(h, 0);
}

void * extract_heap(heap * hp, double * ds) {
  if(hp->size < 1)
    return NULL;
  *ds = hp->dscores[0];
  void * p = hp->records[0];
  pop_heap(hp);
  return p;
}

static inline void decrease_key(heap heap, int i, double key) {
  heap.dscores[i] = key;
  while( i > 0 && heap.dscores[parent(i)] > heap.dscores[i]) {
    swap(heap, parent(i), i);
    i = parent(i);
  }
}

void insert_heap(heap * hp, double ds, void * p, int node) {
  heap h = *hp;
  h.dscores[h.size]  = DBL_MAX;
  h.records[h.size] = p;
  h.indices[node]   = h.size;
  decrease_key(h, h.size, ds);
  hp->size++;
}

/* delete the entry of a node through the heap */
void remove_heap(heap * hp, int node) {
  decrease_key(*hp, hp->indices[node], DBL_MIN);
  pop_heap(hp);
}
