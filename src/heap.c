#include <stdlib.h>
#include <float.h>

typedef struct heap {
  double * dscore;
  void **  records;
  int      max_size;
  int      size;
} heap;

heap * create_heap(int heap_size) {
  heap * hp = malloc(sizeof(heap));
  hp->max_size = heap_size;
  hp->size     = 0;
  hp->dscore   = malloc(heap_size * sizeof(double));
  hp->records  = malloc(heap_size * sizeof(void *));
  return hp;
}

void free_heap(heap * hp) {
  free(hp->dscore);
  free(hp->records);
  free(hp);
}

static inline int parent(int i) {
  return (i - 1)/2;
}

static inline int left(int i) {
  return i*2 + 1;
}

static inline int right(int i) {
  return i*2 + 2;
}

static inline void swap(heap h, int i, int j) {
  double d = h.dscore[i];
  void * p = h.records[i];
  /* swap */
  h.dscore[i]  = h.dscore[j];
  h.records[i] = h.records[j];
  h.dscore[j]  = d;
  h.records[j] = p;
}

/* todo make non recursive */
static inline void min_heapify(heap h, int i) {
  int l   = left(i);
  int r   = right(i);
  int min = i;
  if(l < h.size && (h.dscore[i] < h.dscore[i]))
    min = l;
  if(r < h.size && h.dscore[r] < h.dscore[min])
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
  h.dscore[0]  = h.dscore[h.size];
  h.records[0] = h.records[h.size];
  min_heapify(h, 0);
}

void * extract_heap(heap * hp, double * ds) {
  if(hp->size < 1)
    return NULL;
  *ds = hp->dscore[0];
  void * p = hp->records[0];
  pop_heap(hp);
  return p;
}

static inline void decrease_key(heap heap, int i, double key) {
  heap.dscore[i] = key;
  while( i > 0 && heap.dscore[parent(i)] > heap.dscore[i]) {
    swap(heap, parent(i), i);
    i = parent(i);
  }
}

void insert_heap(heap heap, double dscore, void * p) {
  heap.size++;
  heap.dscore[heap.size]  = DBL_MAX;
  heap.records[heap.size] = p;
  decrease_key(heap, heap.size - 1, dscore);
}

void delete_heap(heap * hp, int i) {
  decrease_key(*hp, i, DBL_MIN);
  pop_heap(hp);
}
