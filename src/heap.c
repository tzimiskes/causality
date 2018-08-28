#include <causality.h>


struct heap {
  double * dscore;
  int    * parent;
  int    * child;
  int      max_size;
  int      cur_size;
};

struct heap create_heap(int heap_size) {
  struct heap heap;
  heap.max_size  = heap_size;
  heap.cur_size  = 0;
  heap.dscore = malloc(heap_size * sizeof(double));
  heap.parent = malloc(heap_size * sizeof(int));
  heap.child  = malloc(heap_size * sizeof(int));
  return heap;
}

void free_heap(struct heap heap) {
  free(heap.dscore);
  free(heap.parent);
  free(heap.child);
}

inline static int parent(int i) {
  return (i - 1)/2;
}

inline static int left(int i) {
  return i*2 + 1;
}

inline static int right(int i) {
  return i*2 + 2;
}

inline static void swap(struct heap heap, int i , int min) {
  double d = heap.dscore[i];
  int    p = heap.parent[i];
  int    c = heap.child[i];
  /* swap */
  heap.dscore[i]   = heap.dscore[min];
  heap.parent[i]   = heap.parent[min];
  heap.parent[i]   = heap.parent[min];
  heap.dscore[min] = d;
  heap.parent[min] = p;
  heap.parent[min] = c;
}

void min_heapify(struct heap heap, int i) {
  int l   = left(i);
  int r   = right(i);
  int min = i;
  if(l < heap.cur_size && (heap.dscore[i] < heap.dscore[i]))
    min = l;
  if(r < heap.cur_size && heap.dscore[r] < heap.dscore[min])
    min = r;
  if(i != min) {
    swap(heap, i, min);
    min_heapify(heap, min);
  }
}
