#ifndef _HEAP_H
#define _HEAP_H

typedef struct heap {
  double * dscores;
  void **  records;
  int *    indices;
  int      max_size;
  int      size;
} heap;

heap * create_heap(int heap_size);
void free_heap(heap * hp);
void build_heap(heap * hp);
void * extract_heap(heap * hp, double * ds);
void insert_heap(heap * hp, double ds, void * p, int node);
void remove_heap(heap * hp, int node);
#endif
