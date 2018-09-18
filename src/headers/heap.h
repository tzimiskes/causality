#ifndef _HEAP_H
#define _HEAP_H

struct heap {
    double *dscores;
    void  **records;
    int    *indices;
    int     max_size;
    int     size;
};

struct heap *create_heap(int heap_size);
void         free_heap(struct heap *hp);
void         build_heap(struct heap *hp);
void        *extract_heap(struct heap *hp, double *ds);
void         insert_heap(struct heap *hp, double ds, void *p, int node);
void         remove_heap(struct heap *hp, int node);
#endif
