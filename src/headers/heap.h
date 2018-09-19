#ifndef _HEAP_H
#define _HEAP_H

struct heap {
    double      *keys;
    void       **data;
    const void  *ext_data_loc;
    int          ext_data_size;
    int         *indices;
    int          max_size;
    int          size;
};

struct heap * create_heap(const int max_size, const void *ext_data_loc,
                                         const int ext_data_size);
void          free_heap(struct heap *hp);
void          build_heap(struct heap *hp);
void        * extract_heap(struct heap *hp, double *ds);
void          insert_heap(struct heap *hp, double ds, void *p);
void          remove_heap(struct heap *hp, int node);
#endif
