#include <stdint.h>
#ifndef GES_UTILS_H
#define GES_UTILS_H

struct gesrec {
    int    x;
    int    y;
    union {
        uint64_t t;
        uint64_t h;
    };
    int   *naxy;
    int   *set;
    int   *parents;
    int    parents_size;
    int    naxy_size;
    int    set_size;
    int    type;
    double score_diff;
}; /*  64 bytes */

void   free_gesrec(struct gesrec g);
int    forms_clique(struct cgraph *cg, struct gesrec g);
int    cycle_created(struct cgraph *cg, struct gesrec g, int *mem);
void   partition_neighbors(struct cgraph *cg, struct gesrec *g);
void   calculate_naxy(struct cgraph *cg, struct gesrec *g);
int  * deterimine_nodes_to_recalc(struct cgraph *cpy, struct cgraph *cg,
                                                      struct gesrec g,
                                                      int *visited,
                                                      int n_visited,
                                                      int *n_nodes);
#endif
