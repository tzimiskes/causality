#include <stdint.h>
#ifndef GES_UTILS_H
#define GES_UTILS_H

#define INSERTION 1
#define DELETION  0

struct ges_op {
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

void   free_ges_op(struct ges_op op);
int    forms_clique(struct cgraph *cg, struct ges_op op);
int    cycle_created(struct cgraph *cg, struct ges_op op, int *mem);
void   partition_neighbors(struct cgraph *cg, struct ges_op *op);
void   calculate_naxy(struct cgraph *cg, struct ges_op *op);
void   calculate_parents(struct cgraph *cg, struct ges_op *op);
int  * deterimine_nodes_to_recalc(struct cgraph *cpy, struct cgraph *cg,
                                                      struct ges_op op,
                                                      int *visited,
                                                      int n_visited,
                                                      int *n_nodes);
#endif
