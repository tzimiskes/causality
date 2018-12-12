#include <stdint.h>

#include "scores.h"
#include "dataframe.h"

#ifndef GES_H
#define GES_H

#define INSERTION 1
#define DELETION  0

struct ges_score {
    ges_score_func    score;
    struct dataframe  df;
    struct score_args args;
    double           *fmem;
    int              *imem;
};

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
    int    n_parents;
    int    naxy_size;
    int    set_size;
    int    type;
    double score_diff;
}; /*  64 bytes */

void free_ges_op(struct ges_op op);
void free_ges_score(struct ges_score sc);
int  valid_fes_clique(struct cgraph *cg, struct ges_op op);
int  valid_bes_clique(struct cgraph *cg, struct ges_op op);
int  cycle_created(struct cgraph *cg, struct ges_op op, int *mem);
void partition_neighbors(struct cgraph *cg, struct ges_op *op);
void calculate_naxy(struct cgraph *cg, struct ges_op *op);
void calculate_parents(struct cgraph *cg, struct ges_op *op);
void reorient(struct cgraph *cg, struct ges_op op, int *visited, int *n);
void reorient_and_determine_operators_to_update(struct cgraph *cpy,
                                                struct cgraph *cg,
                                                struct ges_op op,
                                                int *nodes, int *n);
#endif
