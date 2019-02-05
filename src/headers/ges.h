#include <stdint.h>

#include "cgraph.h"
#include "scores.h"
#include "dataframe.h"

#ifndef GES_H
#define GES_H

struct ges_score_mem {
    double *cov_xy;
    double *cov_xx; /* m by m matrix */
    double *cov_xpx;
    int    *lbls;
    int     m;
}; /* 36 bytes */

typedef double (*ges_score_func)(struct dataframe df, int x, int y, int *ypar,
                                                      int npar,
                                                      struct score_args args,
                                                      struct ges_score_mem gsm);

double ges_bdeu_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                             struct score_args args,
                                             struct ges_score_mem gsm);

double ges_bic_score(struct dataframe df, int xp, int y, int *x, int nx,
                                          struct score_args args,
                                          struct ges_score_mem gsm);

struct ges_score {
    ges_score_func       gsf;
    struct ges_score_mem gsm;
    struct dataframe     df;
    struct score_args    args;
};

struct ges_operator {
    int    xp;
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
    double score_diff;
}; /* 64 bytes */

/*
struct ges_operator {
    int    xp;
    int    y;
    union {
        uint64_t t;
        uint64_t h;
    } set;
    uint64_t naxpy;
    int   *spouses;
    int   *parents;
    int    nparents;
    int    nspouses;
    int    n_naxpy;
    int    nset;
    double score_diff;
};
*/

/* memory utility functions */
void free_ges_score_mem(struct ges_score_mem gsm);
/* functions that deterimine whether or not a operators is legal */
int  valid_fes_clique(struct cgraph *cg, struct ges_operator *op);
int  valid_bes_clique(struct cgraph *cg, struct ges_operator *op);
int  cycle_created(struct cgraph *cg, struct ges_operator *op, int *mem);
/* misc utility functions */
void partition_neighbors(struct cgraph *cg, struct ges_operator *op);
void calculate_naxy(struct cgraph *cg, struct ges_operator *op);
void calculate_parents(struct cgraph *cg, struct ges_operator *op);
/* reorient cgraph after an operator has been applied */
void reorient_fes(struct cgraph *cg, struct ges_operator op, int *visited);
void reorient_bes(struct cgraph *cg, struct ges_operator op, int *visited);
int determine_deletion_operators_to_update(int *nodes, struct cgraph *cpy,
                                                       struct cgraph *cg,
                                                       struct ges_operator *op,
                                                       int *visited);
int determine_insertion_operators_to_update(int *nodes, struct cgraph *cpy,
                                                        struct cgraph *cg,
                                                        struct ges_operator *op,
                                                        int *visited);
/* functions that optimize ges_bic_score score */
void ges_bic_optimization1(struct cgraph *cg, int y, int n,
                                              struct ges_score *gs);
void ges_bic_optimization2(int xp, struct ges_score *gs);
#endif
