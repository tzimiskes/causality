#ifndef GES_INTERNAL_H
#define GES_INTERNAL_H

#include <stdint.h>
#include <cgraph/cgraph.h>
#include <scores/scores.h>
#include <dataframe.h>
#include <ges/ges.h>

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

struct ges_heap {
    int     max_size;
    int     size;
    int    *indices;
    double *score_diffs;
    struct ges_operator  *ops;
    struct ges_operator **ops_ptrs;
};
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

static inline int IS_TAIL_NODE(uint64_t t, int node)
{
    /* I believe this (i) is required for reasons of integer promotion */
    uint64_t i = 0x1 << node;
    return (t & i) == i;
}

static inline int IS_HEAD_NODE(uint64_t h, int node)
{
    uint64_t i = 0x1 << node;
    return (h & i) == i;
}

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
                                               struct ges_operator *op, int
                                               *visited);
int determine_insertion_operators_to_update(int *nodes, struct cgraph *cpy,
                                                 struct cgraph *cg,
                                                 struct ges_operator *op, int
                                                 *visited);
/* functions that optimize ges_bic_score score */
void ges_bic_optimization1(struct cgraph *cg, int y, int n, struct ges_score *gs);
void ges_bic_optimization2(int xp, struct ges_score *gs);
/* ges_heap functions */
void free_heap(struct ges_heap *hp);
void build_heap(struct ges_heap *hp);
void insert_heap(struct ges_heap *hp, struct ges_operator *op);
void remove_heap(struct ges_heap *hp, int node);
struct ges_heap * create_heap(int max_size, struct ges_operator *ext_ops);
struct ges_operator * peek_heap(struct ges_heap *hp);
void print_heap(struct ges_heap *hp);
#endif /* ges_internal.h */
