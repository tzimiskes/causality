#ifndef CAUSALITY_H
#define CAUSALITY_H

#include <stdio.h>
#include <stdarg.h>

#include <ges/ges.h>
#include <scores/scores.h>

/* When compiling causality so it links to R, we want to print via the RAPI */
#ifdef CAUSALITY_R
#include <R.h>
#define CAUSALITY_PRINT(...) Rprintf(__VA_ARGS__)
#define CAUSALITY_ERROR(...) REprintf(__VA_ARGS__)
#else
#define CAUSALITY_PRINT(...) printf(__VA_ARGS__)
#define CAUSALITY_ERROR(...) fprintf(stderr, __VA_ARGS__)
#endif

#define DIRECTED          0  /* -->               */
#define UNDIRECTED        1  /* ---               */
#define PLUSPLUSARROW     2  /* ++> aka --> dd nl */
#define SQUIGGLEARROW     3  /* ~~> aka --> pd nl */
#define CIRCLEARROW       4  /* o->               */
#define CIRCLECIRCLE      5  /* o-o               */
#define BIDIRECTED        6  /* <->               */
#define DIRECTED_REV      7  /* <--, only used in causality_aggregate_graphs */
#define PLUSPLUSARROW_REV 8  /* <++, " " */
#define SQUIGGLEARROW_REV 9 /* <~~, " " */
#define CIRCLEARROW_REV   10 /* <-o, " " */

#define NUM_NL_EDGETYPES  2
#define NUM_LAT_EDGETYPES 7
#define NUM_CAG_EDGETYPES 11 /* causality_aggregate_graphs, or BSG rank */

#define IS_DIRECTED(edge) ((edge) == DIRECTED || (edge) == CIRCLEARROW || \
                           (edge) == SQUIGGLEARROW || (edge) == PLUSPLUSARROW)

/* Graph manipulations */
int           * causality_sort(struct cgraph *cg);
struct cgraph * causality_pdx(struct cgraph *cg);
void            causality_chickering(struct cgraph *cg);
void            causality_meek(struct cgraph *cg);
/* misc functions */
struct tree ** causality_aggregate_graphs(struct cgraph **cgs, double *weights,
                                              int n_graphs);
double causality_score_graph(struct cgraph *cg, struct dataframe *df, score_func
                                 score, struct score_args args);
void ccf_fr_layout(double *positions, int n_nodes, int *edges, int n_edges,
                       double width, double height, int iterations);
#endif
