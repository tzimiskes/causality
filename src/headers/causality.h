#include <stdio.h>
#include <stdarg.h>

#include "cgraph.h"
#include "ges.h"

#ifndef CAUSALITY_H
#define CAUSALITY_H

#ifdef CAUSALITY_R
#include <R.h>
#define CAUSALITY_PRINT(...) Rprintf(__VA_ARGS__)
#define CAUSALITY_ERROR(...) Rprintf(__VA_ARGS__)
#else
#define CAUSALITY_PRINT(...) printf(__VA_ARGS__)
#define CAUSALITY_ERROR(...) fprintf(stderr, __VA_ARGS__)
#endif


#define DIRECTED      1 /* -->               */
#define UNDIRECTED    2 /* ---               */
#define PLUSPLUSARROW 3 /* ++> aka --> dd nl */
#define SQUIGGLEARROW 4 /* ~~> aka --> pd nl */
#define CIRCLEARROW   5 /* o->               */
#define CIRCLECIRCLE  6 /* o-o               */
#define BIDIRECTED    7 /* <->               */

#define NUM_NL_EDGETYPES  2
#define NUM_LAT_EDGETYPES 7
#define NUM_EDGES_STORED  11

#define IS_DIRECTED(edge) ((edge) == DIRECTED || (edge) == CIRCLEARROW || \
                           (edge) == SQUIGGLEARROW || (edge) == PLUSPLUSARROW)


/* Search algorithms */
double ccf_ges(struct ges_score score, struct cgraph *cg);
/* Graph manipulations */
int           * ccf_sort(struct cgraph *cg);
struct cgraph * ccf_pdx(struct cgraph *cg);
void            ccf_chickering(struct cgraph *cg);
void            ccf_meek(struct cgraph *cg);
/* misc functions */
double ccf_score_graph(struct cgraph *cg, struct dataframe df, score_func score,
                                          struct score_args args);
void ccf_fr_layout(double *positions, int n_nodes, int *edges, int n_edges,
                                      double width, double height,
                                      int iterations);
#endif
