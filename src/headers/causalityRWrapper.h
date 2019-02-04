#ifndef CAUSALITY_R_H
#define CAUSALITY_R_H

// R API
#include <R.h>
#include <Rinternals.h>

#include "cgraph.h"

#define NODES       0
#define ADJACENCIES 1
#define EDGES       2

SEXP causalitySort(SEXP Graph);
SEXP causalityMeek(SEXP Graph);
SEXP causalityPDX(SEXP Pdag);
SEXP causalityChickering(SEXP Graph);
SEXP causalityScoreGraph(SEXP Graph, SEXP Df, SEXP ScoreType, SEXP States,
                                     SEXP FloatingArgs, SEXP IntegerArgs);
SEXP causalityGES(SEXP Df, SEXP ScoreType, SEXP States, SEXP FloatingArgs,
                           SEXP IntegerArgs);

int * calculateEdgesPtr(SEXP Graph);
void calcluateEdgesFromCgraph(struct cgraph *cgPtr, SEXP Graph);
struct cgraph * cgraph_from_causality_graph(SEXP Graph);
SEXP causality_graph_from_cgraph(struct cgraph *cg, SEXP Nodes);
SEXP create_causality_graph(int n_nedges, int n_nodes, SEXP Nodes);
#endif
