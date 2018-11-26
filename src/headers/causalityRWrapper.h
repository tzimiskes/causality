#ifndef CAUSALITY_R_WRAPPER_H
#define CAUSALITY_R_WRAPPER_H

// R API
#include <R.h>
#include <Rinternals.h>

#include <cgraph.h>

#define NODES       0
#define ADJACENCIES 1
#define EDGES       2

int * calculateEdgesPtr(SEXP Graph);
void calcluateEdgesFromCgraph(struct cgraph *cgPtr, SEXP Graph);
SEXP causalityGraphFromCgraph(struct cgraph *cg, SEXP Nodes);
#endif
