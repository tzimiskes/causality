#ifndef _CAUSALITY_
#define _CAUSALITY_

// R API
#include <R.h>
#include <Rinternals.h>

// C LIBS
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <cgraph.h>

#define NODES 0
#define ADJACENCIES 1
#define EDGES 2
#define EDGES_NCOL 3

int * ccf_sort(cgraph cg);
void ccf_chickering(cgraph cg);
int * calculate_edges_ptr(SEXP Graph);
SEXP calculate_edges_from_ptr(int * edges_ptr, SEXP Graph);
int * ccf_topological_sort(SEXP Graph);
int is_directed(int edge);
int * calculate_edges_ptr(SEXP Graph);
SEXP cf_topological_sort(SEXP dag);

#endif
