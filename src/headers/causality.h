#ifndef _CAUSALITY_
#define _CAUSALITY_

// R API
#include <R.h>
#include <Rinternals.h>

// C LIBS
#include<stdlib.h>
#include<string.h>
#include<stdio.h>
#include<math.h>

#define NODES 0
#define ADJACENCIES 1
#define EDGES 2
#define EDGES_NCOL 3


int* calculate_edges_ptr(SEXP Graph);
SEXP cf_topological_sort(SEXP dag);
#endif

