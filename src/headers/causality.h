#ifndef _CAUSALITY_
#define _CAUSALITY_

// R API
#include <R.h>
#include <Rinternals.h>

/* Define some memory allocation macros so to increase portability between the
 * R version of this library and the potential Python version */
#ifdef R_R_H
#define CALLOC(n_elem, type) Calloc(n_elem, type)
#define FREE(ptr) Free(ptr)
#else
#define CALLOC(n_elem, size) calloc(n_elem, sizeof(type))
#define FREE(ptr) free(ptr)
#endif

// C LIBS
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <cgraph.h>

#define NODES       0
#define ADJACENCIES 1
#define EDGES       2

int * calculateEdgesPtr(SEXP Graph);
void calcluateEdgesFromCgraph(struct cgraph *cgPtr, SEXP Graph);
int   is_directed(int edge);
#endif
