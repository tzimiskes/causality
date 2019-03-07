#ifndef CAUSALITY_R_H
#define CAUSALITY_R_H

// R API
#include <R.h>
#include <Rinternals.h>

#include <cgraph/cgraph.h>

#define NODES       0
#define ADJACENCIES 1
#define EDGES       2

SEXP r_causality_sort(SEXP graph);
SEXP r_causality_meek(SEXP graph);
SEXP r_causality_pdx(SEXP pdag);
SEXP r_causality_chickering(SEXP graph);
SEXP r_causality_score_graph(SEXP Graph, SEXP Df, SEXP ScoreType, SEXP States,
                                     SEXP FloatingArgs, SEXP IntegerArgs);
SEXP r_causality_ges(SEXP Df, SEXP ScoreType, SEXP States, SEXP FloatingArgs,
                           SEXP IntegerArgs);
SEXP r_causality_aggregate_graphs(SEXP graphs, SEXP graph_weights);
int          edge_to_int(const char *edge);
const char * edge_to_char(int edge);
int          node_to_int(const char *node, const char **nodes);

int * calculate_edges_ptr(SEXP Graph);
void calculate_edges_from_cgraph(struct cgraph *cg, SEXP graph);
struct cgraph * cgraph_from_causality_graph(SEXP Graph);
SEXP causality_graph_from_cgraph(struct cgraph *cg, SEXP Nodes);
SEXP create_causality_graph(int n_nedges, int n_nodes, SEXP Nodes);
#endif
