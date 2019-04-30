/* Author: Alexander Rix
 * Date  : 3/8/2019
 * Description: r_causality_aggregate.c contains the R interface to the ...
 * casuality aggregate_graphs function. R must do additional processing on its
 * end to reorder the returned list, turn it into a dataframe, name the columns,
 * and drop empty columns.
 */

#include <R_causality/R_causality.h>
#include <aggregate/tree.h>
#include <causality.h>

#define X 0
#define Y 1

/*
 * tree_to_matrix takes the edge tree, which is the result of
 * causality_aggregate_graphs, and uses it to fill in output, which is a
 * n_nodes + 2 R list. The first two entries (X, Y) are character vectors of
 * length size(trees). The rest are numeric vectors of the same length.
 */
static void tree_to_matrix(SEXP output, const char **nodes, int n_rows, int x,
                               int *index, struct tree *root, double inv_sw)
{
    if (root == NULL)
        return;
    int y = tree_node(root);
    double *edges = tree_edges(root);
    SET_STRING_ELT(VECTOR_ELT(output, X), *index, mkChar(nodes[x]));
    SET_STRING_ELT(VECTOR_ELT(output, Y), *index, mkChar(nodes[y]));
    for (int i = 0; i < NUM_CAG_EDGETYPES; ++i)
        REAL(VECTOR_ELT(output, i + 2))[*index] = edges[i] * inv_sw;
    (*index)++;
    tree_to_matrix(output, nodes, n_rows, x, index, left_child(root), inv_sw);
    tree_to_matrix(output, nodes, n_rows, x, index, right_child(root), inv_sw);
}

/*
 * r_causality_aggregate_graphs takes a weights vector and list of
 * causality.graphs from R, converts each causality.graph into a cgraph, and
 * then aggregates all the graphs together, weighing each graph by its
 * proportion of the weight.
 */
SEXP r_causality_aggregate_graphs(SEXP graphs, SEXP graph_weights)
{
    SEXP graph_nodes = VECTOR_ELT(VECTOR_ELT(graphs, 0), NODES);
    int n_graphs = Rf_length(graphs);
    int n_nodes  = Rf_length(graph_nodes);
    struct cgraph **cgs = calloc(n_graphs, sizeof(struct cgraph *));
    /*
     * calculate the sum of the weights and invert it, and convert the
     * causality graphs to cgraphs
     */
    double *weights = REAL(graph_weights);
    double  inv_sw  = 0.0f;
    for (int i = 0; i < n_graphs; ++i) {
        inv_sw += weights[i];
        cgs[i]  = cgraph_from_causality_graph(VECTOR_ELT(graphs, i));
    }
    inv_sw = 1.0f / inv_sw;
    struct tree **trees = causality_aggregate_graphs(cgs, weights, n_graphs);
    /* calculate the total number of entries in the trees */
    int n_rows = 0;
    for (int i = 0; i < n_nodes; ++i)
        n_rows += tree_size(trees[i]);
    int n_cols = NUM_CAG_EDGETYPES + 2;
    /*
     * Allocate the R List that will be returned. The first two entries are
     * strings, and the rest are numeric
     */
    SEXP output = PROTECT(allocVector(VECSXP, n_cols));
    SET_VECTOR_ELT(output, X, allocVector(STRSXP, n_rows));
    SET_VECTOR_ELT(output, Y, allocVector(STRSXP, n_rows));
    for (int i = 2; i < n_cols; ++i) {
        SET_VECTOR_ELT(output, i, allocVector(REALSXP, n_rows));
        memset(REAL(VECTOR_ELT(output, i)), 0, n_rows * sizeof(double));
    }
    /* Fill in output. */
    const char **nodes = malloc(n_nodes * sizeof(const char *));
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = CHAR(STRING_ELT(graph_nodes, i));
    int index = 0;
    for (int i = 0; i < n_nodes; ++i) {
        tree_to_matrix(output, nodes, n_rows, i, &index, trees[i], inv_sw);
        free_tree(trees[i]);
    }
    for (int i = 0; i < n_graphs; ++i)
        free_cgraph(cgs[i]);
    free(cgs);
    free(trees);
    free(nodes);
    UNPROTECT(1);
    return output;
}
