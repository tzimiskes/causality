#include <R_causality/R_causality.h>
#include <aggregate/tree.h>
#include <causality.h>

#define NODE1 0
#define NODE2 1


void convert_tree_to_matrix(SEXP output, const char **nodes, int n_rows, int x,
                            int *index, struct tree *root, double inv_sw)
{
    if (root == NULL)
        return;
    int     y      = tree_node(root);
    double *edges  = tree_edges(root);
    SET_STRING_ELT(VECTOR_ELT(output, NODE1), *index, mkChar(nodes[x]));
    SET_STRING_ELT(VECTOR_ELT(output, NODE2), *index, mkChar(nodes[y]));
    for (int i = 0; i < NUM_CAG_EDGETYPES; ++i)
        REAL(VECTOR_ELT(output, i + 2))[*index] = edges[i] * inv_sw;
    (*index)++;
    convert_tree_to_matrix(output, nodes, n_rows, x, index, left_child(root),
                           inv_sw);
    convert_tree_to_matrix(output, nodes, n_rows, x, index, right_child(root),
                           inv_sw);
}

SEXP r_causality_aggregate_graphs(SEXP graphs, SEXP graph_weights)
{
    int          n_graphs    = Rf_length(graphs);
    SEXP         graph_nodes = VECTOR_ELT(VECTOR_ELT(graphs, 0), NODES);
    double *weights = REAL(graph_weights);
    int          n_nodes     = Rf_length(graph_nodes);
    const char **nodes       = malloc(n_nodes * sizeof (const char *));
    for (int i = 0; i < n_nodes; ++i)
        nodes[i] = CHAR(STRING_ELT(graph_nodes, i));
    struct cgraph **cgs = malloc(n_graphs * sizeof(struct cgraph *));
    double inv_sw = 0.0f;
    for (int i = 0; i < n_graphs; ++i) {
        inv_sw += weights[i];
        cgs[i] = cgraph_from_causality_graph(VECTOR_ELT(graphs, i));
    }
    inv_sw = 1.0f /  inv_sw;
    struct tree **trees = causality_aggregate_graphs(cgs, weights, n_graphs);
    int n_rows = 0;
    for (int i = 0; i < n_nodes; ++i)
        n_rows += tree_size(trees[i]);
    int n_cols = NUM_CAG_EDGETYPES + 2;
    SEXP output = PROTECT(allocVector(VECSXP, n_cols));
    SET_VECTOR_ELT(output, NODE1, allocVector(STRSXP, n_rows));
    SET_VECTOR_ELT(output, NODE2, allocVector(STRSXP, n_rows));
    for (int i = 2; i < n_cols; ++i) {
        SET_VECTOR_ELT(output, i, allocVector(REALSXP, n_rows));
        memset(REAL(VECTOR_ELT(output, i)), 0, n_rows * sizeof(double));
    }
    int index = 0;
    for (int i = 0; i < n_nodes; ++i) {
        convert_tree_to_matrix(output, nodes, n_rows, i, &index, trees[i], inv_sw);
        free_tree(trees[i]);
    }
    free(trees);
    free(nodes);
    UNPROTECT(1);
    return output;
}
