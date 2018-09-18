#include <causality.h>
#include <dataframe.h>
#include <scores.h>

double score_graph(struct cgraph *cg, struct dataframe df, double *fargs,
                                      int *iargs, score_func score);

SEXP ccf_score_graph_wrapper(SEXP Graph, SEXP Df, SEXP ScoreType, SEXP States,
                                         SEXP FloatingArgs, SEXP IntegerArgs)
{
    int    *edges   = calculate_edges_ptr(Graph);
    int     n_nodes = length(VECTOR_ELT(Graph, NODES));
    int     n_edges = nrows(VECTOR_ELT(Graph, EDGES));
    struct cgraph *cg      = create_cgraph(n_nodes);
    fill_in_cgraph(cg, n_edges, edges); free(edges);
    /*
     * Determine the integer arguments and floating point arguments for the
     * score function.
     */
    int *iargs = NULL;
    if (!isNull(IntegerArgs))
        iargs = INTEGER(IntegerArgs);
    double *fargs = NULL;
    if (!isNull(FloatingArgs))
        fargs = REAL(FloatingArgs);
    /*
     * states stores the number of states (in the sense of degrees of freedom)
     * of each variable in the data frame with the caveat that for continuous
     * variables, the number of states is considered to be to be 0 instead of 1.
     * This allows me to use the actual value in each entry of dims to determine
     * the type (real or discrete/integer)of the variable in the data frame.
     * Instead, we store the columns as void pointers in df. This helps divorce
     * C and R so it is easier to port this package to python, julia, etc.
     */
    struct dataframe df;
    df.nvar  = length(Df);
    df.nobs  = length(VECTOR_ELT(Df, 0));
    df.states = INTEGER(States);
    df.df     = malloc(df.nvar * sizeof(void *));
    /* populate df with the pointers to the columns of the R dataframe */
    for (int i = 0; i < df.nvar; ++i) {
        if (df.states[i] != 0)
            df.df[i] = INTEGER(VECTOR_ELT(Df, i));
        else
            df.df[i] = REAL(VECTOR_ELT(Df, i));
    }

    score_func score;
    if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
        score = bic_score;
    else if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BDEU_SCORE))
        score = bdeu_score;
    else
        error("nope\n");
    double graph_score = score_graph(cg, df, fargs, iargs, score);
    free(df.df);
    free_cgraph(cg);
    return(ScalarReal(graph_score));
}

/*
 * score_graph calculates the (BIC-like) score of a graph using a causality
 * graph, the data associated with the graph, and score function (with
 * associated floating point args and integer args). Roughly, we use cg
 * to construct the model x --> y, where x:= Parents(y), and then score
 * the model given the data.
 */
double score_graph(struct cgraph *cg, struct dataframe df, double *fargs,
                                      int *iargs, score_func score)
{
    double   graph_score = 0.0f;
    int      n_nodes     = cg->n_nodes;
    struct ill **parents     = cg->parents;
    for (int i = 0; i < n_nodes; ++i) {
        struct ill *p = parents[i];
        if (p) {
            int npar = ill_size(p);
            int xy[npar + 1];
            for (int j = 0; j < npar; ++j) {
                xy[j] = p->key;
                p     = p->next;
            }
            xy[npar + 1] = i; /* set y to i */
            graph_score += score(df, xy, npar, fargs, iargs);
        }
    }
    return graph_score;
}

/*
 * score diff calculates the difference in BIC scores between two configurations
 * new and old
 */
double score_diff(struct dataframe df, int *new_xy, int *old_xy, int new_npar,
                                int old_npar, double *fargs, int *iargs,
                                score_func score)
{
    if (old_npar == 0)
        return score(df, new_xy, new_npar, fargs, iargs);
    else
        return score(df, new_xy, new_npar, fargs, iargs)
               - score(df, old_xy, old_npar, fargs, iargs);
}
