#include <R_causality/R_causality.h>
#include <causality.h>
#include <scores/scores.h>

/*
 * causalitySort takes in an R object, proccesses it down to the C level
 * and then runs C level sort on this lower level representation. In then takes
 * the output of ccf_sort and turns it back into an R object. ccf_sort returns
 * NULL if graph doesn't have a sort. In that case, we return R_NilValue
 * (aka R's version of NULL)
 */
SEXP r_causality_sort(SEXP graph)
{
    struct cgraph *cg = cgraph_from_causality_graph(graph);
    /* Attempt to generate a topological sort of cg */
    int *sort = ccf_sort(cg);
    free_cgraph(cg);
    if (sort == NULL)
        return R_NilValue;
    SEXP nodes  = VECTOR_ELT(graph, NODES);
    SEXP sorted = PROTECT(allocVector(STRSXP, cg->n_nodes));
    /* convert C level output to R level output */
    for (int i = 0; i < cg->n_nodes; ++i)
        SET_STRING_ELT(sorted, i, STRING_ELT(nodes, sort[i]));
    free(sort);
    UNPROTECT(1);
    return sorted;
}

SEXP r_causality_pdx(SEXP pdag)
{
    struct cgraph *cg = cgraph_from_causality_graph(pdag);
    cg = ccf_pdx(cg);
    if (cg == NULL)
        return R_NilValue;
    SEXP nodes = VECTOR_ELT(pdag, NODES);
    SEXP dag   = PROTECT(causality_graph_from_cgraph(cg, nodes));
    free_cgraph(cg);
    UNPROTECT(1);
    return dag;
}

SEXP r_causality_chickering(SEXP dag)
{
    struct cgraph *cg = cgraph_from_causality_graph(dag);
    ccf_chickering(cg);
    SEXP nodes   = VECTOR_ELT(dag, NODES);
    SEXP pattern = PROTECT(causality_graph_from_cgraph(cg, nodes));
    free_cgraph(cg);
    UNPROTECT(1);
    return pattern;
}

SEXP r_causality_meek(SEXP pdag)
{
    struct cgraph *cg = cgraph_from_causality_graph(pdag);
    ccf_meek(cg);
    SEXP nodes = VECTOR_ELT(pdag, NODES);
    SEXP cpdag = PROTECT(causality_graph_from_cgraph(cg, nodes));
    free_cgraph(cg);
    UNPROTECT(1);
    return cpdag;
}

SEXP r_causality_score_graph(SEXP Graph, SEXP Df, SEXP ScoreType, SEXP States,
                                         SEXP FloatingArgs, SEXP IntegerArgs)
{
    struct cgraph *cg = cgraph_from_causality_graph(Graph);
    struct score_args args = {NULL, NULL};
    score_func score;
    if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
        score = bic_score;
    else if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BDEU_SCORE))
        score = bdeu_score;
    else
        error("nope\n");
    /*
     * Determine the integer arguments and floating point arguments for the
     * score function.
     */
    if (!isNull(IntegerArgs))
        args.iargs = INTEGER(IntegerArgs);
    if (!isNull(FloatingArgs))
        args.fargs = REAL(FloatingArgs);
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
    df.nvar   = length(Df);
    df.nobs   = length(VECTOR_ELT(Df, 0));
    df.states = INTEGER(States);
    df.df     = malloc(df.nvar * sizeof(void *));
    /* populate df with the pointers to the columns of the R dataframe */
    for (int i = 0; i < df.nvar; ++i) {
        if (df.states[i])
            df.df[i] = INTEGER(VECTOR_ELT(Df, i));
        else
            df.df[i] = REAL(VECTOR_ELT(Df, i));
    }
    double graph_score = ccf_score_graph(cg, df, score, args);
    free(df.df);
    free_cgraph(cg);
    return(ScalarReal(graph_score));
}
