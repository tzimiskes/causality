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
    int *sort = causality_sort(cg);
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
    cg = causality_pdx(cg);
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
    causality_chickering(cg);
    SEXP nodes   = VECTOR_ELT(dag, NODES);
    SEXP pattern = PROTECT(causality_graph_from_cgraph(cg, nodes));
    free_cgraph(cg);
    UNPROTECT(1);
    return pattern;
}

SEXP r_causality_meek(SEXP pdag)
{
    struct cgraph *cg = cgraph_from_causality_graph(pdag);
    causality_meek(cg);
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
    struct dataframe df = prepare_df(Df, States);
    double graph_score = causality_score_graph(cg, df, score, args);
    free(df.df);
    free_cgraph(cg);
    return ScalarReal(graph_score);
}
