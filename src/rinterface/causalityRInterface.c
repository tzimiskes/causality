#include "../headers/causalityRWrapper.h"
#include "../headers/causality.h"
#include "../headers/scores.h"

/*
 * causalitySort takes in an R object, proccesses it down to the C level
 * and then runs C level sort on this lower level representation. In then takes
 * the output of ccf_sort and turns it back into an R object. ccf_sort returns
 * NULL if graph doesn't have a sort. In that case, we return R_NilValue
 * (aka R's version of NULL)
 */
SEXP causalitySort(SEXP Graph)
{
    struct cgraph *cg = cgraph_from_causality_graph(Graph);
    int *sort = ccf_sort(cg);
    free_cgraph(cg);
    if (sort == NULL)
        return R_NilValue;
    SEXP Nodes  = VECTOR_ELT(Graph, NODES);
    SEXP Sorted = PROTECT(allocVector(STRSXP, cg->n_nodes));
    /* convert C level output to R level output */
    for (int i = 0; i < cg->n_nodes; ++i)
        SET_STRING_ELT(Sorted, i, STRING_ELT(Nodes, sort[i]));
    free(sort);
    UNPROTECT(1);
    return Sorted;
}

SEXP causalityPDX(SEXP Pdag)
{
    struct cgraph *cg = cgraph_from_causality_graph(Pdag);
    cg = ccf_pdx(cg);
    if (cg == NULL)
        return R_NilValue;
    SEXP Dag = PROTECT(duplicate(Pdag));
    calcluateEdgesFromCgraph(cg, Dag);
    free_cgraph(cg);
    UNPROTECT(1);
    return Dag;
}

SEXP causalityScoreGraph(SEXP Graph, SEXP Df, SEXP ScoreType, SEXP States,
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

SEXP causalityChickering(SEXP Graph)
{
    struct cgraph *cg = cgraph_from_causality_graph(Graph);
    ccf_chickering(cg);
    SEXP Pattern = PROTECT(duplicate(Graph));
    calcluateEdgesFromCgraph(cg, Pattern);
    free_cgraph(cg);
    UNPROTECT(1);
    return Pattern;
}

SEXP causalityMeek(SEXP Graph)
{
     struct cgraph *cg = cgraph_from_causality_graph(Graph);
     ccf_meek(cg);
     SEXP Pattern = PROTECT(duplicate(Graph));
     calcluateEdgesFromCgraph(cg, Pattern);
     free_cgraph(cg);
     UNPROTECT(1);
     return Pattern;
}
