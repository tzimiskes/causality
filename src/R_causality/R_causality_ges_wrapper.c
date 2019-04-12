#include <R_causality/R_causality.h>

#include <causality.h>
#include <dataframe.h>
#include <cgraph/cgraph.h>
#include <scores/scores.h>
#include <ges/ges_internal.h>

SEXP r_causality_ges(SEXP Df, SEXP ScoreType, SEXP States,
                           SEXP FloatingArgs, SEXP IntegerArgs)
{
    /*
     * calculate the integer arguments and floating point arguments for the
     * score function.
     */
    int *iargs = NULL;
    if (!isNull(IntegerArgs))
        iargs = INTEGER(IntegerArgs);
    double *fargs = NULL;
    if (!isNull(FloatingArgs))
        fargs = REAL(FloatingArgs);
    struct dataframe *df = prepare_dataframe(Df, States);
    ges_score_func ges_score;
    if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
        ges_score = ges_bic_score;
    else if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BDEU_SCORE))
        ges_score = ges_bdeu_score;
    else
        error("nope\n");
    /*
     * All the preprocessing work has now been done, so lets instantiate
     * an empty graph and run FGES.
     */
    struct ges_score score = {ges_score, {0}, df, {fargs, iargs}};
    struct cgraph *cg      = create_cgraph(df->nvar);
    /* run GES! */
    double graph_score     = ccf_ges(score, cg);
    /* free dataframe */
    free_dataframe(df);
    /* Create R causality.graph object from cg */
    SEXP Output = PROTECT(allocVector(VECSXP, 2));
    SEXP Names  = PROTECT(getAttrib(Df, R_NamesSymbol));
    SET_VECTOR_ELT(Output, 0, causality_graph_from_cgraph(cg, Names));
    SET_VECTOR_ELT(Output, 1, ScalarReal(graph_score));
    free_cgraph(cg);
    /* Set the output of GES to the class causality.pattern */
    SEXP Class = PROTECT(allocVector(STRSXP, 2));
    SET_STRING_ELT(Class, 0, mkChar("causality.pattern"));
    SET_STRING_ELT(Class, 1, mkChar("causality.graph"));
    setAttrib(VECTOR_ELT(Output, 0), R_ClassSymbol, Class);
    /* Return the graph and its score */
    UNPROTECT(3);
    return Output;
}
