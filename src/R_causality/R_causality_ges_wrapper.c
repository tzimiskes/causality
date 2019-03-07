#ifndef __WIN32__
#define _POSIX_C_SOURCE 200112L
#endif

#include <R_causality/R_causality.h>

#include <causality.h>
#include <dataframe.h>
#include <cgraph/cgraph.h>
#include <scores/scores.h>
#include <ges/ges_internal.h>

/*
 * normalize continuous variables to help speed up scoring of continuous
 * variables.
 */
static void normalize(double *x, int n)
{
    double mu = 0.0f;
    for (int i = 0; i < n; ++i)
        mu += x[i];
    mu /= n;
    double var = 0.0f;
    for (int i = 0; i < n; ++i) {
        x[i] -= mu;
        var += x[i] * x[i];
    }
    var = 1 / sqrt(var / (n - 1));
    for (int i = 0; i < n; ++i)
        x[i] *= var;
}

struct dataframe prepare_df(SEXP Df, SEXP States)
{
    struct dataframe df;
    df.nvar   = length(Df);
    df.nobs   = length(VECTOR_ELT(Df, 0));
    df.states = INTEGER(States);
    df.df   = malloc(df.nvar * sizeof(void *));
    for (int i = 0; i < df.nvar; ++i) {
        SEXP Df_i = VECTOR_ELT(Df, i);
        if (df.states[i]) {
            
            df.df[i] = malloc(df.nobs * sizeof(int));
            memcpy(df.df[i], INTEGER(Df_i), df.nobs * sizeof(int));
        }
        else {
            #ifdef _WIN32
            df.df[i] = malloc(df.nobs *sizeof(double));
            #else
            posix_memalign(&df.df[i], 32, df.nobs * sizeof(double));
            #endif
            memcpy(df.df[i], REAL(Df_i), df.nobs * sizeof(double));
            normalize(df.df[i], df.nobs);
        }
    }
    return df;
}


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
    struct dataframe data = prepare_df(Df, States);
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
    struct ges_score score = {ges_score, {0}, data, {fargs, iargs}};
    struct cgraph *cg      = create_cgraph(data.nvar);
    /* run GES! */
    double graph_score     = ccf_ges(score, cg);
    /* free dataframe */
    for(int i = 0; i < data.nvar; ++i)
        free(data.df[i]);
    free(data.df);
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