#include "headers/causalityRWrapper.h"
#include "headers/causality.h"
#include "headers/cgraph.h"
#include "headers/dataframe.h"
#include "headers/scores.h"
#include "headers/ges.h"


/*
 * normalize continuous variables to help speed up scoring of continuous
 * variables.
 */
static void normalize(double *x, int n)
{
    double mu  = 0.0f;
    double var = 0.0f;
    for (int i = 0; i < n; ++i)
        mu += x[i];
    mu = mu / n;
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
            df.df[i] = malloc(df.nobs * sizeof(double));
            memcpy(df.df[i], REAL(Df_i), df.nobs * sizeof(double));
            normalize(df.df[i], df.nobs);
        }
    }
    return df;
}


SEXP ccf_ges_wrapper(SEXP Df, SEXP ScoreType, SEXP States,
                              SEXP FloatingArgs, SEXP IntegerArgs)
{
    /*
     * calcluate the integer arguments and floating point arguments for the
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
    else
        error("nope\n");
    /*
     * All the preprocessing work has now been done, so lets instantiate
     * an empty graph and run FGES
     */
    struct ges_score score = {ges_score, data, {fargs, iargs}, NULL, NULL};
    struct cgraph *cg      = create_cgraph(data.nvar);
    /* run ges */
    double graph_score      = ccf_ges(score, cg);
    for(int i = 0; i < data.nvar; ++i)
        free(data.df[i]);
    free(data.df);
    /* POST PROCESSING */
    SEXP Graph = causalityGraphFromCgraph(cg, getAttrib(Df, R_NamesSymbol));

    SEXP Output = PROTECT(allocVector(VECSXP, 2));
    SET_VECTOR_ELT(Output, 0, Graph);
    SET_VECTOR_ELT(Output, 1, ScalarReal(graph_score));
    free_cgraph(cg);
    UNPROTECT(1);
    return Output;
}
