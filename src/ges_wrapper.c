#include <causality.h>
#include <cgraph.h>
#include <dataframe.h>
#include <scores.h>

struct score {
    ges_score score;
    struct dataframe df;
    double    *fargs;
    int       *iargs;
    double    *fmem;
    int       *imem;
};


struct cgraph * ccf_ges(struct score score);

void normalize(double * x, int n) {
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

struct dataframe prepare_df(SEXP Df, SEXP States) {
    struct dataframe d;
    d.nvar   = length(Df);
    d.nobs   = length(VECTOR_ELT(Df, 0));
    d.states = INTEGER(States);
    d.df   = malloc(d.nvar * sizeof(void *));
    for (int i = 0; i < d.nvar; ++i) {
        SEXP Df_i = VECTOR_ELT(Df, i);
        if (d.states[i]) {
            d.df[i] = malloc(d.nobs * sizeof(int));
            memcpy(d.df[i], INTEGER(Df_i), d.nobs * sizeof(int));
        }
        else {
            d.df[i] = malloc(d.nobs * sizeof(double));
            memcpy(d.df[i], REAL(Df_i), d.nobs * sizeof(double));
            normalize(d.df[i], d.nobs);
        }
    }
    return d;
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
    ges_score ges_score;
    if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
        ges_score = ges_bic_score;
    else
        error("nope\n");
    /*
     * All the preprocessing work has now been done, so lets instantiate
     * an empty graph and run FGES
     */
    struct score score = {ges_score, data, fargs, iargs, NULL, NULL};
    struct cgraph *cg  = ccf_ges(score);
    /* POST PROCESSING */
    free_cgraph(cg);
    for(int i = 0; i < data.nvar; ++i)
        free(data.df[i]);
    free(data.df);
    Rprintf("GES complete\n");
    return ScalarReal(0);
}
