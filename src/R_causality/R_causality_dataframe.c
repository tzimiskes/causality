#include <dataframe.h>
#include <causality.h>
#include <R_causality/R_causality.h>

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

/*
 * states stores the number of states (in the sense of degrees of freedom)
 * of each variable in the data frame with the caveat that for continuous
 * variables, the number of states is considered to be to be 0 instead of 1.
 * This allows me to use the actual value in each entry of dims to determine
 * the type (real or discrete/integer)of the variable in the data frame.
 * Instead, we store the columns as void pointers in df. This helps divorce
 * C and R so it is easier to port this package to python, julia, etc.
 */
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
