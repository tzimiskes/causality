/* Author: Alexander Rix
 * Date  : 3/8/2019
 * Description: r_causality_dataframe.c implements an R interface to the
 * causality dataframe structure, which is causality's internal storage for
 * dataframes. Notably on Unix systems, prepare_dataframe uses alligned memory
 * allocation for better loop vectorization. This is more helpful on
 * older architectures.
 */

#ifdef __WIN32__
#else
#define _POSIX_C_SOURCE 200112L
#endif

#include <dataframe.h>
#include <causality.h>
#include <R_causality/R_causality.h>

/* normalize a numeric variable */
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
struct dataframe *prepare_dataframe(SEXP Df, SEXP States)
{
    struct dataframe *df = malloc(sizeof(struct dataframe));
    if (!df)
        goto ERR;
    df->nvar   = length(Df);
    df->nobs   = length(VECTOR_ELT(Df, 0));
    df->states = INTEGER(States);
    df->df   = calloc(df->nvar, sizeof(void *));
    if (!df->df)
        goto ERR;
    for (int i = 0; i < df->nvar; ++i) {
        SEXP Df_i = VECTOR_ELT(Df, i);
        if (df->states[i]) {
            df->df[i] = malloc(df->nobs * sizeof(int));
            if (!df->df[i])
                goto ERR;
            memcpy(df->df[i], INTEGER(Df_i), df->nobs * sizeof(int));
        }
        else {
            #ifdef _WIN32
            df->df[i] = malloc(df->nobs *sizeof(double));
            #else
            posix_memalign(&df->df[i], 32, df->nobs * sizeof(double));
            #endif
            if (!df->df[i])
                goto ERR;
            memcpy(df->df[i], REAL(Df_i), df->nobs * sizeof(double));
            normalize(df->df[i], df->nobs);
        }
    }
    if (0) {
        ERR:
        CAUSALITY_ERROR("Failed to allocate memory for causality dataframe.");
        if (!df)
            return df;
        free_dataframe(df);
        df = NULL;
    }
    return df;
}

void free_dataframe(struct dataframe *df)
{
    if (df->df) {
        for (int i = 0; i < df->nvar; ++i)
            free(df->df[i]);
        free(df->df);
    }
    free(df);
}
