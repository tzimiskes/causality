/* Author: Alexander Rix
 * Date  : 1/14/2019
 * Description: bic_score.c contains an implementation of BIC scoring for
 * continuous variables in a simple SEM. This particularly implementation is
 * designed to be used in the function score_graph. GES uses ges_bic_score,
 * however that uses, fcov_xx, fcov_xy, and calcluate_rss from this file.
 */

#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <float.h>

#include <causality.h>
#include <dataframe.h>
#include <scores/scores.h>
#include <scores/linearalgebra.h>

#define ERROR_THRESH 1e-12

double calcluate_bic(double rss, double penalty, int nobs, int npar) {
    return nobs * log(rss) + penalty * log(nobs) * (2 * npar + 1);
}

/* TODO */
double bic_score(struct dataframe *df, int *xy, int npar, struct score_args *args)
{
    double penalty = args->fargs[0];
    int    nobs    = df->nobs;
    double *y      = df->df[xy[npar]];
    /* allocate memory for submatrix and fill in the columns */
    double **x      = malloc(npar * sizeof(double *));
    for (int i = 0; i < npar; ++i)
        x[i] = df->df[xy[i]];

    /* Allocate memory for cov_xx and cov_xy in one block. */
    double *mem    = calloc((npar) * (npar + 2), sizeof(double));
    double *cov_xx = mem;
    double *cov_xy = mem + npar * npar;
    double *cov_xy_t = mem + npar * (npar + 1);
    memcpy(cov_xy_t, cov_xy, npar * sizeof(double));
    calc_covariance_matrix(cov_xx, x, nobs, npar);
    calc_covariance_xy(cov_xy, x, y, nobs, npar);
    double rss = calculate_rss(mem, npar);
    free(mem);
    return calcluate_bic(rss, penalty, nobs, npar);
}

/*
 * Calcluate the BIC rss of this configuration by computing
 * log(cov_xx - cov_xy**T cov_xx^-1 * cov_xy) + log(n) * (npar + 1). The
 * first (and main step) in the rest of this function is to calcluate
 * cov_xy**T cov_xx^-1 * cov_xy.  Note that because we all variables are
 * normalized variables, cov[i, i] = 1
 */
double calculate_rss(double *cov, int m)
{
    double *cov_xx   = cov;
    double *cov_xy   = cov + m * m;
    double *cov_xy_t = cov_xy + m;
    double rss = 1.0f;
    if (m == 0)
        return rss;
    else if (m == 1)
        return rss - cov_xy[0] * cov_xy[0];
    else if (m == 2) {
        /*
         * We have cov_xx = |1,           0|
         *                  |cov(x2, x1), 1|
         * For a 2x2 symmetric matrix A with 1's on the diagonal,
         * det(A) <= 0 ==> A is non positive definite.
         */
        double det = 1.0f - cov_xx[1] * cov_xx[1];
        if (det < ERROR_THRESH)
            CAUSALITY_ERROR("covariance matrix not positive definite.\n");
        else
        /* formula by hand */
            rss -= (cov_xy[0] * cov_xy[0] + cov_xy[1] * cov_xy[1]
                   - 2 * cov_xx[1] * cov_xy[0] * cov_xy[1]) / det;
        return rss;
    }
    else {
        /*
         * for m >= 3, we use the cholesky decompoistion and forward / backward
         * subsitution to solve the quadratic form and calculate the rss
         */
        int err = calc_cholesky_decomposition(cov_xx, m);
        if (err)
            CAUSALITY_ERROR("Leading minor of order %i not positive!\n", err);
        else
            rss -= calc_quadratic_form(cov_xy, cov_xy_t, cov_xx, m);
        return rss;
    }
}
