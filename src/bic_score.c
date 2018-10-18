/* TODO */
#include <causality.h>
#include <dataframe.h>
#include <scores.h>
#include <R_ext/Lapack.h>

#define ERROR_THRESH     1e-6
#define NOT_POSITIVE_DEFINITE 1
#define NON_POSITIVE_RESIDUAL_VARIANCE 2
void fcov_xx(double * restrict cov_xx, double * restrict * x, int npar,
                                         int nobs);
void fcov_xy(double * restrict cov_xy, double * restrict *x,
             double * restrict y, int npar, int nobs);

double calculate_rss(double *cov_xy, double *cov_xy_cpy, double *cov_xx,
                                     int npar);
double calcluate_bic(double rss, double penalty, int nobs, int npar) {
    return nobs * log(rss) + penalty * log(nobs) * (2 * npar + 1);
}

double bic_score(struct dataframe data, int *xy, int npar,
                                        double *fargs, int *iargs)
{
    double  penalty = fargs[0];
    int     nobs    = data.nobs;
    double *_y      = data.df[xy[npar]];
    /* allocate memor y for submatrix and fill in the columns */
    double **_x      = malloc((npar + 1) * sizeof(double *));
    for (int i = 0; i < npar + 1; ++i)
        _x[i] = data.df[xy[i]];

    /* Allocate memory for cov_xx and cov_xy in one block. */
    double *mem    = malloc((npar + 1) * (npar + 2) * sizeof(double));
    double *cov_xx = mem;
    double *cov_xy = mem + (npar + 1) * (npar + 1);
    double *cov_xy_cpy = mem + (npar + 1) * (npar + 2);
    fcov_xx(cov_xx, _x, npar, nobs);
    fcov_xy(cov_xy, _x, _y, npar, nobs);
    double rss = calculate_rss(cov_xy, cov_xy_cpy, cov_xx, npar);
    free(_x);
    free(mem);
    return calcluate_bic(rss, penalty, nobs, npar);
}


/* TODO */
double ges_bic_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                        double *fargs, int *iargs)
{
    double  penalty = fargs[0];
    int     nobs    = data.nobs;
    double *_y      = data.df[y];
    /* allocate memor y for submatrix and fill in the columns */
    double **_x      = malloc((npar + 1) * sizeof(double *));
    _x[0] = data.df[x];
    for (int i = 0; i < npar; ++i)
        _x[i + 1] = data.df[ypar[i]];

    /* Allocate memory for cov_xx and cov_xy in one block. */
    double *p_mem = malloc((npar + 1) * (npar + 3) * sizeof(double));
    if (p_mem == NULL)
        error("failed to allocate memory for BIC score\n");
    double *cov_xx_p     = p_mem;
    double *cov_xy_p     = p_mem + (npar + 1) * (npar + 1);
    double *cov_xy_p_cpy = p_mem + (npar + 1) * (npar + 2);
    double *m_mem   = malloc(npar * (npar + 2) * sizeof(double));
    if (m_mem == NULL)
        error("failed to allocate memory for BIC score\n");
    double *cov_xx_m     = m_mem;
    double *cov_xy_m     = m_mem + npar * npar;
    double *cov_xy_m_cpy = m_mem + npar * (npar + 1);
    /* Calculate the covariance matrix of the data matrix of x */
    fcov_xy(cov_xy_p, _x, _y, npar + 1, nobs);
    fcov_xx(cov_xx_p, _x, npar + 1, nobs);
    free(_x);
    /* calculate the covariance vector between y and x */
    memcpy(cov_xy_m, cov_xy_p + 1, npar * sizeof(double));
    memcpy(cov_xy_p_cpy, cov_xy_p, (npar + 1) * sizeof(double));
    memcpy(cov_xy_m_cpy, cov_xy_m, npar * sizeof(double));
    for (int j = 0; j < npar; ++j) {
        for (int i = 0; i < npar; ++i)
            cov_xx_m[i + j*npar] = cov_xx_p[(i + 1) + (npar + 1) * (j + 1)];
    }
    double rss_p = calculate_rss(cov_xy_p, cov_xy_p_cpy, cov_xx_p, npar + 1);
    double rss_m = calculate_rss(cov_xy_m, cov_xy_m_cpy, cov_xx_m, npar);
    double bic_p = calcluate_bic(rss_p, penalty, nobs, npar + 1);
    double bic_m = calcluate_bic(rss_m, penalty, nobs, npar);
    free(p_mem);
    free(m_mem);
    return bic_p - bic_m;
}

/*
 * Calcluate the BIC rss of this configuration by computing
 * log(cov_xx - cov_xy**T cov_xx^-1 * cov_xy) + log(n) * (npar + 1). The
 * first (and main step) in the rest of this function is to calcluate
 * cov_xy**T cov_xx^-1 * cov_xy.  Note that because we all variables are
 * normalized variables, cov_yy = 1
 */
double calculate_rss(double *cov_xy , double *cov_xy_cpy, double *cov_xx,
                                      int npar)
{
    int    err = 0;
    double rss = 1.0f;
    if (npar == 0)
        return rss;
    if (npar == 1) {
        rss -= cov_xy[0] * cov_xy[0];
    }
    /*
     * In the 2x2 case, we have cov_xx = |1, cov(x1, x2)|
     *                                   |cov(x2, x1), 1|
     */
    else if (npar == 2) {
        double det = 1.0f - cov_xx[1] * cov_xx[1];
        /*
         * For a 2x2 symmetric matrix with 1's on the diagonal, having a non
         * positive determinent is enough to know that the matrix is not
         * positive definite
         */
        if (det < ERROR_THRESH)
            err = NOT_POSITIVE_DEFINITE;
        else
            /* formula by hand */
            rss -= (cov_xy[0] * cov_xy[0] + cov_xy[1] * cov_xy[1]
                   - 2 * cov_xx[1] * cov_xy[0] * cov_xy[1])/det;
    }
    /*
     * since npar > 2, we will now use a few LAPACK routines to solve the
     * equation cov_xy**T * cov_xx^-1 * cov_xy via the cholesky decomposition
     * instead of doing it by hand.
     */
    else {
        int err = 0;
        int u   = 1;
        F77_CALL(dposv)("L", &npar, &u, cov_xx, &npar, cov_xy_cpy,
                             &npar, &err);
        if (err)
            err = NOT_POSITIVE_DEFINITE;
        else
            rss -= F77_CALL(ddot)(&npar, cov_xy, &u, cov_xy_cpy, &u);
    }
    if (rss < ERROR_THRESH)
        err = NON_POSITIVE_RESIDUAL_VARIANCE;
    if (err) {
        warning("The augmented matrix xy is not of full rank!\n");
        rss = DBL_MAX;
    }
    return rss;
}

void fcov_xx(double * restrict cov_xx, double * restrict * x, int npar, int nobs)
{
    int    u           = 1;
    double inv_nminus1 = 1.0f/(nobs - 1.0f);
    for(int i = 0; i < npar; ++i) {
        for(int j = 0; j <= i; ++j) {
            if(i == j)
                cov_xx[i + npar * j] = 1.0f;
            else
                cov_xx[i + npar * j] = F77_CALL(ddot)(&nobs, x[i], &u, x[j],
                                                             &u) * inv_nminus1;
        }
    }
}

/*
 * fcov_xy caclulates the covariance vector between the ranodom variable x
 * and the random variable vector, y.
 */
void fcov_xy(double * restrict cov_xy, double * restrict *x,
             double * restrict y, int npar, int nobs)
{
    int              u = 1;
    double inv_nminus1 = 1.0f/(nobs - 1.0f);
    for (int i = 0; i < npar; ++i)
        cov_xy[i] = F77_CALL(ddot)(&nobs, x[i], &u, y, &u) * inv_nminus1;
}
