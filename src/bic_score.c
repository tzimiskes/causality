/* TODO */
#include <causality.h>
#include <dataframe.h>
#include <scores.h>
#include <R_ext/Lapack.h>

#define ERROR_THRESH     1e-9
void fcov_xx(double * restrict cov_xx, double * restrict * x, int n,
                                         int m);
void fcov_xy(double * restrict cov_xy, double * restrict *x,
             double * restrict y, int n, int m);

double calculate_rss(double *cov, int m);

double calcluate_bic(double rss, double penalty, int nobs, int npar) {
    return nobs * log(rss) + penalty * log(nobs) * (2 * npar + 1);
}

static double calcluate_bic_diff(double rss_p, double rss_m, double penalty,
                                               int nobs)
{
    return nobs * log(rss_p/rss_m) + penalty * log(nobs) * 2;
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
    double *mem    = calloc((npar + 1) * (npar + 2), sizeof(double));
    double *cov_xx = mem;
    double *cov_xy = mem + (npar + 1) * (npar + 1);
    double *cov_xy_t = mem + (npar + 1) * (npar + 2);
    memcpy(cov_xy_t, cov_xy, npar * sizeof(double));
    fcov_xx(cov_xx, _x, npar, nobs);
    fcov_xy(cov_xy, _x, _y, npar, nobs);
    double rss = calculate_rss(mem, npar);
    free(mem);
    return calcluate_bic(rss, penalty, nobs, npar);
}

int find(int node, int n_lbls, int *lbls) {
    for(int i = 0; i < n_lbls; ++i) {
        if (lbls[i] == node)
            return i;
    }
    error("Failed to match node between covariance matrices");
}

void construct_covariances(double *cov, double *pc_cov_xx, double *pc_cov_xy,
                                        int *pars, int m, int *lbls,
                                        int n_lbls)
{
    double *cov_xx   = cov;
    double *cov_xy   = cov + m * m;
    double *cov_xy_t = cov + m * (m + 1);
    for(int i = 0; i < m; ++i) {
        cov_xy[i] = pc_cov_xy[pars[i]];
        int i_pc = find(pars[i], n_lbls, lbls);
        for(int j = 0; j < m; ++j) {
            int j_pc = find(pars[j], n_lbls, lbls);
            cov_xx[i * m + j] = pc_cov_xx[i_pc * n_lbls + j_pc];
        }
    }
    memcpy(cov_xy_t, cov_xy, m * sizeof(double));
}

/* TODO */
double ges_bic_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                        double *fargs, int *iargs, double *fmem,
                                        int *imem)
{
    double  penalty   = fargs[0];
    int     n_lbls    = *imem;
    int    *lbls      = imem + 1;
    double *pc_cov_xy = fmem;
    double *pc_cov_xx = fmem + data.nvar;
    int     nobs      = data.nobs;
    double *m_mem   = calloc(npar * (npar + 2), sizeof(double));
    if (m_mem == NULL)
    error("failed to allocate memory for BIC score\n");
    construct_covariances(m_mem, pc_cov_xx, pc_cov_xy, ypar, npar, lbls, n_lbls);
    double *p_mem = calloc((npar + 1) * (npar + 1 + 2), sizeof(double));
    if (p_mem == NULL)
    error("failed to allocate memory for BIC score\n");
    double *cov_xx_p     = p_mem;
    double *cov_xy_p     = p_mem + (npar + 1) * (npar + 1);
    double *cov_xy_p_t   = p_mem + (npar + 1) * (npar + 2);
    cov_xy_p[0] = pc_cov_xy[x];
    memcpy(cov_xy_p + 1, m_mem + npar * npar, npar * sizeof(double));
    memcpy(cov_xy_p_t, cov_xy_p, (npar + 1 ) * sizeof(double));
    double **_x       = malloc((npar + 1) * sizeof(double *));
    _x[0] = data.df[x];
    for (int i = 0; i < npar; ++i)
        _x[i + 1] = data.df[ypar[i]];
    fcov_xy(cov_xx_p, _x, _x[0], nobs, npar + 1);
    free(_x);


    /* calculate the covariance vector between y and x */

    for (int i = 0; i < npar; ++i) {
        for (int j = 0; j < npar; ++j)
            cov_xx_p[j + 1 + (i + 1) * (npar + 1)] = m_mem[j + i * npar];
    }
    double rss_p = calculate_rss(p_mem, npar + 1);
    if(rss_p == -1000) {
        for (int i = 0; i < npar; ++i) {
            for (int j = 0; j < npar; ++j)
            Rprintf("%f ", m_mem[i + npar * j]);
            Rprintf("\n");
        }
        Rprintf("precalc\n");
        for (int i = 0; i < n_lbls; ++i) {
            for (int j = 0; j < n_lbls; ++j)
            Rprintf("%f ", pc_cov_xx[i + n_lbls * j]);
            Rprintf("\n");
        }
        for(int i = 0; i < n_lbls; ++ i)
            Rprintf("%i ", lbls[i]);
        Rprintf("\n");
        for(int i = 0; i < npar; ++i)
            Rprintf("%i ", ypar[i]);
        Rprintf("\n");
        error("err\n");
    }
    double rss_m = calculate_rss(m_mem, npar);

    free(p_mem);
    free(m_mem);
    return calcluate_bic_diff(rss_p, rss_m, penalty, nobs);
}

/*
 * Calcluate the BIC rss of this configuration by computing
 * log(cov_xx - cov_xy**T cov_xx^-1 * cov_xy) + log(n) * (npar + 1). The
 * first (and main step) in the rest of this function is to calcluate
 * cov_xy**T cov_xx^-1 * cov_xy.  Note that because we all variables are
 * normalized variables, cov_yy = 1
 */
double calculate_rss(double *cov, int m)
{
    double *cov_xx   = cov;
    double *cov_xy   = cov + m * m;
    double *cov_xy_t = cov_xy + m;

    double rss = 1.0f;
    if (m == 0)
        return rss;
    if (m == 1) {
        rss -= cov_xy[0] * cov_xy[0];
    }
    /*
     * In the 2x2 case
     * we have cov_xx = |1,           0|
     *                  |cov(x2, x1), 1|
     */
    else if (m == 2) {
        double det = 1.0f - cov_xx[1] * cov_xx[1];
        /*
         * For a 2x2 symmetric matrix with 1's on the diagonal, having a non
         * positive determinent is enough to know that the matrix is not
         * positive definite
         */
        if (det < ERROR_THRESH)  {
            Rprintf("DET is too small!!\n");
            rss = DBL_MAX;
        }
        else
            /* formula by hand */
            rss -= (cov_xy[0] * cov_xy[0] + cov_xy[1] * cov_xy[1]
                   - 2 * cov_xx[1] * cov_xy[0] * cov_xy[1])/det;
    }
    /*
     * since npar > 2, we will use LAPACK to solve the
     * equation cov_xy**T * cov_xx^-1 * cov_xy
     */
    else {
        int err = 0;
        int u   = 1;
        /*
         * dposv solves the linear system Ax = b, where A is a symmetric
         * positive definite matrix
         */
        F77_CALL(dposv)("L", &m, &u, cov_xx, &m, cov_xy, &m, &err);
        if (err) {
            warning("The leading minor of order %i is not positive definite!\n", err);
            rss = DBL_MAX;
        }
        else
            rss -= F77_CALL(ddot)(&m, cov_xy, &u, cov_xy_t, &u);
    }
    if (rss < ERROR_THRESH) {
        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < m; ++j)
            Rprintf("%f ", cov_xx[i + m * j]);
            Rprintf("\n");
        }
        return -1000;
        rss = DBL_MAX;
    }
    return rss;
}

/*
 * fcov_xy calculates the covariance matrix between random variable vector
 */

void fcov_xx(double * restrict cov_xx, double * restrict *x, int n, int m)
{
    int    u           = 1;
    double inv_nminus1 = 1.0f / (n - 1.0f);
    for(int i = 0; i < m; ++i) {
        for(int j = i; j < m; ++j) {
            if(i == j)
                cov_xx[j + m * i] = 1.0f;
            else {
                cov_xx[j + m * i] = F77_CALL(ddot)(&n, x[i], &u, x[j],
                    &u) * inv_nminus1;
                cov_xx[i + m * j] = cov_xx[j + m * i];
            }
        }
    }
}

/*
 * fcov_xy calculates the covariance vector between the random variable y
 * and the random variable vector, x.
 */
void fcov_xy(double * restrict cov_xy, double * restrict *x,
             double * restrict y, int n, int m)
{
    int              u = 1;
    double inv_nminus1 = 1.0f / (n - 1.0f);
    for (int i = 0; i < m; ++i)
        cov_xy[i] = F77_CALL(ddot)(&n, x[i], &u, y, &u) * inv_nminus1;
}
