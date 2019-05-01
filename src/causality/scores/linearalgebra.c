/* Author: Alexander Rix
 * Date  : 3/16/2019
 * Description: linearalgebra.c contains the various linear algebra routines
 * for causality. These are highly performance sensitive functions and have
 * been somewhat tuned to provide quick performance. In particular, memory is
 * alligned when possible, which allows better simd vector moves
 * (particularly on older architectures). All of these linear algebra functions
 * also assume the data is normalized, which allows for additional some
 * simplifications. LAPACK is not used to keep causality from depending on an
 * external blas library, and the cholesky decompositions calculated in
 * causality are not much bigger than 10 x 10, and so a custom implementation
 * is likely to be faster than any blas library.
 */

#include <math.h>

#define EPSILON 1e-9

/*
 * calc_covariance_xy calculates the covariance between the random variable y
 * and the random variable vector x
 */
void calc_covariance_xy(double * restrict cov, double **x, double *y, int n,
                            int m)
{
    #ifndef  _WIN32
    y = __builtin_assume_aligned(y, 32);
    #endif
    double inv_nm1 = 1.0f / (n - 1.0f);
    for (int i = 0; i < m; ++i) {
        #ifndef _WIN32
        double *x_i = __builtin_assume_aligned(x[i], 32);
        #else
        double *x_i = x[i];
        #endif
        double sum = 0.0f;
        for (int j = 0; j < n; ++j)
            sum += x_i[j] * y[j];
        cov[i] = sum * inv_nm1;
    }
}

/*
 * calc_covariance_matrix calculates the covariance matrix of the
 * n x m dataset x.
 */
void calc_covariance_matrix(double * restrict cov, double **x, int n, int m)
{
    double inv_nm1 = 1.0f / (n - 1.0f);
    for (int i = 0; i < m; ++i) {
        #ifndef _WIN32
        double *x_i =  __builtin_assume_aligned(x[i], 32);
        #else
        double *x_i = x[i];
        #endif
        for (int j = i; j < m; ++j) {
            if (i == j)
                cov[j + m * i] = 1.0f;
            else {
                #ifndef _WIN32
                double *x_j =  __builtin_assume_aligned(x[j], 32);
                #else
                double *x_j =  x[j];
                #endif
                double sum = 0.0f;
                for (int k = 0; k < n; ++k)
                    sum += x_i[k] * x_j[k];
                cov[j + m * i] = sum * inv_nm1;
            }
        }
    }
}

/*
 * calc_cholesky_decomposition calculates the lower trianglular cholesky
 * decomposition for the given m x m covariance matrix. m is assumed >= 3
 */
int calc_cholesky_decomposition(double *cov, int m)
{
    for (int j = 1; j < m; ++j) {
        double *l_xj = cov + m * j;
        double *l_jx = cov + j;
        double  l_jj = 1.0f + EPSILON;
        for (int k = 0; k < j; ++k)
            l_jj -= l_jx[m * k] * l_jx[m * k];
        if (l_jj < 0.0f)
            return j;
        l_jj    = sqrt(l_jj);
        l_xj[j] = l_jj;
        l_jj    = 1.0f / l_jj;
        for (int i = j + 1; i < m; ++i) {
            for (int k = 0; k < j; ++k)
                l_xj[i] -= l_jx[m * k] * cov[i + m * k];
            l_xj[i] *= l_jj;
        }
    }
    return 0;
}

/*
 * calc_quadratic_form calculates the quadratic form
 * cov_xy^T (chol)^ -1 cov_xy  from the cholesky decomposition via backward /
 * forward substition.
 */
double calc_quadratic_form(double * restrict cov_xy, double *cov_xy_t,
                               double * restrict chol, int m)
{
    for (int i = 0; i < m; ++i) {
        double  s      = cov_xy[i];
        double *chol_i = chol + i;
        for (int j = 0; j < i; ++j)
            s -= chol_i[m * j] * cov_xy[j];
        cov_xy[i] = s / chol_i[m * i];
    }
    double sum = 0.0f;
    for (int i = m - 1; i >= 0; --i) {
        double  s       = cov_xy[i];
        double *chol_mi = chol + m * i;
        for (int j = i + 1; j < m; ++j)
            s -= chol_mi[j] * cov_xy[j];
        cov_xy[i] = s / chol_mi[i];
        sum += cov_xy[i] * cov_xy_t[i];
    }
    return sum;
}
