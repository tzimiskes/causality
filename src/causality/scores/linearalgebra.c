/* Author: Alexander Rix
 * Date  : 1/22/2019
 * Description: ges.c contains a (partially optimized) version of
 * greedy equivalence search by Chickering. It is a score based
 * causal discovery algorithm that is correct in the the large sample
 * limit given its assumptions. For more information see the paper by
 * David Maxwell Chickering, Optimal Structure Identification With Greedy
 * Search Journal of Machine Learning Research 3 (2002) 507-554
 */

#include <math.h>

#define EPSILON 1e-6

void dc_cov_xy(double *restrict cov_xy, double **x, double *y, int n, int m)
{
    #ifndef  _WIN32
    y = __builtin_assume_aligned(y, 32);
    #endif
    double inv_nm1 = 1.0f / (n - 1.0f);
    //#pragma omp parallel for num_threads(2) if (m > 8)
    for (int i = 0; i < m; ++i) {
        #ifndef _WIN32
        double *x_i = __builtin_assume_aligned(x[i], 32);
        #else
        double *x_i = x[i];
        #endif
        double sum = 0.0f;
        for (int j = 0; j < n; ++j)
            sum += x_i[j] * y[j];
    cov_xy[i] = sum * inv_nm1;
    }
}

void dc_cov_xx(double * restrict cov_xx, double **x, int n, int m)
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
                cov_xx[j + m * i] = 1.0f;
            else {
                #ifndef _WIN32
                double *x_j =  __builtin_assume_aligned(x[j], 32);
                #else
                double *x_j =  x[j];
                #endif
                double sum = 0.0f;
                for (int k = 0; k < n; ++k)
                    sum += x_i[k] * x_j[k];
                cov_xx[j + m * i] = sum * inv_nm1;
                cov_xx[i + m * j] = cov_xx[j + m * i];
            }
        }
    }
}

/*
 * cholesky_decomp_cov3 calculates the lower trianglular cholesky decomposition
 * for the given m x m covariance matrix. m is assumed >= 3
 */
int cholesky_decomp_cov3(double *cov, int m)
{
    for (int j = 1; j < m; ++j) {
        double *l_xj = cov + m * j;
        double *l_jx = cov + j;
        double  l_jj = 1.0f;
        for (int k = 0; k < j; ++k)
            l_jj -= l_jx[m * k] * l_jx[m * k];
        if (l_jj < 0.0f)
            return j;
        l_jj    = sqrt(l_jj);
        l_xj[j] = l_jj;
        l_jj    = 1 / l_jj;
        for (int i = j + 1; i < m; ++i) {
            for (int k = 0; k < j; ++k)
                l_xj[i] -= l_jx[m * k] * cov[i + m * k];
            l_xj[i] *= l_jj;
        }
    }
    return 0;
}


/*
 * solve solves the linear systems cov_xy^T (cov_xx)^ -1 cov_xy via the
 * cholesky decomposition and backward forward subsitution
 */
double calc_quadratic_form(double * restrict cov_xy, double * restrict cov_xy_t,
                               double * restrict chol, int m)
{
    for (int i = 0; i < m; ++i) {
        double s = cov_xy[i];
        for (int j = 0; j < i; ++j)
            s -= chol[i + m * j] * cov_xy[j];
        cov_xy[i] = s / chol[i + m * i];
    }
    double sum = 0.0f;
    for (int i = m - 1; i >= 0; --i) {
        double s = cov_xy[i];
        for (int j = i + 1; j < m; ++j)
            s -= chol[j + m * i] * cov_xy[j];
        cov_xy[i] = s / chol[i + m * i];
        sum += cov_xy[i] * cov_xy_t[i];
    }
    return sum;
}
