#ifndef LINEARALGEBRA_H
#define LINEARALGEBRA_H

void calc_covariance_xy(double *restrict cov_xy, double **x, double *y, int n,
                            int m);
void calc_covariance_matrix(double * restrict cov, double **x, int n, int m);
int calc_cholesky_decomposition(double *cov, int m);
double calc_quadratic_form(double * restrict cov_xy, double * restrict cov_xy_t,
                               double * restrict chol, int m);
#endif
