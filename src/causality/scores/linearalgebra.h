#ifndef CAUSALITY_LA_H
#define CAUSALITY_LA_H
void dc_cov_xy(double *restrict cov_xy, double **x, double *y, int n, int m);
void dc_cov_xx(double * restrict cov_xx, double **x, int n, int m);
int cholesky_decomp_cov3(double *cov, int m);
double calc_quadratic_form(double * restrict cov_xy, double * restrict cov_xy_t,
                               double * restrict chol, int m);
#endif
