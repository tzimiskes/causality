#ifndef CAUSALITY_LA_H
#define CAUSALITY_LA_H
void dc_cov_xy(double *restrict cov_xy, double **x, double *y, int n, int m);
void dc_cov_xx(double * restrict cov_xx, double **x, int n, int m);
#endif
