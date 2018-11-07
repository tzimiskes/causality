#include <dataframe.h>
#ifndef _SCORES_H
#define _SCORES_H

#define BIC_SCORE "BIC"
#define BDEU_SCORE "BDeu"

typedef double (*score)(struct dataframe df, int *xy, int npar,
                                                  double *fargs, int *iargs);

typedef double (*ges_score)(struct dataframe data, int x, int y, int *ypar,
                                                   int npar, double *fargs,
                                                   int *iargs, double *fmem,
                                                   int *imem);

double bdeu_score(struct dataframe df, int *xy, int npar, double *fargs,
                                       int *iargs);

double bic_score(struct dataframe df, int *xy, int npar, double *fargs,
                                          int *iargs);

double ges_bic_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                     double *fargs, int *iargs, double *fmem,
                                     int *imem);


void fcov_xx(double * restrict cov_xx, double * restrict * x, int npar,
                                       int nobs);
void fcov_xy(double * restrict cov_xy, double * restrict *x,
                                       double * restrict y, int npar, int nobs);
#endif
