#include <dataframe.h>
#ifndef SCORES_H
#define SCORES_H

#define BIC_SCORE "bic"
#define BDEU_SCORE "bdeu"

struct score_args {
    double *fargs;
    int    *iargs;
};

typedef double (*score_func)(struct dataframe df, int *xy, int npar,
                                                  struct score_args args);

typedef double (*ges_score_func)(struct dataframe df, int x, int y, int *ypar,
                                                      int npar,
                                                      struct score_args args,
                                                      double *fmem, int *imem);

double bdeu_score(struct dataframe df, int *xy, int npar,
                                       struct score_args args);

double ges_bdeu_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                             struct score_args args,
                                             double *fmem, int *imem);

double bic_score(struct dataframe df, int *xy, int npar,
                                      struct score_args args);

double ges_bic_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                            struct score_args args,
                                            double *fmem, int *imem);


void fcov_xx(double *cov_xx, double **x, int n, int m);
void fcov_xy(double *cov_xy, double **x, double *y, int n, int m);
#endif
