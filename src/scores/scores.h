#include <headers/dataframe.h>

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

double bdeu_score(struct dataframe data, int *xy, int npar,
                                         struct score_args args);


double bic_score(struct dataframe data, int *xy, int npar,
                                        struct score_args args);

void   fcov_xx(double * restrict cov_xx, double **x, int n, int m);
void   fcov_xy(double * restrict cov_xy, double **x, double *y, int n, int m);
double calculate_rss(double *cov, int m);
#endif
