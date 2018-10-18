#include <dataframe.h>
#ifndef _SCORES_H
#define _SCORES_H

#define BIC_SCORE "BIC"
#define BDEU_SCORE "BDeu"

typedef double (*score)(struct dataframe df, int *xy, int npar,
                                                  double *fargs, int *iargs);

typedef double (*ges_score)(struct dataframe data, int x, int y, int *ypar,
                                                int npar, double *fargs,
                                                int *iargs);

double bdeu_score(struct dataframe df, int *xy, int npar, double *fargs,
                                       int *iargs);

double bic_score(struct dataframe df, int *xy, int npar, double *fargs,
                                          int *iargs);

double ges_bic_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                     double *fargs, int *iargs);


#endif
