#include <dataframe.h>
#ifndef _SCORES_H
#define _SCORES_H

#define BIC_SCORE "BIC"
#define BDEU_SCORE "BDeu"

typedef double (* score_func)(struct dataframe df,
                              int      *xy,
                              int       npar,
                              double   *fargs,
                              int      *iargs);

double bdeu_score(struct dataframe data,
                  int    *xy,
                  int     npar,
                  double *fargs,
                  int    *iargs);
double bic_score(struct dataframe data,
                 int      *xy,
                 int       npar,
                 double   *fargs,
                 int      *iargs);

double score_diff(struct dataframe df,
                  int       *new_xy,
                  int       *old_xy,
                  int        new_npar,
                  int        old_npar,
                  double    *fargs,
                  int       *iargs,
                  score_func score);
#endif
