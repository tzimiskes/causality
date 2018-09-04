#include <dataframe.h>
#ifndef _SCORES_H
#define _SCORES_H

#define BIC_SCORE "BIC"
#define BDEU_SCORE "BDeu"

double bdeu_score(dataframe data,
                  int * xy,
                  int n_par,
                  double * fargs,
                  int * iargs);
double bic_score(dataframe data,
                 int * xy,
                 int n_par,
                 double * fargs,
                 int * iargs);

double score_diff(dataframe df,
                  int * new_xy,
                  int * old_xy,
                  int new_n_par,
                  int old_n_par,
                  double * fargs,
                  int * iargs,
                  double (* score_fp)(dataframe, int *, int, double *, int *));
#endif
