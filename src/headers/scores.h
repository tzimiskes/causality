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
#endif
