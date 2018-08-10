#ifndef CONTINUOUS_BIC_H
#define CONTINUOUS_BIC_H

double continuous_bic(double * node,
                      double ** parents,
                      int n_parents,
                      int n_obs);
void fcov_yy(double * restrict cov_yy,
             double ** parents,
             int n_parents,
             int n_obs);
void fcov_xy(double * restrict cov_yy,
             double * restrict node,
             double ** parents,
             int n_parents,
             int n_obs);
#endif
