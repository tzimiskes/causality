#ifndef CONTINUOUS_BIC_H
#define CONTINUOUS_BIC_H

double bic_score(void ** xy_df, int * dims, int n_par, int n_obs);
void fcov_yy(double * restrict cov_yy,
             double ** y_df,
             int n_par,
             int n_obs);
void fcov_xy(double * restrict cov_xy,
             double ** xy_df,
             int n_par,
             int n_obs);
#endif
