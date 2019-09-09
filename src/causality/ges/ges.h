#ifndef GES_H
#define GES_H

#include <stdint.h>

#include <cgraph/cgraph.h>
#include <scores/scores.h>
#include <dataframe.h>

struct ges_score_mem {
    double *cov_xy;
    double *cov_xx; /* m by m matrix */
    double *cov_xpx;
    int    *lbls;
    unsigned int m: 31;
    unsigned int pc_cov: 1;
}; /* 36 bytes */



typedef double (*ges_score_func)(struct dataframe *df, int x, int y, int *ypar,
                                     int npar, struct score_args *args,
                                     struct ges_score_mem gsm);

struct ges_score {
    ges_score_func        gsf;
    struct ges_score_mem  gsm;
    struct dataframe     *df;
    struct score_args    *args;
};


double ges_bic_score(struct dataframe *df, int xp, int y, int *x, int nx,
                         struct score_args *args, struct ges_score_mem gsm);

double ges_bdeu_score(struct dataframe *df, int x, int y, int *ypar, int npar,
                          struct score_args *args, struct ges_score_mem gsm);

double ccf_ges(struct ges_score score, struct cgraph *cg);
#endif
