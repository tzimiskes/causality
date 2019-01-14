#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "headers/ges.h"
#include "headers/scores.h"
#include "headers/causality.h"

static double calcluate_bic_diff(double rss_p, double rss_m, double penalty,
    int nobs)
{
        return nobs * log(rss_p / rss_m) + penalty * log(nobs) * 2;
}

static int find(int node, int m, int *lbls) {
    int i;
    for (i = 0; i < m; ++i) {
        if (lbls[i] == node)
            break;
    }
    return i;
}

static void construct_covariances(double *cov, double *gsm_cov_xx,
                                               double *gsm_cov_xy, int *pars,
                                               int m, int *lbls)
{
    double *cov_xx   = cov;
    double *cov_xy   = cov + m * m;
    double *cov_xy_t = cov + m * (m + 1);
    for(int i = 0; i < m; ++i) {
        cov_xy[i] = gsm_cov_xy[pars[i]];
        int i_pc = find(pars[i], m, lbls);
        for(int j = 0; j < m; ++j) {
            int j_pc = find(pars[j], m, lbls);
            cov_xx[i * m + j] = gsm_cov_xx[i_pc * m + j_pc];
        }
    }
    memcpy(cov_xy_t, cov_xy, m * sizeof(double));
}



/* TODO */
double ges_bic_score(struct dataframe df, int xp, int y, int *x, int nx,
                                          struct score_args args,
                                          struct ges_score_mem gsm)
{
    double  penalty = args.fargs[0];
    double *m_mem   = malloc(nx * (nx + 2) * sizeof(double));
    if (m_mem == NULL)
        CAUSALITY_ERROR("Failed to allocate memory for BIC score\n");
    double *p_mem = malloc((nx + 1) * ((nx + 1) + 2) * sizeof(double));
    if (p_mem == NULL)
        CAUSALITY_ERROR("Failed to allocate memory for BIC score\n");

    construct_covariances(m_mem, gsm.cov_xx, gsm.cov_xy, x, gsm.m, gsm.lbls);
    double *cov_xx_p     = p_mem;
    double *cov_xy_p     = p_mem + (nx + 1) * (nx + 1);
    double *cov_xy_p_t   = p_mem + (nx + 1) * (nx + 2);
    memcpy(cov_xy_p + 1, m_mem + nx * nx, nx * sizeof(double));
    memcpy(cov_xy_p_t, cov_xy_p, (nx + 1 ) * sizeof(double));

    /* calculate the covariance vector between y and x */

    for (int i = 0; i < nx; ++i) {
        for (int j = 0; j < nx; ++j)
            cov_xx_p[j + 1 + (i + 1) * (nx + 1)] = m_mem[j + i * nx];
    }

    double rss_p = calculate_rss(p_mem, nx + 1);
    double rss_m = calculate_rss(m_mem, nx);

    free(p_mem);
    free(m_mem);
    return calcluate_bic_diff(rss_p, rss_m, penalty, df.nobs);
}


void ges_bic_optimization1(struct cgraph *cg, int y, int n, struct ges_score *gs)
{
    struct ill *p = cg->parents[y];
    struct ill *s = cg->spouses[y];
    /* Allocate memory to store the precalculated convariances. */
    struct ges_score_mem gsm;
    gsm.m       = ill_size(p) + ill_size(s);
    gsm.cov_xy  = malloc(n * sizeof(double));
    gsm.cov_xx  = malloc(gsm.m * gsm.m * sizeof(double));
    gsm.cov_xpx = malloc(gsm.m * sizeof(double));
    /* lbls stores the convariance matrix's column/row names */
    gsm.lbls    = malloc(gsm.m * sizeof(int));
    /* grab datafame and number of observations */
    double **df   = (double **) gs->df.df;
    int      nobs = gs->df.nobs;
    double *x[gsm.m];
    /* fill in x */
    int i = 0;
    while (p) {
        gsm.lbls[i] = p->key;
        x[i++]      = df[p->key];
        p           = p->next;
    }
    while (s) {
        gsm.lbls[i] = s->key;
        x[i++]      = df[s->key];
        s           = s->next;
    }
    fcov_xy(gsm.cov_xy, df, df[y], nobs, n);
    fcov_xx(gsm.cov_xx, x, nobs, gsm.m);
    gs->gsm = gsm;
}

void ges_bic_optimization2(int xp, struct ges_score *gs)
{
    double **df   = (double **) gs->df.df;
    int      nobs = gs->df.nobs;
    struct ges_score_mem gsm = gs->gsm;
    double *x[gsm.m + 1];
    x[0] = df[xp];
    for (int i = 0; i < gsm.m; ++i)
        x[i + 1] = df[gsm.lbls[i]];
    fcov_xy(gs->gsm.cov_xpx, x, x[0], nobs, gsm.m + 1);
}
