#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "../headers/ges.h"
#include "../headers/scores.h"
#include "../headers/causality.h"

static double calcluate_bic_diff(double rss_p, double rss_m, double penalty,
    int nobs)
{
    return nobs * log(rss_p / rss_m) + penalty * log(nobs) * 2;
}

/*
 * find returns where x, a parent of y, is in the precalculated covariance matrix
 * cov_xx. The ordering between the parents of y and the precalculated matrix
 * need not be the same, and we need the ordering to be correct
 */
static int find(int x, int *lbls) {
    int i = 0;
    while (lbls[i] != x)
        i++;
    return i;
}

static void construct_aug_cov_mxp(double *aug_cov_mxp, struct ges_score_mem gsm,
                                                       int *x, int nx)
{
    double *cov_xx   = aug_cov_mxp;
    double *cov_xy   = aug_cov_mxp + nx * nx;
    double *cov_xy_t = aug_cov_mxp + nx * (nx + 1);
    for (int i = 0; i < nx; ++i) {
        cov_xy[i] = gsm.cov_xy[x[i]];
        int i_gsm = find(x[i], gsm.lbls);
        for (int j = 0; j < nx; ++j) {
            int j_gsm = find(x[j], gsm.lbls);
            cov_xx[i * nx + j] = gsm.cov_xx[i_gsm * gsm.m + j_gsm];
        }
    }
    memcpy(cov_xy_t, cov_xy, nx * sizeof(double));
}

static void construct_aug_cov_pxp(double *aug_cov_xpx, double *aug_cov_mxp,
                                                       struct ges_score_mem gsm,
                                                       int xp, int *x, int nx)
{
    double *cov_xpxxpx  = aug_cov_xpx;
    double *cov_xpxy    = aug_cov_xpx + (nx + 1) * (nx + 1);
    double *cov_xpxy_t  = aug_cov_xpx + (nx + 1) * ((nx + 1) + 1);

    double *cov_xx = aug_cov_mxp;
    double *cov_xy = aug_cov_mxp + nx * nx;
    cov_xpxy[0] = gsm.cov_xy[xp];
    memcpy(cov_xpxy + 1, cov_xy, nx * sizeof(double));
    memcpy(cov_xpxy_t, cov_xpxy, (nx + 1) * sizeof(double));

    /* calculate the covariance vector between y and x */
    cov_xpxxpx[0] = 1.0f;
    for (int i = 0; i < nx; ++i) {
        int i_gsm = find(x[i], gsm.lbls);
        cov_xpxxpx[i + 1] = cov_xpxxpx[(i + 1) * (nx + 1)] = gsm.cov_xpx[i_gsm];
    }

    for (int i = 0; i < nx; ++i) {
        for (int j = 0; j < nx; ++j)
            cov_xpxxpx[j + 1 + (i + 1) * (nx + 1)] = cov_xx[j + i * nx];
    }
}


/* TODO */
double ges_bic_score(struct dataframe df, int xp, int y, int *x, int nx,
                                          struct score_args args,
                                          struct ges_score_mem gsm)
{
    double  penalty = args.fargs[0];
    double *aug_cov_mxp = malloc(nx * (nx + 2) * sizeof(double));
    double *aug_cov_pxp = malloc((nx + 1) * ((nx + 1) + 2) * sizeof(double));
    if (aug_cov_mxp == NULL)
        CAUSALITY_ERROR("Failed to allocate memory for BIC score\n");
    if (aug_cov_pxp == NULL)
        CAUSALITY_ERROR("Failed to allocate memory for BIC score\n");
    construct_aug_cov_mxp(aug_cov_mxp, gsm, x, nx);
    construct_aug_cov_pxp(aug_cov_pxp, aug_cov_mxp, gsm, xp, x, nx);
    double rss_p = calculate_rss(aug_cov_pxp, nx + 1);
    double rss_m = calculate_rss(aug_cov_mxp, nx);
    free(aug_cov_mxp);
    free(aug_cov_pxp);
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
    double *x[gsm.m];
    for (int i = 0; i < gsm.m; ++i)
        x[i] = df[gsm.lbls[i]];
    fcov_xy(gs->gsm.cov_xpx, x, df[xp], nobs, gsm.m);
}
