
#include <stdlib.h>
#include <ges/ges.h>

double ges_bdeu_score(struct dataframe *df, int x, int y, int *ypar, int npar,
                                             struct score_args *args,
                                            struct ges_score_mem gsm)
{
    int *xy = malloc((npar + 2) * sizeof(int));
    xy[0] = x;
    xy[npar + 1] = y;
    for(int i = 0; i < npar; ++i)
        xy[i + 1] = ypar[i];
    double score_plus  = bdeu_score(df, xy, npar + 1, args);
    double score_minus = bdeu_score(df, xy + 1, npar, args);
    free(xy);
    return score_plus - score_minus;
}

double ges_discrete_bic_score(struct dataframe *df, int x, int y, int *ypar,
                                  int npar, struct score_args *args,
                                  struct ges_score_mem gsm)
{
    int *xy = malloc((npar + 2) * sizeof(int));
    xy[0] = x;
    xy[npar + 1] = y;
    for(int i = 0; i < npar; ++i)
        xy[i + 1] = ypar[i];
    double score_plus  = discrete_bic_score(df, xy, npar + 1, args);
    double score_minus = discrete_bic_score(df, xy + 1, npar, args);
    free(xy);
    return score_plus - score_minus;
}
