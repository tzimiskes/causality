#include <ges/ges.h>

double ges_bdeu_score(struct dataframe *df, int x, int y, int *ypar, int npar,
                                             struct score_args args,
                                            struct ges_score_mem gsm)
{
    int xy[npar + 2];
    xy[0] = x;
    for(int i = 0; i < npar; ++i)
        xy[i + 1] = ypar[i];
    xy[npar + 1] = y;
    double score_plus  = bdeu_score(df, xy, npar + 1, args);
    double score_minus = bdeu_score(df, xy + 1, npar, args);
    return score_plus - score_minus;
}
