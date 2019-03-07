#include <ges/ges.h>

double ges_bdeu_score(struct dataframe data, int x, int y, int *ypar, int npar,
                                             struct score_args args,
                                            struct ges_score_mem gsm)
{
    int xy[npar + 2];
    xy[0] = x;
    for(int i = 0; i < npar; ++i)
        xy[i + 1] = ypar[i];
    xy[npar + 1] = y;
    double score_plus  = bdeu_score(data, xy, npar + 1, args);
    double score_minus = bdeu_score(data, xy + 1, npar, args);
    return score_plus - score_minus;
}
