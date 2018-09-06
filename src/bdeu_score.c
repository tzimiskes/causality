#include <dataframe.h>
#include <causality.h>
#include <scores.h>

double bdeu_score(dataframe data, int *xy, int npar, double *fargs, int *iargs)
{
    double sample_prior    = fargs[0];
    double structure_prior = fargs[1];
    int    nobs           = data.nobs;
    int * df[npar + 1];
    for(int i = 0; i < npar + 1; ++i)
        df[i] = data.df[xy[i]];
    int * y          = df[npar];
    int   n_y_states = data.states[xy[npar]];
    int   x_states[npar];
    for(int i = 0; i < npar; ++i)
        x_states[i]  = data.states[xy[i]];
    /* now, we need to calcluate the total number of possible y states */
    int n_x_states  = 1;
    for(int i = 0; i < npar; ++i)
        n_x_states *= x_states[i];
    /* create a matrix that will store the frequencies of state (y, x).
    * The matrix will be indexed by row major format */
    int * alloced_mem = calloc((n_x_states + 1) * n_y_states, sizeof(int));
    int * n_jk = alloced_mem;
    int * n_j  = alloced_mem + n_x_states * n_y_states;
    /* unsure if y_vals should be allocated on the heap or stack */
    int x_state [npar];
    for(int i = 0; i < nobs; ++i) {
        int y_state = y[i];
        for(int j = 0; j < npar; ++j) {
            x_state[j] = df[j][i];
        }
        int k = 0;
        for (int j = 0; j < npar; ++j) {
            k *= x_states[j];
            k += x_state[j];
        }
        n_jk[k * n_y_states + y_state]++;
        n_j[k]++;
    }
    double score = npar * log(structure_prior/df.nvar - 1)) + (df.nvar - npar) *
         log(1.0f - (structure_prior/(df.nvar - 1)));
    double cell_prior = sample_prior / (n_x_states * n_y_states);
    double row_prior  = sample_prior / n_x_states;
    score += n_x_states * lgamma(row_prior);
    score -= n_y_states * n_x_states * lgamma(cell_prior);
    for(int i = 0; i < n_x_states; ++i) {
        score -= lgamma(row_prior + n_j[i]);
        for(int j = 0; j < n_y_states; ++j)
            score += lgamma(cell_prior + n_jk[j + i*n_y_states]);
    }
    free(alloced_mem);
    return score;
}
