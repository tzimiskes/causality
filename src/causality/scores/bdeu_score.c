#include <stdlib.h>
#include <math.h>

#include <dataframe.h>
#include <causality.h>
#include <scores/scores.h>

double bdeu_score(struct dataframe *df, int *xy, int npar,
                                         struct score_args args)
{
    double sample_prior    = args.fargs[0];
    double structure_prior = args.fargs[1];
    int * data[npar + 1];
    for(int i = 0; i < npar + 1; ++i)
        data[i] = df->df[xy[i]];
    int *y          = data[npar];
    int  n_y_states = df->states[xy[npar]];
    /* get the number of states for each x */
    int x_states[npar];
    for(int i = 0; i < npar; ++i)
        x_states[i]  = df->states[xy[i]];
    /* Now, we need to calcluate the total number of possible x states */
    int n_x_states  = 1;
    for(int i = 0; i < npar; ++i)
        n_x_states *= x_states[i];
    /*
     * Create a matrix that will store the frequencies of the microstate (x, y).
     * The matrix will populated in row major format.
     */
    int *alloced_mem = calloc(n_x_states * (n_y_states + 1), sizeof(int));
    if (alloced_mem == NULL)
        CAUSALITY_ERROR("Failed to allocate enough memory for bdeu_score\n");
    int *n_jk = alloced_mem;
    int *n_j  = alloced_mem + n_x_states * n_y_states;
    /*
     * x_state stores the observed microstate (x). Unsure if it declaring it on
     * the stack is a good idea.
     */
    int x_state [npar];
    for (int i = 0; i < df->nobs; ++i) {
        int y_state = y[i];
        for (int j = 0; j < npar; ++j)
            x_state[j] = data[j][i];
        /* convert the macro state of x into an index (i.e. k) for n_jk */
        int k = 0;
        for (int j = 0; j < npar; ++j) {
            k *= x_states[j];
            k += x_state[j];
        }
        /* increment the observed microstate (x,y) by 1 in n_jk */
        n_jk[k * n_y_states + y_state]++;
        /* increment observed microstate (y) by 1 in n_j */
        n_j[k]++;
    }
    int nvar     = df->nvar;
    double score = npar * log(structure_prior/(nvar - 1))
                    + (nvar - npar) * log(1.0f - structure_prior/(nvar - 1));
    double cell_prior = sample_prior / (n_x_states * n_y_states);
    double row_prior  = sample_prior / n_x_states;
    score += n_x_states * lgamma(row_prior);
    score -= n_y_states * n_x_states * lgamma(cell_prior);
    for(int i = 0; i < n_x_states; ++i) {
        score -= lgamma(row_prior + n_j[i]);
        for(int j = 0; j < n_y_states; ++j)
            score += lgamma(cell_prior + n_jk[j + i * n_y_states]);
    }
    free(alloced_mem);
    return score;
}
