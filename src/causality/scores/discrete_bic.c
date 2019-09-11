#include <stdlib.h>
#include <math.h>

#include <dataframe.h>
#include <causality.h>
#include <scores/scores.h>

#define EPSILON 1e-6


double discrete_bic_score(struct dataframe *df, int *xy, int npar,
                            struct score_args *args)
{
    double penalty = args->fargs[0];

    int *data[npar + 1];

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
    for (int i = 0; i < npar; ++i)
        n_x_states *= x_states[i];

    /*
     * Create a matrix that will store the frequencies of the microstate (x, y).
     * The matrix will populated in row major format.
     */
    int *alloced_mem = calloc(n_x_states * (n_y_states + 1), sizeof(int));
    if (alloced_mem == NULL) {
        CAUSALITY_ERROR("Failed to allocate enough memory for discrete_bic,\
        memory requested: %i %i \n", n_x_states, n_y_states);
    }

    int *n_jk = alloced_mem;
    int *n_j  = alloced_mem + n_x_states * n_y_states;
    for (int i = 0; i < df->nobs; ++i) {
        /* convert the state of x into an index (i.e. k) for n_jk */
        int k = 0;
        for (int j = 0; j < npar; ++j) {

            k = k * x_states[j] + data[j][i];
        }

        /* increment the observed microstate (x,y) by 1 in n_jk */
        n_jk[k * n_y_states + y[i]]++;
        /* increment observed microstate (y) by 1 in n_j */
        n_j[k]++;
    }

    double lik = 0.0;
    for (int j = 0; j < n_x_states; ++j)
        for (int k = 0; k < n_y_states; ++k)
            if (n_jk[j * n_y_states + k])
                lik += n_jk[j * n_y_states + k ] *
                         log(n_jk[j * n_y_states + k] / (double) n_j[j]);

    double params = n_x_states * (n_y_states - 1);
    free(alloced_mem);

    return -2.0 * lik + penalty * params * log(df->nobs) + EPSILON;
}
