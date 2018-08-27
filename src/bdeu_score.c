#include <dataframe.h>
#include <causality.h>
#include <scores.h>



static inline double calc_structure_prior(int n_par, int n_var, double structure_prior);

double bdeu_score(dataframe data, int * xy, int n_par, double * fargs, int * iargs) {
  double sample_prior    = fargs[0];
  double structure_prior = calc_structure_prior(n_par, data.n_var, fargs[1]);
  int    n_obs           = data.n_obs;
  int * df[n_par + 1];
  for(int i = 0; i < n_par + 1; ++i)
    df[i] = data.df[xy[i]];
  int * x          = df[0];
  int   n_x_states = data.states[xy[0]];
  int   y_states[n_par];
  for(int i = 0; i < n_par; ++i)
    y_states[i]  = data.states[xy[i + 1]];
  /* now, we need to calcluate the total number of possible y states */
  int   n_y_states  = 1;
  for(int i = 0; i < n_par; ++i)
    n_y_states *= y_states[i];
  /* create a matrix that will store the frequencies of state (y, x).
   * The matrix will be indexed by row major format */
  int * alloced_mem = calloc((n_x_states + 1) * n_y_states, sizeof(int));
  int * n_jk = alloced_mem;
  int * n_j  = alloced_mem + n_x_states * n_y_states;
  /* unsure if y_vals should be allocated on the heap or stack */
  int y_state [n_par];
  for(int i = 0; i < n_obs; ++i) {
    int x_state = x[i];
    for(int j = 0; j < n_par; ++j) {
      y_state[j] = df[j + 1][i];
    }
    int k = 0;
    for (int j = 0; j < n_par; ++j) {
      k *= y_states[j];
      k += y_state[j];
    }
    n_jk[k * n_x_states + x_state]++;
    n_j[k]++;
  }
  double cell_prior = sample_prior / (n_x_states * n_y_states);
  double row_prior  = sample_prior / n_y_states;
  double score      = structure_prior;
  for(int i = 0; i < n_y_states; ++i) {
    score -= lgamma(row_prior + n_j[i]);
    for(int j = 0; j < n_x_states; ++j)
      score += lgamma(cell_prior + n_jk[j + i*n_x_states]);
  }
  score += n_y_states * lgamma(row_prior);
  score -= n_x_states * n_y_states * lgamma(cell_prior);
  free(alloced_mem);
  return score;
}

static inline double calc_structure_prior(int n_par, int n_var, double structure_prior) {
  return n_par * log(structure_prior/(n_var - 1)) + (n_var - n_par) *
           log(1.0f - (structure_prior/(n_var - 1)));
}
