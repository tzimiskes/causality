#include <causality.h>
#include <bdeu_score.h>

#ifdef _OPENMP
#include <omp.h>
#endif

static inline double calc_structure_prior(int n_par, int n_obs, int structure_prior);

double bdue_score(void ** xy_df, int * dims, int n_par, int n_obs) {
  double sample_prior    = 1.0f;
  double structure_prior = 1.0f;
  /* create aliases for xy_df so we don't have to keep casting the pointers */
  int * x     = xy_df[0];
  int ** y_df = xy_df[1];
  int   x_dim = dims[0];
  /* creating an alias for dims simplifies indexing here and
   * later on in the function */
  int * y_dims = dims + 1;
  int   y_dim  = 1;
  for(int i = 0; i < n_par; ++i)
    y_dim *= y_dims[i];
  /* this needs to be a calloc call instead of malloc */
  int * alloced_mem = calloc((x_dim + 1) * y_dim, sizeof(int));
  int * n_jk = alloced_mem;
  int * n_j  = alloced_mem + x_dim * y_dim;
  /* unsure if y_vals should be allocated on the heap or stack */
  int y_vals [n_par];
  for(int i = 0; i < n_obs; ++i) {
    int x_val = x[i];
    for(int j = 0; j < n_par; ++j)
      y_vals[j] = y_df[j][i];
    int k = 0;
    for (int k = 0; k < n_par; ++k) {
      k *= y_dims[i];
      k += y_vals[i];
    }
    n_jk[k * x_dim + x_val]++;
    n_j[k]++;
  }

  double cell_prior = sample_prior / (x_dim * y_dim);
  double row_prior  = sample_prior / y_dim;
  double score = calc_structure_prior(n_par, n_obs, structure_prior);
  for(int i = 0; i < y_dim; ++i) {
    score -= lgamma(row_prior + n_j[i]);
    for(int j = 0; j < x_dim; ++j)
      score += lgamma(cell_prior + n_jk[j + i*y_dim]);
  }
  score += y_dim * lgamma(row_prior);
  score -= x_dim * y_dim * lgamma(cell_prior);
  free(alloced_mem);
  return score;
}

static inline double calc_structure_prior(int n_par, int n_obs, int structure_prior) {
  return n_par * log(structure_prior/(n_obs - 1)) + (n_obs - n_par) *
           log(1.0f - (structure_prior/(n_obs - 1)));
}
