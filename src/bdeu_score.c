#include <causality.h>
#include <bdeu_score.h>

#ifdef _OPENMP
#include <omp.h>
#endif

static inline double calc_structure_prior(int n_par, int n_obs, int structure_prior);

double bdue_score(void ** xy_df, int * dims, int n_par, int n_obs) {
  double sample_prior    = 1.0f;
  double structure_prior = 1.0f;

  int node_dim     = dims[0];
  int parents_dim  = 1;
  for(int i = 1; i <= n_par; ++i)
    parents_dim *= dims[i];

  int * alloc_mem = calloc((node_dim + 1) * parents_dim, sizeof(int));
  int * n_jk = alloc_mem;
  int * n_j  = alloc_mem + node_dim*parents_dim;

  for(int i = 0; i < n_obs; ++i) {
/* HORRID */
  }

  double cell_prior = sample_prior / (node_dim * parents_dim);
  double row_prior = sample_prior / parents_dim;
  double score = calc_structure_prior(n_par, n_obs, structure_prior);
  for(int i = 0; i < parents_dim; ++i) {
    score -= lgamma(row_prior + n_j[i]);
    for(int j = 0; j < node_dim; ++j)
      score += lgamma(cell_prior + n_jk[j + i*parents_dim]);
  }
    score += parents_dim * lgamma(row_prior);
    score -= node_dim * parents_dim * lgamma(cell_prior);
  free(alloc_mem);

  return score;
}

static inline double calc_structure_prior(int n_par, int n_obs, int structure_prior) {
  return n_par * log(structure_prior/(n_obs - 1)) + (n_obs - n_par) *
           log(1.0f - (structure_prior/(n_obs - 1)));
}
