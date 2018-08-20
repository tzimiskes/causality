#include <causality.h>

#ifdef _OPENMP
#include <omp.h>
#endif

double bdeu_score(int * node, int ** parents, int * dims, int n_parents, int n_obs) {
  double sample_prior    = 1.0f;
  double structure_prior = 1.0f;

  int n_states = 1;
  for(int i = 1; i <= n_parents; ++i)
    n_states *= dims[i];
}
