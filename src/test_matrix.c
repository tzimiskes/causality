#include<R.h>
#include<Rinternals.h>

SEXP test_matrix(SEXP x, SEXP y, SEXP z) {
  int n = length(x);
  int n_cols = ncols(z);
  double r_hat;

  return(ScalarReal(r_hat));
}