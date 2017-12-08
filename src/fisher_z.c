#include<R.h>
#include<Rinternals.h>

SEXP c_fisher_transformation_z_score(SEXP rho, SEXP n) {
  double r = asReal(rho);
  double fr = log((1 + r)/(1 - r))/2;
  // H0 rho = 0
  double z = (fr - 0) * sqrt(asReal(n) - 3);
  return(ScalarReal(z));
}
SEXP c_is_independent(SEXP z_score, SEXP alpha) {
// H0 rho =0
  double a = asReal(alpha);

  double p = 1 + erf((0 - asReal(z_score)) * M_SQRT1_2);
  // fail to reject
  if(a < p)
    return(ScalarLogical(1));
  else
    return(ScalarLogical(0));
}