#include"headers/causality_stdlib.h"

SEXP c_pearson_correlation(SEXP x, SEXP y) {
  // get the length of the vectors;
  // int is 32 bit in R. R_xlen_t appears to be 64
  R_xlen_t n = length(x);
  // create a copy of x/y data on the heap and protect it from the R garbageman
  double* restrict x_dup_ptr = REAL(PROTECT(duplicate(x)));
  double* restrict y_dup_ptr = REAL(PROTECT(duplicate(y)));
 // calculate the means of x and y
  double mu_x = 0;
  double mu_y = 0;
  for(R_xlen_t i = 0; i < n; ++i) {
    mu_x += x_dup_ptr[i];
    mu_y += y_dup_ptr[i];
  }
  mu_x = mu_x/n;
  mu_y = mu_y/n;
  // subtract the means from the duplicates
  for (R_xlen_t i = 0; i < n; ++i) {
    x_dup_ptr[i] -= mu_x;
    y_dup_ptr[i] -= mu_y;
  }
  // calculate the standard deviations and numerator
  double sd_x  = 0;
  double sd_y  = 0;
  double numer = 0;
  for (R_xlen_t i = 0; i < n; ++i) {
    sd_x   += x_dup_ptr[i] * x_dup_ptr[i];
    sd_y   += y_dup_ptr[i] * y_dup_ptr[i];
    numer  += x_dup_ptr[i] * y_dup_ptr[i];
  }
  // unprotect the dup_ptrs so the garbageman can take them away
  UNPROTECT(2);
  // check to see if sds might indicate vectors are *too* constant
  if (sd_x < 1e-11)
    error("x standard deviation too small! sd(x) < 1e-11");
  if (sd_y < 1e-11)
    error("y standard deviation too small! sd(y) < 1e-11");
   return(ScalarReal(numer /sqrt(sd_x * sd_y)));
}

SEXP c_partial_correlation(SEXP x, SEXP y, SEXP z) {
  double rho_xy = asReal(c_pearson_correlation(x, y));
  double rho_xz = asReal(c_pearson_correlation(x, z));
  double rho_yz = asReal(c_pearson_correlation(y, z));
// caclulate the partial correlation...
  double rho_xy_g_z = (rho_xy - rho_xz * rho_yz)/
    sqrt((1 - rho_xz * rho_xz)*(1 - rho_yz * rho_yz));

  return(ScalarReal(rho_xy_g_z));
}


