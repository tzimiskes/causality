#include"headers/rapi.h"

#include<math.h>
#include<stdlib.h>
#include<stdio.h>

SEXP c_pearson_correlation(SEXP x, SEXP y);
SEXP c_partial_correlation(SEXP x, SEXP y, SEXP z);

SEXP c_nth_order_partial_correlation(SEXP x, SEXP y, SEXP z) {
  R_xlen_t n_obs = nrows(z);
  R_xlen_t n_vars = ncols(z);

  if(n_vars == 0)
    return(c_pearson_correlation(x, y));
  else if( n_vars == 1)
    return(c_partial_correlation(x, y, z));

  return(ScalarReal(0));


}