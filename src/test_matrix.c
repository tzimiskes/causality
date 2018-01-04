#include<R.h>
#include<Rinternals.h>
#include<string.h>

#define NCOL 3
SEXP c_topological_order(SEXP edges) {
  R_xlen_t n = nrows(edges);

SEXP children = PROTECT(allocVector(VECSXP, n));


  return(ScalarReal(1));
}

void visit() {}