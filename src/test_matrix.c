#include<R.h>
#include<Rinternals.h>
#include<stdlib.h>


#define NCOL 3

//void visit(R_xlen_t i, int** hash_table, int* order);
SEXP c_topological_order(SEXP dag) {
  R_xlen_t n = length(VECTOR_ELT(dag, 0));
  R_xlen_t n_edges = nrows(VECTOR_ELT(dag, 2));

  SEXP skeleton = PROTECT(VECTOR_ELT(dag, 1));

  /*
  int** hash_table;

  R_xlen_t n_marked = 0;
  int* marked = calloc(0, n*sizeof(int));
  int* order = calloc(0, n*sizeof(int));
  R_xlen_t i = 0;
  while(n_marked < n_edges) {
    if(marked[i] == 0)
      visit(i, hash_table, order);
    else
      ++i;
  }

  free(marked);
  free(order);
   */
  UNPROTECT(1);
  return(VECTOR_ELT(skeleton, "X1"));
}
