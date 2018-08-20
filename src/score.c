#include <causality.h>

const char * BIC_SCORE  = "BIC";
const char * BDEU_SCORE = "BDeu";

double score_graph(cgraph_ptr cg_ptr, int * dims,void ** df, int n_var, int n_obs);

SEXP ccf_score_graph_wrapper(SEXP Graph, SEXP Df, SEXP ScoreType, SEXP Dims) {
  int * edges         = calculate_edges_ptr(Graph);
  int n_nodes         = length(VECTOR_ELT(Graph, NODES));
  int n_edges         = nrows(VECTOR_ELT(Graph, EDGES));
  cgraph_ptr cg_ptr   = create_cgraph(n_nodes);
  fill_in_cgraph(cg_ptr, n_edges, edges);
  free(edges);

  /* dims stores the dimensions (in the sense of degrees of freedom) of each
   * variable in the data frame. For continuous variables, this is considered
   * to be zero instead of 1, as this allows me to use the actual value in each
   * entry of dims to determine the type (real or discrete/integer) of the
   * variable in the data frame. This helps divorce C and R. */
  int n_var  = ncols(Df);
  int n_obs  = nrows(Df);
  int * dims = INTEGER(Dims);
  void ** df = malloc(n_var * sizeof(void *));
  for(int i = 0; i < n_var; ++i) {
    if(dims[i])
      df[i] = INTEGER(VECTOR_ELT(Df, i));
    else
      df[i] = REAL(VECTOR_ELT(Df, i));
  }
  double score = 0;
  if(!strcmp(CHAR(ScoreType), BIC_SCORE))
    score = score_graph(cg_ptr, dims, df, n_var, n_obs);
  else if (!strcmp(CHAR(ScoreType), BDEU_SCORE))
    score = 0;
  free(df);
  free_cgraph(cg_ptr);
  return(ScalarReal(score));
}

double score_graph(cgraph_ptr cg_ptr, int * dims, void ** df, int n_var, int n_obs) {
  return 0;
}

double score_configuration(cgraph_ptr cg_ptr, int * dims, void ** df, int n_var, int n_obs) {
  return 0;
}
