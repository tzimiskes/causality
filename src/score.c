#include <causality.h>

#include <bic_score.h>
#include <bdeu_score.h>

const char * BIC_SCORE  = "BIC";
const char * BDEU_SCORE = "BDeu";


double score_graph(cgraph_ptr cg_ptr, void ** df, int * dims, int n_obs,
  double (* score_fp)(void **, int *, int, int));
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
  int     n_var  = ncols(Df);
  int     n_obs  = nrows(Df);
  int *   dims   = INTEGER(Dims);
  void ** df     = malloc(n_var * sizeof(void *));
  /* populate df with the pointers to the columns of the R dataframe */
  for(int i = 0; i < n_var; ++i) {
    if(dims[i])
      df[i] = INTEGER(VECTOR_ELT(Df, i));
    else
      df[i] = REAL(VECTOR_ELT(Df, i));
  }

  double (* score_fp)(void **, int * dims, int n_par, int n_obs);

  if(!strcmp(CHAR(ScoreType), BIC_SCORE))
    score_fp = bic_score;
  else if (!strcmp(CHAR(ScoreType), BDEU_SCORE))
    score_fp = bdue_score;
  else
    error("nope\n");
  double score = score_graph(cg_ptr, df, dims, n_obs, score_fp);
  free(df);
  free_cgraph(cg_ptr);
  return(ScalarReal(score));
}

double score_graph(cgraph_ptr cg_ptr, void ** df, int * dims, int n_obs,
  double (* score_fp)(void **, int *, int, int))
{
  double    score   = 0.0f;
  int       n_nodes = cg_ptr->n_nodes;
  ill_ptr * parents = cg_ptr->parents;
  for(int i = 0; i < n_nodes; ++i) {
    ill_ptr p = parents[i];
    if(parents[i]) {
      int n_par = ill_size(p);
      void * xy_df   [n_par + 1];
      int    xy_dims [n_par + 1];
      int j = 1;
      xy_df[0]   = df[0];
      xy_dims[0] = dims[0];
      while(p) {
        xy_df[j]   = df[p->key];
        xy_dims[j] = dims[p->key];
        p = p->next;
      }
      score += score_fp(xy_df, xy_dims, n_par, n_obs);
    }
  }
  return score;
}
