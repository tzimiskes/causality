#include <causality.h>
#include <dataframe.h>
#include <scores.h>

#include <benchmarkr.h>

const char * BIC_SCORE  = "BIC";
const char * BDEU_SCORE = "BDeu";

double score_graph(cgraph_ptr cg_ptr, dataframe d, double * fargs, int * iargs,
  double (* score_fp)(dataframe, int *, int, double *, int *));

SEXP ccf_score_graph_wrapper(SEXP Graph, SEXP Df, SEXP ScoreType,
  SEXP States, SEXP FloatingArgs, SEXP IntegerArgs)
  {
  int * edges         = calculate_edges_ptr(Graph);
  int n_nodes         = length(VECTOR_ELT(Graph, NODES));
  int n_edges         = nrows(VECTOR_ELT(Graph, EDGES));
  cgraph_ptr cg_ptr   = create_cgraph(n_nodes);
  fill_in_cgraph(cg_ptr, n_edges, edges);
  free(edges);
  /* calcluate the integer arguments and floating point arguments for the
   * score function. */
  int * iargs;
  if(!isNull(IntegerArgs))
    iargs = INTEGER(IntegerArgs);
  else
    iargs = NULL;
  double * fargs;
  if(!isNull(FloatingArgs))
    fargs = REAL(FloatingArgs);
  else
    fargs = NULL;
  /* states stores the number of states (in the sense of degrees of freedom)
   * of each variable in the data frame with the caveat that for continuous
   * variables, the number of states is considered to be to be 0 instead of 1.
   * This allows me to use the actual value in each entry of dims to determine
   * the type (real or discrete/integer) of the variable in the data frame.
   * Instead, we store the columns as void pointers in df. This helps divorce C
   * and R so it is easier to port this package to python, julia, etc. */
  dataframe df;
  df.n_var  = length(Df);
  df.n_obs  = length(VECTOR_ELT(Df, 0));
  df.states = INTEGER(States);
  df.df     = malloc(df.n_var * sizeof(void *));
  /* populate df with the pointers to the columns of the R dataframe */
  int * states = df.states;
  for(int i = 0; i < df.n_var; ++i) {
    if(states[i])
      df.df[i] = INTEGER(VECTOR_ELT(Df, i));
    else
      df.df[i] = REAL(VECTOR_ELT(Df, i));
  }

  double (* score_fp)(dataframe, int *, int, double *, int *);
  if(!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
    score_fp = bic_score;
  else if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BDEU_SCORE))
    score_fp = bdeu_score;
  else
    error("nope\n");
  double score = score_graph(cg_ptr, df, fargs, iargs, score_fp);

  free(df.df);
  free_cgraph(cg_ptr);
  return(ScalarReal(score));
}

double score_graph(cgraph_ptr cg_ptr, dataframe df, double * fargs, int * iargs,
  double (* score_fp)(dataframe, int *, int, double *, int *))
{
  double    score   = 0.0f;
  int       n_nodes = cg_ptr->n_nodes;
  ill_ptr * parents = cg_ptr->parents;
  for(int i = 0; i < n_nodes; ++i) {
    ill_ptr p = parents[i];
    if(parents[i]) {
      int n_par = ill_size(p);
      int xy[n_par + 1];
      xy[0] = i;
      int j = 1;
      Rprintf("scoring node %i, which has parents", i);
      while(p) {
        Rprintf("%i", p->key);
        xy[j] = p->key;
        j++;
        p = p->next;
      }
      Rprintf("\n");
      score += score_fp(df, xy, n_par, fargs, iargs);
    }
  }
  Rprintf("total score: %f\n", score);
  return score;
}
/* score diff calculates the difference in BIC scores between two configurations
* new and old */
double score_diff(dataframe df,
                int * new_xy,
                int * old_xy,
                int new_n_par,
                int old_n_par,
                double * fargs,
                int * iargs,
                double (* score_fp)(dataframe, int *, int, double *, int *)
              )
{
  if(old_n_par == 0)
    return score_fp(df, new_xy, new_n_par, fargs, iargs);
  else
    return score_fp(df, new_xy, new_n_par, fargs, iargs)
             - score_fp(df, old_xy, old_n_par, fargs, iargs);
}
