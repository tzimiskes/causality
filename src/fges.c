#include <causality.h>
#include <cgraph.h>
#include <dataframe.h>
#include <scores.h>

void ccf_fges(cgraph_ptr cg_ptr,
         dataframe df,
         double (* score_fp)(dataframe, int *, int, double *, int *),
         double * fargs,
         int * iargs);

SEXP ccf_fges_wrapper(SEXP Df, SEXP ScoreType,
  SEXP States, SEXP FloatingArgs, SEXP IntegerArgs)
{
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
  /* All the preprocessing work has now been done, so lets instantiate
   * an empty graph and run FGES */
  cgraph_ptr cg_ptr = create_cgraph(df.n_var);
  ccf_fges(cg_ptr, df, score_fp, fargs, iargs);
  free(df.df);

  return ScalarReal(0);
}

void ccf_fges(cgraph_ptr cg_ptr,
         dataframe df,
         double (* score_fp)(dataframe, int *, int, double *, int *),
         double * fargs,
         int * iargs)
{
  /* STEP 0 score all y --> x and form a data strucure that will be used */

  /* build phase
  * while buff !is.empty()
  * a pop an edge
  * b (if legal) add edge
  *   i recalcluate scores for all other possible parents
  *    1 resort i in data strucure
  *   ii recalcluate reversion scores for children
  *    1 resort ii in data struture */


  /* step 2 prune
  ???
  */





}
