#include <causality.h>
#include <cgraph.h>
#include <heap.h>
#include <ges_record.h>
#include <dataframe.h>
#include <scores.h>
#include <pdx.h>
#include <chickering.h>
#include <edgetypes.h>

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
  free_cgraph(cg_ptr);
  free(df.df);
  return ScalarReal(0);
}


int is_clique(cgraph_ptr cg_ptr, int * nodes, int n_nodes) {
  for(int i = 0; i < n_nodes; ++i) {
    int inode = nodes[i];
    for(int j = 0; j < i; ++j) {
      if(!adjacent_in_cgraph(cg_ptr, inode, nodes[j]))
        return 0;
    }
  }
  return 1;
}

static inline void ges_insert(cgraph_ptr cg_ptr, ges_record gesrec) {
  add_edge_to_cgraph(cg_ptr, gesrec.parent, gesrec.child, DIRECTED);
  for(int i = 0; i < gesrec.n_s; ++i)
    orient_undirected_edge(cg_ptr, gesrec.s[i], gesrec.child);
}

void rescore_node(cgraph_ptr cg_ptr,
         dataframe df,
         ges_record * gesrecp,
         double (* score_fp)(dataframe, int *, int, double *, int *),
         double * fargs,
         int * iargs)
{
  int child = gesrecp->child;

  ges_record * gesrecs    = calloc(df.n_var, sizeof(ges_record));
  heap       * rescore_hp = create_heap(df.n_var);
  for(int i = 0; i < df.n_var; ++i) {
    if(i != child && !adjacent_in_cgraph(cg_ptr, i, child)) {
      ill_ptr na_xy   = NULL;
      ill_ptr s       = NULL;
      ill_ptr tmp     = cg_ptr->spouses[child];
      int     n_s     = 0;
      int     n_na_xy = 0;
      while(tmp) {
        if(!adjacent_in_cgraph(cg_ptr, i, tmp->key)) {
          s = ill_insert(s, tmp->key, 0);
          n_s++;
        }
        else {
          na_xy = ill_insert(na_xy, tmp->key, 0);
          n_na_xy++;
        }
        tmp = tmp->next;
      }

    }
  }
}




static inline void ges_delete(cgraph_ptr cg_ptr, ges_record gesrec) {
  delete_edge_from_cgraph(cg_ptr, gesrec.parent, gesrec.child, DIRECTED);
  for(int i = 0; i < gesrec.n_s; ++i) {
    orient_undirected_edge(cg_ptr, gesrec.parent, gesrec.s[i]);
    orient_undirected_edge(cg_ptr, gesrec.child, gesrec.s[i]);
  }
}



void ccf_fges(cgraph_ptr cg_ptr,
         dataframe df,
         double (* score_fp)(dataframe, int *, int, double *, int *),
         double * fargs,
         int * iargs)
{
  double score  = 0.0f;
  double dscore = 0.0f;
  /* We need to set up the priority queue so we know which edge to add
   * (and the other relevant information) at each stage of fges. Roughly how
   * this works is that each the highest scoring edge incident in each node is
   * recorded and then we find the highest scoring edge of all of those by
   * using the heap data structure we have */
  ges_record * gesrecords = calloc(df.n_var, sizeof(ges_record));
  heap *       queue       = create_heap(df.n_var);
  double *     dscores     = queue->dscores;
  void **      records     = queue->records;
  int  *       indices     = queue->indices;
  for(int i = 0; i < df.n_var; ++i) {
    records[i] = gesrecords + i;
    indices[i] = i;
  }
  /* step 0 score each edge y --> x. only check score if index(x) < index(y) */
  for(int i = 0; i < df.n_var; ++i) {
    double min     = DBL_MAX;
    int    arg_min = -1;
    int    xy[2]   = {i, 0};
    for(int j = 0; j < i; ++j) {
      xy[1]     = j;
      double ds = score_diff(df, xy, NULL, 1, 0, fargs, iargs, score_fp);
      if(ds < min) {
        min     = ds;
        arg_min = j;
      }
    }
    Rprintf("dscore %f\n", min);
    dscores[i]            = min;
    gesrecords[i].parent = arg_min;
    gesrecords[i].child  = i;
  }
  build_heap(queue);
  ges_record * gesrecp;
  while((gesrecp = extract_heap(queue, &dscore)) && dscore <= 0) {
    score += dscore;
    ges_insert(cg_ptr, *gesrecp);

    rescore_node(cg_ptr, df, gesrecp, score_fp, fargs, iargs);
    insert_heap(queue, dscore, gesrecp, gesrecp - gesrecords);
    /* revert the now incomplete PDAG to a pattern */
    /* COULD BE SPED UP */
    ccf_chickering(cg_ptr = ccf_pdx(cg_ptr));



    if(0) {
      ges_delete(cg_ptr, *gesrecp);
    }
  }

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

  free(gesrecords);
  print_cgraph(cg_ptr);
  free_heap(queue);
}
