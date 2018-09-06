#include <causality.h>
#include <cgraph.h>
#include <heap.h>
#include <ges_record.h>
#include <dataframe.h>
#include <scores.h>
#include <pdx.h>
#include <chickering.h>
#include <edgetypes.h>

#include <stdint.h>
void ccf_fges(cgraph_ptr cg_ptr, dataframe df, score_func score,
                                 double * fargs, int * iargs);

SEXP ccf_fges_wrapper(SEXP Df, SEXP ScoreType, SEXP States,
                               SEXP FloatingArgs, SEXP IntegerArgs)
{
    /*
     * calcluate the integer arguments and floating point arguments for the
     * score function.
     */
    int *iargs = NULL;
    if(!isNull(IntegerArgs))
        iargs = INTEGER(IntegerArgs);
    double *fargs = NULL;
    if(!isNull(FloatingArgs))
        fargs = REAL(FloatingArgs);
        dataframe df;
    df.nvar  = length(Df);
    df.nobs  = length(VECTOR_ELT(Df, 0));
    df.states = INTEGER(States);
    df.df     = malloc(df.nvar * sizeof(void *));
    /* populate df with the pointers to the columns of the R dataframe */
    int *states = df.states;
    for(int i = 0; i < df.nvar; ++i) {
        if(states[i])
            df.df[i] = INTEGER(VECTOR_ELT(Df, i));
        else
            df.df[i] = REAL(VECTOR_ELT(Df, i));
    }
    score_func score;
    if(!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
        score = bic_score;
    else if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BDEU_SCORE))
        score = bdeu_score;
    else
        error("nope\n");
    /*
     * All the preprocessing work has now been done, so lets instantiate
     * an empty graph and run FGES
     */
    cgraph_ptr cg_ptr = create_cgraph(df.nvar);
    ccf_fges(cg_ptr, df, score, fargs, iargs);
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

void calcluate_powerset(cgraph *cg, ges_record gesrec) {
    int naxy_size = gesrec.naxy_size;
    int set_size  = gesrec.set_size;

    int set[naxy_size + set_size];
    memcpy(set , gesrec.naxy, naxy_size * sizeof(naxy_size));
    uint64_t n = 1 << set_size;
    for(int i = 0; i < n; ++i) {
        for(int j = 0 ; i < set_size; ++j)
            set[naxy_size + j] = gesrec.set[i & (1 << j)];
        if (is_clique(cg, set, naxy_size + set_size)) {
            /* add to clique list */
        }
    }
}



static void insert(cgraph_ptr cg_ptr, ges_record gesrec)
{
    add_edge_to_cgraph(cg_ptr, gesrec.parent, gesrec.child, DIRECTED);
    for(int i = 0; i < gesrec.set_size; ++i)
        orient_undirected_edge(cg_ptr, gesrec.set[i], gesrec.child);
}

/*
static double score_insert(dataframe df, cgraph_ptr cg_ptr, ges_record gesrec,
                                         score_func score, double *fargs,
                                         int *iargs)
{
    ill_ptr par  = cg_ptr->parents[gesrec.child];
    int npar     = ill_size(par);
    int old_npar = gesrec.naxy_size + gesrec.set_size + npar;
    int new_npar = old_npar + 1;
    int xy[new_npar + 1];
    xy[0]  = gesrec.parent;
    memcpy(xy + 1             , gesrec.set   , gesrec.set_size * sizeof(int));
    memcpy(xy + 1 + gesrec.set_size, gesrec.naxy, gesrec.naxy_size * sizeof(int));
    for(int i = 1 + gesrec.set_size + gesrec.naxy_size; i < new_npar; ++i) {
        xy[i] = par->key;
        par   = par->next;
    }
    xy[new_npar] = gesrec.child;
    return score_diff(df, xy, xy + 1, new_npar, old_npar, fargs, iargs, score);
}

*/
static void delete(cgraph_ptr cg_ptr, ges_record gesrec)
{
    delete_edge_from_cgraph(cg_ptr, gesrec.parent, gesrec.child, DIRECTED);
    for(int i = 0; i < gesrec.set_size; ++i) {
        orient_undirected_edge(cg_ptr, gesrec.parent, gesrec.set[i]);
        orient_undirected_edge(cg_ptr, gesrec.child, gesrec.set[i]);
    }
}


void ccf_fges(cgraph_ptr cg_ptr, dataframe df, score_func score,
                                 double *fargs, int *iargs)
{
    double graph_score = 0.0f;
    double dscore      = 0.0f;
    /*
    * We need to set up the priority queue so we know which edge to add
    * (and the other relevant information) at each stage of fges. Roughly how
    * this works is that each the highest scoring edge incident in each node is
    * recorded and then we find the highest scoring edge of all of those by
    * using the heap data structure we have
    */
    ges_record *gesrecords = calloc(df.nvar, sizeof(ges_record));
    heap       *queue      = create_heap(df.nvar);
    double     *dscores    = queue->dscores;
    void      **records    = queue->records;
    int        *indices    = queue->indices;
    for(int i = 0; i < df.nvar; ++i) {
        records[i] = gesrecords + i;
        indices[i] = i;
    }
/* step 0 score each edge y --> x. only check score if index(x) < index(y) */
    for(int i = 0; i < df.nvar; ++i) {
        double min     = DBL_MAX;
        int    arg_min = -1;
        int    xy[2]   = {0, i};
        for(int j = 0; j < i; ++j) {
            xy[0]     = j;
            double ds = score_diff(df, xy, NULL, 1, 0, fargs, iargs, score);
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
        graph_score += dscore;
        insert(cg_ptr, *gesrecp);
        /* revert the now incomplete PDAG to a pattern */
        /* COULD BE SPED UP */
        ccf_chickering(cg_ptr = ccf_pdx(cg_ptr));

        insert_heap(queue, dscore, gesrecp, gesrecp - gesrecords);



        if(0) {
            delete(cg_ptr, *gesrecp);
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
