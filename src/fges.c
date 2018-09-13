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
cgraph  *ccf_fges(dataframe df, score_func score, double * fargs, int * iargs);

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

    cgraph *cg = ccf_fges(df, score, fargs, iargs);
    free_cgraph(cg);
    free(df.df);
    return ScalarReal(0);
}


int is_clique(cgraph *cg, int * nodes, int n_nodes)
{
    for(int i = 0; i < n_nodes; ++i) {
        int inode = nodes[i];
        for(int j = 0; j < i; ++j) {
            if (i >=0  && j >= 0) {
                if(!adjacent_in_cgraph(cg, inode, nodes[j]))
                    return 0;
            }
        }
    }
    return 1;
}

int no_unblocked_semidirected_path(cgraph *cg, int x, int y, int * seps,
                                                   int seps_size)
{
    ill *queue = ill_insert(NULL, x, 0);
    ill *v     = ill_insert(NULL, x, 0);
    while(queue) {
        int node = queue->key;
        ill *tmp = queue;
        queue = queue->next;
        free(tmp);

        ill *p = cg->spouses[node];
        while(p) {
            int spouse = p->key;
            for(int i = 0; i < seps_size; ++i) {
                if(spouse == seps[i])
                    goto NEXT_SPOUSE;
            }
            if(spouse == y) {
                ill_free(queue);
                ill_free(v);
                return 0;
            }
            if(!ill_search(v, spouse)) {
                queue = ill_insert(queue, spouse, 0);
                v     = ill_insert(v, spouse, 0);
            }
            NEXT_SPOUSE:
            p = p->next;
        }
        p = cg->children[node];
        while(p) {
            int child = p->key;
            for(int i = 0; i < seps_size; ++i) {
                if(child == seps[i])
                    goto NEXT_CHILD;
            }
            if(child == y) {
                ill_free(queue);
                ill_free(v);
                return 0;
            }
            if(!ill_search(v, child)) {
                queue = ill_insert(queue, child, 0);
                v     = ill_insert(v, child, 0);
            }
            NEXT_CHILD:
            p = p->next;
        }
    }
    ill_free(queue);
    ill_free(v);
    return 1;
}

double score_powerset(cgraph *cg, dataframe data, ges_record g,
                                score_func score, double *fargs, int *iargs)
{
    double dscore = DBL_MAX;
    /* saute in butter for best results */
    int *onion = malloc((g.naxy_size + g.set_size) * sizeof(int));
    for(int i = 0; i < g.naxy_size; ++i)
        onion[i] = g.naxy[i];
    uint64_t n = 1 << g.set_size;
    for(int i = 0; i < n; ++i) {
        int onion_size = 0;
        for(uint32_t j = 0; j <  (uint32_t) g.set_size; ++j) {
            if((i & (1 << j)) == (1 << j)) {
                onion[g.naxy_size + onion_size] = g.set[onion_size];
                onion_size++;
            }
        }
        onion_size += g.naxy_size;
        if(is_clique(cg, onion, onion_size) &&
           no_unblocked_semidirected_path(cg, g.y,g.y, onion, onion_size)) {
            ill *parents = cg->parents[g.y];
            int new_npar = onion_size + ill_size(parents);
            int *xy = malloc((new_npar + 2) * sizeof(int));
            xy[0]        = g.x;
            xy[new_npar + 1] = g.y;
            int j;
            for(j = 0; j < onion_size; ++j)
                xy[1 + j] = onion[j];
            while(parents) {
                xy[j++] = parents->key;
                parents = parents->next;
            }
            double ds = score_diff(data, xy, xy + 1, new_npar, new_npar - 1,
                                             fargs, iargs, score);
            if(ds < dscore) {
                ds  = dscore;
                g.set_size = onion_size - g.naxy_size;
                free(g.set);
                g.set = malloc(g.set_size * sizeof(int));
                for(int j = 0; j < g.set_size; ++j) {
                    g.set[j] = onion[j + g.naxy_size];
                }
            }
            free(xy);
        }
    }
    free(onion);
    return dscore;
}



static void insert(cgraph *cg, ges_record gesrec)
{
    add_edge_to_cgraph(cg, gesrec.x, gesrec.y, DIRECTED);
    for(int i = 0; i < gesrec.set_size; ++i)
        orient_undirected_edge(cg, gesrec.set[i], gesrec.y);
}

double recalcluate_node(dataframe df, cgraph *cg, ges_record *gesrecp,
                                         score_func score, double *fargs,
                                         int *iargs)
{
    double     dscore = 0.0f;
    double min_dscore = DBL_MAX;
    int y = gesrecp->y;
    for(int x = 0; x < df.nvar; ++x) {
        if(x == y || adjacent_in_cgraph(cg, x, y))
            continue;
        ges_record g = {0};
        g.x = x;
        g.y = y;
        ill *l = cg->spouses[y];
        while(l) {
            if(adjacent_in_cgraph(cg, x, l->key))
                g.naxy_size++;
            else
                g.set_size++;
            l = l->next;
        }
        g.naxy = malloc(g.naxy_size * sizeof(int));
        g.set  = malloc(g.set_size * sizeof(int));
        /* we have to reiterate through the list */
        l = cg->spouses[y];
        int j = 0;
        int k = 0;
        while(l) {
            int z = l->key;
            if(adjacent_in_cgraph(cg, x, z))
                g.naxy[j++] = z;
            else
                g.set[k++]  = z;
            l = l->next;
        }
        dscore = score_powerset(cg, df, g, score, fargs, iargs);
        if(dscore < min_dscore) {
            free(gesrecp->set);
            free(gesrecp->naxy);
            *gesrecp = g;
        }
        else {
            free(g.set);
            free(g.naxy);
        }
    }
    return dscore;
}


static void delete(cgraph *cg, ges_record gesrec)
{
    delete_edge_from_cgraph(cg, gesrec.x, gesrec.y, DIRECTED);
    for(int i = 0; i < gesrec.set_size; ++i) {
        orient_undirected_edge(cg, gesrec.x, gesrec.set[i]);
        orient_undirected_edge(cg, gesrec.y, gesrec.set[i]);
    }
}


cgraph *ccf_fges(dataframe df, score_func score,
                                        double *fargs, int *iargs)
{
    cgraph *cg         = create_cgraph(df.nvar);
    double graph_score = 0.0f;
    double dscore      = 0.0f;
    Rprintf("hello!\n");
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
        dscores[i]      = min;
        gesrecords[i].x = arg_min;
        gesrecords[i].y = i;
    }
    build_heap(queue);
    Rprintf("here\n");
    ges_record * gesrecp;
    while((gesrecp = extract_heap(queue, &dscore)) && dscore <= 0) {
        graph_score += dscore;
        Rprintf("insert\n");
        insert(cg, *gesrecp);
        /* revert the now incomplete PDAG to a pattern */
        /* COULD BE SPED UP */
        Rprintf("reorient\n");
        ccf_chickering(cg = ccf_pdx(cg));
        Rprintf("rescore\n");
        dscore = recalcluate_node(df, cg, gesrecp, score,  fargs, iargs);
        Rprintf("put in heap\n");
        insert_heap(queue, dscore, gesrecp, gesrecp - gesrecords);

        Rprintf("graph_score %f\n", graph_score);
        if(0) {
            delete(cg, *gesrecp);
        }
    }

  /* step 2 prune
  ???
  */
    for(int i = 0; i < df.nvar; ++i) {
        free(gesrecords[i].set);
        free(gesrecords[i].naxy);
    }
    free(gesrecords);
    print_cgraph(cg);
    free_heap(queue);
    Rprintf("fges complete\n");
    return cg;
}
