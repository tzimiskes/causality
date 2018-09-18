#include <causality.h>
#include <cgraph.h>
#include <heap.h>
#include <dataframe.h>
#include <scores.h>
#include <pdx.h>
#include <chickering.h>
#include <edgetypes.h>
#include <stdint.h>

struct gesrec {
    int   x;
    int   y;
    int   set_size;
    int   naxy_size;
    int  *set;
    int  *naxy;
}; /*  32 bytes */

struct cgraph *ccf_fges(dataframe df, score_func score, double * fargs, int * iargs);

SEXP ccf_fges_wrapper(SEXP Df, SEXP ScoreType, SEXP States,
                               SEXP FloatingArgs, SEXP IntegerArgs)
{
    /*
     * calcluate the integer arguments and floating point arguments for the
     * score function.
     */
    int *iargs = NULL;
    if (!isNull(IntegerArgs))
        iargs = INTEGER(IntegerArgs);
    double *fargs = NULL;
    if (!isNull(FloatingArgs))
        fargs = REAL(FloatingArgs);
    dataframe df;
    df.nvar  = length(Df);
    df.nobs  = length(VECTOR_ELT(Df, 0));
    df.states = INTEGER(States);
    df.df     = malloc(df.nvar * sizeof(void *));
    /* populate df with the pointers to the columns of the R dataframe */
    int *states = df.states;
    for (int i = 0; i < df.nvar; ++i) {
        if (states[i])
            df.df[i] = INTEGER(VECTOR_ELT(Df, i));
        else
            df.df[i] = REAL(VECTOR_ELT(Df, i));
    }
    score_func score;
    if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BIC_SCORE))
        score = bic_score;
    else if (!strcmp(CHAR(STRING_ELT(ScoreType, 0)), BDEU_SCORE))
        score = bdeu_score;
    else
        error("nope\n");
    /*
     * All the preprocessing work has now been done, so lets instantiate
     * an empty graph and run FGES
     */
    struct cgraph *cg = ccf_fges(df, score, fargs, iargs);


    /* POST PROCESSING */
    free_cgraph(cg);
    free(df.df);
    return ScalarReal(0);
}

static int is_clique(struct cgraph *cg, int *nodes, int n_nodes)
{
    for (int i = 0; i < n_nodes; ++i) {
        int inode = nodes[i];
        for (int j = 0; j < i; ++j) {
            if (!adjacent_in_cgraph(cg, inode, nodes[j])) {
                return 0;
            }
        }
    }
    return 1;
}

int unblocked_semidirected_path(struct cgraph *cg, int src, int dst, int *onion,
                                            int onion_size)
{

    int unblocked_path = 0;
    struct ill *stack  = ill_insert_front(NULL, src, 0);
    int *marked = calloc(cg->n_nodes, sizeof(int));
    if (marked == NULL)
        error("Failed to allocate memory for marked in fges!\n");
    while (stack) {
        /* pop */
        int node = stack->key;
        struct ill *tmp = stack->next;
        free(stack);
        stack = tmp;
        /* check to if T \cup NAXY block the path. */
        for (int i = 0; i < onion_size; ++i) {
            if (node == onion[i])
                goto NEXT;
        }
        /* If node == dst, we have a cycle */
        if (node == dst) {
            unblocked_path = 1;
            goto CLEANUP;
        }
        /*
         * if we haven't visited the node before, we need to push all of its
         * spouses and children onto the stack and mark it
         */
        if (!marked[node]) {
            marked[node] = 1;
            struct ill *p = cg->spouses[node];
            while (p) {
                stack = ill_insert_front(stack, p->key, 0);
                p     = p->next;
            }
            p = cg->children[node];
            while (p) {
                stack = ill_insert_front(stack, p->key, 0);
                p     = p->next;
            }
        }
        NEXT: ;
    }
    CLEANUP: ;
    if (stack)
        ill_free(stack);
    free(marked);
    return unblocked_path;
}

int is_valid_insertion(struct cgraph *cg, struct gesrec g, int *onion, int onion_size)
{
    if (is_clique(cg, onion, onion_size)) {
        if (!unblocked_semidirected_path(cg, g.y, g.x, onion, onion_size))
            return 1;
    }
    return 0;
}

struct gesrec score_powerset(struct cgraph *cg, dataframe data, struct gesrec g,
                                         double *dscore, score_func score,
                                         double *fargs, int *iargs)
{
    double min_ds = DBL_MAX;
    struct gesrec min_g;
    min_g.x         = g.x;
    min_g.y         = g.y;
    min_g.set_size  = 0;
    min_g.naxy_size = g.naxy_size;
    min_g.set       = NULL;
    min_g.naxy      = malloc(min_g.naxy_size * sizeof(int));
    if (min_g.naxy == NULL)
        error("failed to allocate memory for naxy in fges!\n");
    memcpy(min_g.naxy, g.naxy, min_g.naxy_size * sizeof(int));

    // if (!is_clique(cg, min_g.naxy, min_g.naxy_size)) {
    //     Rprintf("got ya!\n");
    //     *dscore = min_ds;
    //     return min_g;
    // }

    /* saute in butter for best results */
    int *onion = malloc((g.naxy_size + g.set_size) * sizeof(int));
    if (onion == NULL)
        error("failed to allocate memory for onion in fges!\n");
    for (int i = 0; i < g.naxy_size; ++i)
        onion[i] = g.naxy[i];

    uint64_t n = 1 << g.set_size;
    for (int i = 0; i < n; ++i) {
        int onion_size = 0;
        for (uint32_t j = 0; j <  (uint32_t) g.set_size; ++j) {
            if ((i & (1 << j)) == (1 << j)) {
                onion[g.naxy_size + onion_size] = g.set[onion_size];
                onion_size++;
            }
        }
        onion_size += g.naxy_size;
        if (is_valid_insertion(cg, g, onion, onion_size)) {
            struct ill *parents = cg->parents[g.y];
            int new_npar = onion_size + ill_size(parents) + 1;
            int *xy = malloc((new_npar + 1) * sizeof(int));
            if (xy == NULL)
                error("failed to allocate memory for xy in fges!\n");
            xy[0]        = g.x;
            xy[new_npar] = g.y;
            for (int j = 0; j < onion_size; ++j)
                xy[1 + j] = onion[j];
            int j = onion_size + 1;
            while (parents) {
                xy[j]   = parents->key;
                parents = parents->next;
                j++;
            }
            double ds = score_diff(data, xy, xy + 1, new_npar, new_npar - 1,
                                             fargs, iargs, score);
            free(xy);
            if (ds < min_ds) {
                min_ds = ds;
                min_g.set_size = onion_size - g.naxy_size;
                min_g.set = malloc(min_g.set_size * sizeof(int));
                for (int j = 0; j < min_g.set_size; ++j) {
                    min_g.set[j] = onion[j + min_g.naxy_size];
                }
            }
        }
    }
    free(onion);
    *dscore = min_ds;
    return min_g;
}



static void insert(struct cgraph *cg, struct gesrec g)
{
    add_edge_to_cgraph(cg, g.x, g.y, DIRECTED);
    for (int i = 0; i < g.set_size; ++i) {
        orient_undirected_edge(cg, g.set[i], g.y);
    }
}

double recalcluate_node(dataframe df, struct cgraph *cg, struct gesrec *gesrecp,
                                      score_func score, double *fargs,
                                                        int *iargs)
{
    double     dscore = DBL_MAX;
    double min_dscore = DBL_MAX;
    int y = gesrecp->y;
    for (int x = 0; x < df.nvar; ++x) {
        if ((x == y) || adjacent_in_cgraph(cg, x, y))
            continue;
        struct gesrec g = {0};
        g.x = x;
        g.y = y;
        struct ill *l = cg->spouses[y];
        while (l) {
            if (adjacent_in_cgraph(cg, x, l->key))
                g.naxy_size += 1;
            else
                g.set_size += 1;
            l = l->next;
        }
        g.naxy = malloc(g.naxy_size * sizeof(int));
        g.set  = malloc(g.set_size * sizeof(int));
        /* we have to reiterate through the list */
        l = cg->spouses[y];
        int j = 0;
        int k = 0;
        while (l) {
            int z = l->key;
            if (adjacent_in_cgraph(cg, x, z)) {
                g.naxy[j] = z;
                j++;
            }
            else {
                g.set[k]  = z;
                k++;
            }
            l = l->next;
        }
        struct gesrec min_g = score_powerset(cg, df, g, &dscore, score,
                                                 fargs, iargs);
        free(g.set);
        free(g.naxy);
        if (dscore < min_dscore) {
            min_dscore = dscore;
            free(gesrecp->set);
            free(gesrecp->naxy);
            *gesrecp = min_g;
        }
        else {
            free(min_g.set);
            free(min_g.naxy);
        }
    }
    return min_dscore;
}


static void delete(struct cgraph *cg, struct gesrec g)
{
    delete_edge_from_cgraph(cg, g.x, g.y, DIRECTED);
    for (int i = 0; i < g.set_size; ++i) {
        orient_undirected_edge(cg, g.x, g.set[i]);
        orient_undirected_edge(cg, g.y, g.set[i]);
    }
}

struct cgraph *ccf_fges(dataframe df, score_func score,
                                        double *fargs, int *iargs)
{
    struct cgraph *cg         = create_cgraph(df.nvar);
    double graph_score = 0.0f;
    double dscore      = 0.0f;
    /*
    * We need to set up the priority queue so we know which edge to add
    * (and the other relevant information) at each stage of fges. Roughly how
    * this works is that each the highest scoring edge incident in each node is
    * recorded and then we find the highest scoring edge of all of those by
    * using the heap data structure we have
    */
    struct gesrec *gesrecords = calloc(df.nvar, sizeof(struct gesrec));
    struct heap   *heap       = create_heap(df.nvar);
    double        *dscores    = heap->dscores;
    void         **records    = heap->records;
    int           *indices    = heap->indices;
    for (int i = 0; i < df.nvar; ++i) {
        records[i] = gesrecords + i;
        indices[i] = i;
    }
    /* STEP 0: score x --> y */
    for (int y = 0; y < df.nvar; ++y) {
        double min     = DBL_MAX;
        int    arg_min = -1;
        for (int x = 0; x < y; ++x) {
            int    xy[2] = {x, y};
            double ds = score_diff(df, xy, NULL, 1, 0, fargs, iargs, score);
            if (ds < min) {
                min     = ds;
                arg_min = x;
            }
        }
        dscores[y]      = min;
        gesrecords[y].x = arg_min;
        gesrecords[y].y = y;
    }
    build_heap(heap);
    /* FORWARD EQUIVALENCE SEARCH (FES) */
    struct gesrec *gesrecp;
    while ((gesrecp = extract_heap(heap, &dscore)) && dscore <= 0) {
        graph_score += dscore;
        insert(cg, *gesrecp);
        ccf_chickering(cg = ccf_pdx(cg));
        //print_cgraph(cg);
        for (int i = 0; i < df.nvar; ++i) {
             gesrecp =  gesrecords + i;
             gesrecp->y = i;
            dscore = recalcluate_node(df, cg, gesrecp, score, fargs, iargs);
            heap->dscores[i] = dscore;
            heap->records[i] = gesrecp;
            heap->indices[i] = i;
        }
        build_heap(heap);
    }

    /* BACKWARD EQUIVALENCE SEARCH (FES) */
    while (0) { /* TODO */}
    if (0) {
        delete(cg, *gesrecp);
    }

    for (int i = 0; i < df.nvar; ++i) {
        free(gesrecords[i].set);
        free(gesrecords[i].naxy);
    }
    free(gesrecords);
    //print_cgraph(cg);
    free_heap(heap);
    Rprintf("FES complete\n");
    return cg;
}
