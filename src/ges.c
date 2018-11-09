#include <causality.h>
#include <cgraph.h>
#include <heap.h>
#include <dataframe.h>
#include <scores.h>
#include <local_meek.h>
#include <edgetypes.h>
#include <stdint.h>
#include <ges_utils.h>

#define DEFAULT_SCORE 1.0f

#ifndef DEBUG
#define DEBUG 0
#endif

struct score {
    ges_score score;
    double *fargs;
    int    *iargs;
    double *fmem;
    int    *imem;
    int   mem_sizes[4];
    void *mem[4];
};

static int is_valid_insertion(struct cgraph *cg, struct gesrec g, int *mem)
{
    if (!forms_clique(cg, g) && !cycle_created(cg, g, mem))
        return 1;
    else
        return 0;
}

static int is_valid_deletion(struct cgraph *cg, struct gesrec g)
{
    return forms_clique(cg, g);
}

void score_insertion(struct cgraph *cg, struct dataframe df, struct gesrec *g,
                                        struct score score, int *mem)
{
    struct gesrec t = *g;
    /* pronounced pun axy c zey */
    int  py_u_naxy_size = t.naxy_size  + t.parents_size;
    int *py_u_naxy_u_s  = malloc((py_u_naxy_size + t.set_size) * sizeof(int));
    memcpy(py_u_naxy_u_s, t.parents, t.parents_size * sizeof(int));
    memcpy(py_u_naxy_u_s, t.naxy   , t.naxy_size * sizeof(int));
    /*
     * we need to iterate over the power set of S, and we do this by
     * calculating the powerset size and then iterating through that, using bit
     * operations to construct the subset
     */
    uint64_t powerset_size = 1 << t.set_size;
    for (uint64_t i = 0; i < powerset_size; ++i) {
        t.t = i;
        if (!is_valid_insertion(cg, t, mem))
            continue;
        int py_u_naxy_u_s_size = py_u_naxy_size;
        /*
         * constructs the subset of P(S) we are using for this iteration using
         * bitwise operations
         */
        for (uint32_t j = 0; j < (uint32_t) t.set_size; ++j) {
            if ((i & (1 << j)) == (1 << j))
                py_u_naxy_u_s[py_u_naxy_u_s_size++] = t.set[j];
        }
        double score_diff = score.score(df, t.x, t.y, py_u_naxy_u_s,
                                            py_u_naxy_u_s_size, score.fargs,
                                            score.iargs, score.fmem,
                                            score.imem);
        if (score_diff < g->score_diff) {
            g->score_diff = score_diff;
            g->t = i;
        }
    }
    free(py_u_naxy_u_s);
}

void compute_common_covariances(struct cgraph *cg, struct dataframe data,
                                                   int y, int n,
                                                   struct score *scorep)
{
    int npar        = ill_size(cg->parents[y]) + ill_size(cg->spouses[y]);
    int cov_xx_size = npar * npar;
    int cov_xy_size = cg->n_nodes;
    scorep->fmem    = calloc(npar + cov_xx_size + cov_xy_size, sizeof(double));
    scorep->imem    = malloc((npar + 1) * sizeof(double));
    scorep->imem[0] = npar;
    /*
     * grab the data we need so we can calculate the largest covariance matrix
     * of the the parents of y
     */
    double    **x = malloc(npar * sizeof(double *));
    int         i = 0;
    struct ill *p = cg->parents[y];
    while (p) {
        int par = p->key;
        scorep->imem[i + 1] = par;
        x[i] = data.df[par];
        p    = p->next;
        i++;
    }
    p = cg->spouses[y];
    while (p) {
        scorep->imem[i + 1] = p->key;
        x[i] = data.df[p->key];
        p    = p->next;
        i++;
    }
    double *cov_xy = scorep->fmem;
    double *cov_xx = cov_xy + cov_xy_size;
    fcov_xy(cov_xy, (double **) data.df, data.df[y], data.nobs, n);
    fcov_xx(cov_xx, x, data.nobs, npar);
    free(x);
}

void compute_xgroup_covariances(struct dataframe data, int _x,
                                                       struct score *scorep)
{
    int  npar = scorep->imem[0];
    int *pars = scorep->imem + 1;
    if(npar == 0)
        return;
    double  *cov_x_x = scorep->fmem + data.nvar + npar * npar;
    double **x       = malloc((npar + 1) * sizeof(double *));
    x[0] = data.df[_x];
    for(int i = 0; i < npar; ++i)
        x[i + 1] = data.df[pars[i]];
    fcov_xy(cov_x_x, x, x[0], data.nobs, npar);
    free(x);
}

double recalculate_fes(struct dataframe df,  struct cgraph *cg,
                                             struct gesrec *gesrecp,
                                             struct score score)
{
    double min_score_diff = DEFAULT_SCORE;
    int    y              = gesrecp->y;
    /* preallocate memory for validity testing in FES */
    int   *mem            = malloc(cg->n_nodes * 2 * sizeof(int));
    /* precalculate the covariances common to all calculations */
    compute_common_covariances(cg, df, y, df.nvar, &score);
    for (int x = 0; x < df.nvar; ++x) {
        if (x == y || adjacent_in_cgraph(cg, x, y))
            continue;
        struct gesrec g;
        g.x          = x;
        g.y          = y;
        g.type       = 1;
        g.score_diff = DEFAULT_SCORE;
        calculate_parents(cg, &g);
        /*
         * Partition the neighbors of y into those adjacent to x (naxy), and
         * those who are not (set).
         */
        partition_neighbors(cg, &g);
        score_insertion(cg, df, &g, score, mem);

        if (g.score_diff < gesrecp->score_diff) {
            free_gesrec(*gesrecp);
            *gesrecp = g;
        }
        else
            free_gesrec(g);;
    }
    free(mem);
    free(score.fmem);
    free(score.imem);
    return min_score_diff;
}

double recalculate_bes(struct dataframe df, struct cgraph *cg,
                                            struct gesrec *gesrecp,
                                            struct score score)
{
    double score_diff     = DEFAULT_SCORE;
    double min_score_diff = DEFAULT_SCORE;
    int    y              = gesrecp->y;
    /*
     * We calculate what parents and spouses to iterate over to make
     * load balancing easier
     */
    int        *nodes = malloc(100 * sizeof(int));
    int         n     = 0;
    struct ill *p     = cg->parents[y];
    while (p) {
        if (n > 100)
            error("buffer overflow\n");
        nodes[n++] = p->key;
        p = p->next;
    }
    p = cg->spouses[y];
    while (p) {
        if (n > 100)
            error("buffer overflow\n");
        nodes[n++] = p->key;
        p = p->next;
    }
    /* precalculate the covariances common to all calculations */
    compute_common_covariances(cg, df, y, df.nvar, &score);
    for (int i = 0; i < n; ++i) {
        struct gesrec g = {};
        g.x = nodes[i];
        g.y = y;
        /*
         * Partition the neighbors of y into those adjacent to x (naxy), and
         * those who are not (set). partition_neighbors fills in g.naxy
         * and g.set.
         */
        partition_neighbors(cg, &g);

        score_insertion(cg, df, &g, score, nodes);

        if (g.score_diff < min_score_diff) {
            min_score_diff = score_diff;
            free_gesrec(*gesrecp);
            *gesrecp = g;
        }
        else
            free_gesrec(g);
    }
    free(nodes);
    free(score.fmem);
    free(score.imem);
    return min_score_diff;
}

/*
 * insert takes the gesrec and adds the edge x --> y, and then for all
 * nodes in s, orients node --> y.
 */
static void insert(struct cgraph *cg, struct gesrec g)
{
    add_edge_to_cgraph(cg, g.x, g.y, DIRECTED);
    for (int i = 0; i < g.set_size; ++i) {
//        Rprintf("orient %i -- > %i\n", g.set[i], g.y);
        orient_undirected_edge(cg, g.set[i], g.y);
    }
}

/*
 * delete takes the gesrec and deletes edge between x and y, and then for all
 * node in s, orients y -- > node, and x --> node if x --- y. Since we do not
 * know whether or not  the edges involving x are directed or undirected,
 * we must check them.
 */
static void delete(struct cgraph *cg, struct gesrec g)
{
    if (edge_directed_in_cgraph(cg, g.x, g.y))
        delete_edge_from_cgraph(cg, g.x, g.y, DIRECTED);
    else
        delete_edge_from_cgraph(cg, g.x, g.y, UNDIRECTED);
    for (int i = 0; i < g.set_size; ++i) {
        if (edge_undirected_in_cgraph(cg, g.x, g.set[i]))
            orient_undirected_edge(cg, g.x , g.set[i]);
        orient_undirected_edge(cg, g.y, g.set[i]);
    }
}

struct cgraph *ccf_ges(struct dataframe df, struct score score)
{
    struct cgraph *cg          = create_cgraph(df.nvar);
    double         graph_score = 0.0f;
    double         score_diff  = 0.0f;
    /*
    * We need to set up a priority queue so we know which edge to add
    * (and the other relevant information) at each stage of ges. Roughly how
    * this works is that each the highest scoring edge incident in each node is
    * recorded and then we find the highest scoring edge of all of those by
    * extracting the top of the heap.
    */
    struct gesrec *ops  = calloc(df.nvar, sizeof(struct gesrec));
    struct heap   *heap = create_heap(df.nvar, ops, sizeof(struct gesrec));
    double        *dscores    = heap->keys;
    void         **records    = heap->data;
    int           *indices    = heap->indices;
    for (int i = 0; i < df.nvar; ++i) {
        records[i] = ops + i;
        indices[i] = i;
    }
    /* FES STEP 0: For all x,y score x --> y */
    for (int j = 0; j < df.nvar; ++j) {
        double min     = DEFAULT_SCORE;
        int    arg_min = -1;
        compute_common_covariances(cg, df, j, j, &score);
        for (int i = 0; i < j; ++i) {
            double ds = score.score(df, i, j, NULL, 0, score.fargs,
                                        score.iargs, score.fmem, score.imem);
            if (ds < min) {
                min     = ds;
                arg_min = i;
            }
        }
        free(score.fmem);
        free(score.imem);
        dscores[j]      = min;
        ops[j].x = arg_min;
        ops[j].y = j;
    }
    build_heap(heap);
    /* FORWARD EQUIVALENCE SEARCH (FES) */
    struct cgraph *cpy = copy_cgraph(cg);
    struct gesrec *gesrecp;
    int *mem = malloc(cg->n_nodes * 2 * sizeof(int));
    /* extract the smallest gesrec from the heap and check to see if positive */
    while ((gesrecp = peek_heap(heap, &score_diff)) && score_diff <= 0.0f) {
        if (!is_valid_insertion(cg, *gesrecp, mem)) {
            remove_heap(heap, gesrecp->y);
            score_diff = recalculate_fes(df, cg, gesrecp, score);
            insert_heap(heap, score_diff, gesrecp);
            continue;
        }
        graph_score   += score_diff;
        insert(cg, *gesrecp);
        int  n_visited = 0;
        int *visited   = reorient(cg, *gesrecp, FES, &n_visited);
        int  n_nodes   = 0;
        int *nodes     = deterimine_nodes_to_recalc(cpy, cg, *gesrecp, visited,
                                                         n_visited, &n_nodes);
        for(int i = 0; i < n_nodes; ++i) {
            gesrecp    = ops + nodes[i];
            remove_heap(heap, nodes[i]);
            score_diff = recalculate_fes (df, cg, gesrecp, score);
            insert_heap(heap, score_diff, gesrecp);
        }
        free(nodes);
    }
    Rprintf("FES complete\n");
    free(mem);

    /* BES STEP 0 */
    if (0) {
        for (int i = 0; i < df.nvar; ++i) {
            gesrecp = ops + i;
            records[i]      = gesrecp;
            indices[i]      = i;
            gesrecp->y      = i;
            dscores[i]      = recalculate_bes(df, cg, gesrecp, score);
        }
    }
    build_heap(heap);
    /* BACKWARD EQUIVALENCE SEARCH (FES) */
    while (0 && (gesrecp = peek_heap(heap, &score_diff)) && score_diff <= 0.0f) {
        if (!is_valid_deletion(cg, *gesrecp)) {
            remove_heap(heap, gesrecp->y);
            score_diff = recalculate_bes(df, cg, gesrecp, score);
            insert_heap(heap, score_diff, gesrecp);
            continue;
        }
        graph_score   += score_diff;
        delete(cg, *gesrecp);
        int  n_visited = 0;
        int *visited   = reorient(cg, *gesrecp, BES, &n_visited);
        int  n_nodes   = 0;
        int *nodes     = deterimine_nodes_to_recalc(cpy, cg, *gesrecp, visited,
                                                         n_visited, &n_nodes);
        for (int i = 0; i < n_nodes; ++i) {
            gesrecp    = ops + nodes[i];
            remove_heap(heap, nodes[i]);
            /* broken */
            score_diff = recalculate_bes(df, cg, gesrecp, score);
            insert_heap(heap, score_diff, gesrecp);
        }
        free(nodes);
    }
    Rprintf("BES complete\n");
    /* Memory cleanup */
    for (int i = 0; i < df.nvar; ++i)
        free_gesrec(ops[i]);
    free(ops);
    free_heap(heap);
    free_cgraph(cpy);
    print_cgraph(cg);
    Rprintf("Number of edges: %i\n", cg->n_edges);

    return cg;
}
