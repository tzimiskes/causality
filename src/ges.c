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
    struct dataframe df;
    double    *fargs;
    int       *iargs;
    double    *fmem;
    int       *imem;
};

static int is_valid_insertion(struct cgraph *cg, struct ges_op op, int *mem)
{
    if (forms_clique(cg, op) && !cycle_created(cg, op, mem))
        return 1;
    else
        return 0;
}

static int is_valid_deletion(struct cgraph *cg, struct ges_op op)
{
    return forms_clique(cg, op);
}

void score_insertion(struct cgraph *cg, struct ges_op *op, struct score score,
                                        int *mem)
{
    struct ges_op o      = *op;
    int  n_base_features = o.naxy_size  + o.parents_size;
    int *features        = malloc((n_base_features + o.set_size) * sizeof(int));
    memcpy(features, o.parents, o.parents_size * sizeof(int));
    memcpy(features, o.naxy   , o.naxy_size * sizeof(int));
    /*
     * we need to iterate over the power set of S, and we do this by
     * calculating the powerset size and then iterating through that, using bit
     * operations to construct the subset
     */
    uint64_t powerset_size = 1 << o.set_size;
    for (o.t = 0; o.t < powerset_size; ++o.t) {
        if (!is_valid_insertion(cg, o, mem))
            continue;
        int nfeatures = n_base_features;
        for (int j = 0; j < o.set_size; ++j) {
            if ((o.t & (1 << j)) == (1 << j))
                features[nfeatures++] = o.set[j];
        }
        o.score_diff = score.score(score.df, o.x, o.y, features, nfeatures,
                                       score.fargs, score.iargs, score.fmem,
                                       score.imem);
        if (o.score_diff < op->score_diff)
            *op = o;
    }
    free(features);
}

void compute_common_covariances(struct cgraph *cg, int y, int n,
                                                   struct score *scorep)
{
    struct dataframe data = scorep->df;
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

void recalculate_fes(struct cgraph *cg, struct ges_op *op, struct score score)
{
    op->score_diff = DEFAULT_SCORE;
    /* preallocate memory for validity testing in FES */
    int *mem = malloc(cg->n_nodes * 2 * sizeof(int));
    /* precalculate the covariances common to all calculations */
    int y = op->y;
    compute_common_covariances(cg, y, cg->n_nodes, &score);
    Rprintf("calculate_common\n");
    for (int x = 0; x < cg->n_nodes; ++x) {
        if (x == y || adjacent_in_cgraph(cg, x, y))
            continue;
        struct ges_op o;
        o.x          = x;
        o.y          = y;
        o.type       = INSERTION;
        o.score_diff = DEFAULT_SCORE;
        Rprintf("calculate_pars\n");
        calculate_parents(cg, &o);
        /*
         * Partition the neighbors of y into those adjacent to x (naxy), and
         * those who are not (set).
         */
        Rprintf("calculate_negs\n");
        partition_neighbors(cg, &o);
        Rprintf("score\n");
        score_insertion(cg, &o, score, mem);
        Rprintf("better?\n");
        if (o.score_diff < op->score_diff) {
            free_ges_op(*op);
            *op = o;
        }
        else
            free_ges_op(o);
    }
    free(mem);
    free(score.fmem);
    free(score.imem);
}

void recalculate_bes(struct cgraph *cg, struct ges_op *op, struct score score)
{
    op->score_diff = DEFAULT_SCORE;
    int    y              = op->y;
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
    compute_common_covariances(cg, y, cg->n_nodes, &score);
    for (int i = 0; i < n; ++i) {
        struct ges_op o;
        o.x = nodes[i];
        o.y = y;
        o.type = DELETION;
        calculate_parents(cg, &o);
        /* Calculate the neighbors of y that are adjacent to x */
        calculate_naxy(cg, &o);
        score_insertion(cg, &o, score, nodes);
        if (o.score_diff < op->score_diff) {
            free_ges_op(*op);
            *op = o;
        }
        else
            free_ges_op(o);
    }
    free(nodes);
    free(score.fmem);
    free(score.imem);
}

/*
 * insert takes the ges_op and adds the edge x --> y, and then for all
 * nodes in s, orients node --> y.
 */
static void insert(struct cgraph *cg, struct ges_op g)
{
    add_edge_to_cgraph(cg, g.x, g.y, DIRECTED);
    for (int i = 0; i < g.set_size; ++i) {
        if ((g.t & 1 << i) == 1 << i) {
            orient_undirected_edge(cg, g.set[i], g.y);
        }
    }
}

/*
 * delete takes the ges_op and deletes edge between x and y, and then for all
 * node in s, orients y -- > node, and x --> node if x --- y. Since we do not
 * know whether or not  the edges involving x are directed or undirected,
 * we must check them.
 */
static void delete(struct cgraph *cg, struct ges_op g)
{
    if (edge_directed_in_cgraph(cg, g.x, g.y))
        delete_edge_from_cgraph(cg, g.x, g.y, DIRECTED);
    else
        delete_edge_from_cgraph(cg, g.x, g.y, UNDIRECTED);
    for (int i = 0; i < g.naxy_size; ++i) {
        if (edge_undirected_in_cgraph(cg, g.x, g.set[i]))
            orient_undirected_edge(cg, g.x , g.set[i]);
        orient_undirected_edge(cg, g.y, g.set[i]);
    }
}

struct cgraph *ccf_ges(struct score score)
{
    int            nvar        = score.df.nvar;
    struct cgraph *cg          = create_cgraph(nvar);
    double         graph_score = 0.0f;
    /*
    * We need to set up a priority queue so we know which edge to add
    * (and the other relevant information) at each stage of ges. Roughly how
    * this works is that each the highest scoring edge incident in each node is
    * recorded and then we find the highest scoring edge of all of those by
    * extracting the top of the heap.
    */
    struct ges_op *ops  = calloc(nvar, sizeof(struct ges_op)); /*OBP + SLG */
    struct heap   *heap = create_heap(nvar, ops, sizeof(struct ges_op));
    double        *dscores    = heap->keys;
    void         **records    = heap->data;
    int           *indices    = heap->indices;
    for (int i = 0; i < nvar; ++i) {
        records[i] = ops + i;
        indices[i] = i;
    }
    /* FES STEP 0: For all x,y score x --> y */
    for (int y = 0; y < nvar; ++y) {
        double min_score = DEFAULT_SCORE;
        int    x         = -1;
        compute_common_covariances(cg, y, y, &score);
        for (int i = 0; i < y; ++i) {
            double score_diff = score.score(score.df, i, y, NULL, 0,
                                                      score.fargs, score.iargs,
                                                      score.fmem, score.imem);
            if (score_diff < min_score) {
                min_score = score_diff;
                x         = i;
            }
        }
        free(score.fmem);
        free(score.imem);
        dscores[y]        = min_score;
        ops[y].x          = x;
        ops[y].y          = y;
        ops[y].score_diff = min_score;
        ops[y].type       = INSERTION;
    }
    build_heap(heap);
    /* FORWARD EQUIVALENCE SEARCH (FES) */
    Rprintf("Begin FES\n");
    struct cgraph *cpy = copy_cgraph(cg);
    struct ges_op *op;
    int *mem = malloc(nvar * 2 * sizeof(int));
    /* extract the smallest ges_op from the heap and check to see if positive */
    while ((op = peek_heap(heap)) && op->score_diff <= 0.0f) {
        if (!is_valid_insertion(cg, *op, mem)) {
            Rprintf("here\n");
            remove_heap(heap, op->y);
            recalculate_fes(cg, op, score);
            insert_heap(heap, op->score_diff, op);
            continue;
        }
        graph_score   += op->score_diff;
        insert(cg, *op);
        int  n_visited = 0;
        int *visited   = reorient(cg, *op, &n_visited);
        int  n_nodes   = 0;
        int *nodes     = deterimine_nodes_to_recalc(cpy, cg, *op, visited,
                                                         n_visited, &n_nodes);
        for(int i = 0; i < n_nodes; ++i) {
            op    = ops + nodes[i];
            remove_heap(heap, nodes[i]);
            Rprintf("here\n");
            recalculate_fes(cg, op, score);
            Rprintf("here2\n");
            insert_heap(heap, op->score_diff, op);
        }
        free(nodes);
    }
    Rprintf("FES complete\n");
    free(mem);

    /* BES STEP 0 */
    if (0) {
        for (int i = 0; i < nvar; ++i) {
            op = ops + i;
            op->y = i;
            recalculate_bes(cg, op, score);
            records[i]      = op;
            indices[i]      = i;
            dscores[i]      = op->score_diff;
        }
    }
    build_heap(heap);
    /* BACKWARD EQUIVALENCE SEARCH (FES) */
    while (0 && (op = peek_heap(heap)) && op->score_diff <= 0.0f) {
        if (!is_valid_deletion(cg, *op)) {
            remove_heap(heap, op->y);
            recalculate_bes(cg, op, score);
            insert_heap(heap, op->score_diff, op);
            continue;
        }
        graph_score   += op->score_diff;
        delete(cg, *op);
        int  n_visited = 0;
        int *visited   = reorient(cg, *op, &n_visited);
        int  n_nodes   = 0;
        int *nodes     = deterimine_nodes_to_recalc(cpy, cg, *op, visited,
                                                         n_visited, &n_nodes);
        for (int i = 0; i < n_nodes; ++i) {
            op    = ops + nodes[i];
            remove_heap(heap, nodes[i]);
            /* broken */
            recalculate_bes(cg, op, score);
            insert_heap(heap, op->score_diff, op);
        }
        free(nodes);
    }
    Rprintf("BES complete\n");
    /* Memory cleanup */
    for (int i = 0; i < nvar; ++i)
        free_ges_op(ops[i]);
    free(ops);
    free_heap(heap);
    free_cgraph(cpy);
    print_cgraph(cg);
    Rprintf("Number of edges: %i\n", cg->n_edges);

    return cg;
}
