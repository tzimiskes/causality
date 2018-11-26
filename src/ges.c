#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "headers/cgraph.h"
#include "headers/heap.h"
#include "headers/dataframe.h"
#include "headers/scores.h"
#include "headers/edgetypes.h"
#include "headers/ges.h"

#define DEFAULT_SCORE 1.0f

#ifndef DEBUG
#define DEBUG 0
#endif

static int is_valid_insertion(struct cgraph *cg, struct ges_op op, int *mem)
{
    return valid_fes_clique(cg, op) && !cycle_created(cg, op, mem);
}

static int is_valid_deletion(struct cgraph *cg, struct ges_op op)
{
    return valid_bes_clique(cg, op);
}

static void score_insertion(struct cgraph *cg, struct ges_op *op,
                                               struct ges_score sc, int *mem)
{
    struct ges_op o       = *op;
    int           base_nx = o.naxy_size + o.n_parents;
    int          *x       = malloc((base_nx + o.set_size) * sizeof(int));
    memcpy(x,               o.parents, o.n_parents * sizeof(int));
    memcpy(x + o.n_parents, o.naxy,    o.naxy_size * sizeof(int));
    uint64_t powerset_size = 1 << o.set_size;
    for (o.t = 0; o.t < powerset_size; ++o.t) {
        if (!is_valid_insertion(cg, o, mem))
            continue;
        int nx = base_nx;
        for (int j = 0; j < o.set_size; ++j) {
            if (o.t & 1 << j)
                x[nx++] = o.set[j];
        }
        o.score_diff = sc.score(sc.df, o.x, o.y, x, nx, sc.args, sc.fmem,
                                     sc.imem);
        if (o.score_diff < op->score_diff)
            *op = o;
    }
    free(x);
}

void score_deletion(struct cgraph *cg, struct ges_op *op, struct ges_score sc)
{
    struct ges_op o       = *op;
    int           base_nx = 0;
    int          *x       = malloc((o.n_parents + o.naxy_size) * sizeof(int));
    for (int i = 0; i < o.n_parents; ++i) {
        if (o.parents[i] != o.x)
            x[base_nx++] = o.parents[i];
    }
    uint64_t powerset_size = 1 << o.naxy_size;
    for (o.h = 0; o.h < powerset_size; ++o.h) {
        if (!is_valid_deletion(cg, o))
            continue;
        int nx = base_nx;
        for (int j = 0; j < o.naxy_size; ++j) {
            if (!(o.h & 1 << j) && o.naxy[j] != o.x)
                x[nx++] = o.naxy[j];
        }
        o.score_diff = -sc.score(sc.df, o.x, o.y, x, nx, sc.args, sc.fmem,
                                        sc.imem);
        if (o.score_diff < op->score_diff)
            *op = o;
    }
    free(x);
}

void compute_common_covariances(struct cgraph *cg, int y, int n,
                                                   struct ges_score *sc)
{
    struct dataframe data = sc->df;
    int npar        = ill_size(cg->parents[y]) + ill_size(cg->spouses[y]);
    int cov_xx_size = npar * npar;
    int cov_xy_size = cg->n_nodes;
    sc->fmem    = calloc(npar + cov_xx_size + cov_xy_size, sizeof(double));
    sc->imem    = malloc((npar + 1) * sizeof(double));
    sc->imem[0] = npar;
    /*
     * grab the data we need so we can calculate the largest covariance matrix
     * of the the parents of y
     */
    double    **x = malloc(npar * sizeof(double *));
    int         i = 0;
    struct ill *p = cg->parents[y];
    while (p) {
        int par = p->key;
        sc->imem[i + 1] = par;
        x[i] = data.df[par];
        p    = p->next;
        i++;
    }
    p = cg->spouses[y];
    while (p) {
        sc->imem[i + 1] = p->key;
        x[i] = data.df[p->key];
        p    = p->next;
        i++;
    }
    double *cov_xy = sc->fmem;
    double *cov_xx = cov_xy + cov_xy_size;
    fcov_xy(cov_xy, (double **) data.df, data.df[y], data.nobs, n);
    fcov_xx(cov_xx, x, data.nobs, npar);
    free(x);
}

void compute_xgroup_covariances(struct dataframe data, int _x,
                                                       struct ges_score *score)
{
    int  npar = score->imem[0];
    int *pars = score->imem + 1;
    if(npar == 0)
        return;
    double  *cov_x_x = score->fmem + data.nvar + npar * npar;
    double **x       = malloc((npar + 1) * sizeof(double *));
    x[0] = data.df[_x];
    for(int i = 0; i < npar; ++i)
        x[i + 1] = data.df[pars[i]];
    fcov_xy(cov_x_x, x, x[0], data.nobs, npar);
    free(x);
}

void recalculate_fes(struct cgraph *cg, struct ges_op *op, struct ges_score sc)
{
    op->score_diff = DEFAULT_SCORE;
    /* preallocate memory for validity testing in FES */
    int *mem = malloc(cg->n_nodes * 2 * sizeof(int));
    /* precalculate the covariances common to all calculations */
    int y = op->y;
    compute_common_covariances(cg, y, cg->n_nodes, &sc);
    for (int x = 0; x < cg->n_nodes; ++x) {
        if (x == y || adjacent_in_cgraph(cg, x, y))
            continue;
        struct ges_op o;
        o.x          = x;
        o.y          = y;
        o.type       = INSERTION;
        o.score_diff = DEFAULT_SCORE;
        calculate_parents(cg, &o);
        /*
         * Partition the neighbors of y into those adjacent to x (naxy), and
         * those who are not (set).
         */
        partition_neighbors(cg, &o);
        score_insertion(cg, &o, sc, mem);
        if (o.score_diff < op->score_diff) {
            free_ges_op(*op);
            *op = o;
        }
        else
            free_ges_op(o);
    }
    free(mem);
    free_ges_score(sc);
}

void recalculate_bes(struct cgraph *cg, struct ges_op *op, struct ges_score sc)
{
    op->score_diff = DEFAULT_SCORE;
    int y = op->y;
    /*
     * We calculate what parents and spouses to iterate over to make
     * load balancing easier
     */
    int         n     = ill_size(cg->parents[y]) + ill_size(cg->spouses[y]);
    int        *nodes = malloc(n * sizeof(int));
    struct ill *p     = cg->parents[y];
    int         i     = 0;
    while (p) {
        nodes[i++] = p->key;
        p          = p->next;
    }
    p = cg->spouses[y];
    while (p) {
        nodes[i++] = p->key;
        p          = p->next;
    }
    /* precalculate the covariances common to all calculations */
    compute_common_covariances(cg, y, cg->n_nodes, &sc);
    for (int i = 0; i < n; ++i) {
        struct ges_op o;
        o.x          = nodes[i];
        o.y          = y;
        o.score_diff = DEFAULT_SCORE;
        o.type       = DELETION;
        calculate_parents(cg, &o);
        /* Calculate the neighbors of y that are adjacent to x */
        calculate_naxy(cg, &o);
        score_deletion(cg, &o, sc);
        if (o.score_diff < op->score_diff) {
            free_ges_op(*op);
            *op = o;
        }
        else
            free_ges_op(o);
    }
    free(nodes);
    free_ges_score(sc);
}

/*
 * insert takes the ges_op and adds the edge x --> y, and then for all
 * nodes in s, orients node --> y.
 */
static void insert(struct cgraph *cg, struct ges_op g)
{
    add_edge_to_cgraph(cg, g.x, g.y, DIRECTED);
    for (int i = 0; i < g.set_size; ++i) {
        if (g.t & 1 << i)
            orient_undirected_edge(cg, g.set[i], g.y);
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
            if (g.h & 1 << i) {
                if (edge_undirected_in_cgraph(cg, g.x, g.naxy[i]))
                    orient_undirected_edge(cg, g.x , g.naxy[i]);
                orient_undirected_edge(cg, g.y, g.naxy[i]);
        }
    }
}

struct cgraph *ccf_ges(struct ges_score sc)
{
    int            nvar        = sc.df.nvar;
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
        compute_common_covariances(cg, y, y, &sc);
        for (int i = 0; i < y; ++i) {
            double score_diff = sc.score(sc.df, i, y, NULL, 0, sc.args,
                                                sc.fmem, sc.imem);
            if (score_diff < min_score) {
                min_score = score_diff;
                x         = i;
            }
        }
        free_ges_score(sc);
        dscores[y]        = min_score;
        ops[y].x          = x;
        ops[y].y          = y;
        ops[y].score_diff = min_score;
        ops[y].type       = INSERTION;
    }
    build_heap(heap);
    /* FORWARD EQUIVALENCE SEARCH (FES) */
    struct cgraph *cpy = copy_cgraph(cg);
    struct ges_op *op;
    int *mem = malloc(nvar * 2 * sizeof(int));
    /* extract the smallest op from the heap and check to see if positive */
    while ((op = peek_heap(heap)) && op->score_diff <= 0.0f) {
        if (!is_valid_insertion(cg, *op, mem)) {
            remove_heap(heap, op->y);
            recalculate_fes(cg, op, sc);
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
            op = ops + nodes[i];
            remove_heap(heap, nodes[i]);
            recalculate_fes(cg, op, sc);
            insert_heap(heap, op->score_diff, op);
        }
        free(nodes);
    }
    free(mem);
    /* BES STEP 0 */
    for (int i = 0; i < nvar; ++i) {
        op    = ops + i;
        op->y = i;
        recalculate_bes(cg, op, sc);
        records[i]      = op;
        indices[i]      = i;
        dscores[i]      = op->score_diff;
    }
    build_heap(heap);
    /* BACKWARD EQUIVALENCE SEARCH (BES) */
    while ((op = peek_heap(heap)) && op->score_diff <= 0.0f) {
        if (!is_valid_deletion(cg, *op)) {
            remove_heap(heap, op->y);
            recalculate_bes(cg, op, sc);
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
            op = ops + nodes[i];
            remove_heap(heap, nodes[i]);
            recalculate_bes(cg, op, sc);
            insert_heap(heap, op->score_diff, op);
        }
        free(nodes);
    }
    /* Memory cleanup */
    for (int i = 0; i < nvar; ++i)
        free_ges_op(ops[i]);
    free(ops);
    free_heap(heap);
    free_cgraph(cpy);
    return cg;
}
