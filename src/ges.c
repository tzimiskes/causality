/* Author: Alexander Rix
 * Date  : 11/29/2018
 * Description: ges.c contains a (partially optimized) version of
 * greedy equivalence search by Chickering. It is a score based
 * causal discovery algorithm that is correct in the the large sample
 * limit given its assumptions. For more information see the paper by
 * David Maxwell Chickering, Optimal Structure Identification With Greedy
 * Search Journal of Machine Learning Research 3 (2002) 507-554
 */

#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "headers/causality.h"
#include "headers/cgraph.h"
#include "headers/heap.h"
#include "headers/dataframe.h"
#include "headers/ges.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#define DEFAULT_SCORE_DIFF 1.0f

#define IS_TAIL_NODE(t, node) ((t) & 1 << (node))
#define IS_HEAD_NODE(h, node) ((h) & 1 << (node))

/*
 * is_valid_insertion returns whether or not applying the given insertion
 * operator, op, is valid. It checks to see if T U NaXY forms a clique and
 * that adding the edge x --> y does not result in a cycle. Dynamically
 * allocated memory is passed in to cycle_created as an optimization.
 */
static int is_valid_insertion(struct cgraph *cg, struct ges_operator op,
                                                 int *cycle_mem)
{
    return valid_fes_clique(cg, op) && !cycle_created(cg, op, cycle_mem);
}

/*
 * is_valid_deletion returns whether or not applying the given deletion
 * operator, op, results in a valid deletion. It checks to see if NaXY/H
 * forms a clique.
 */
static int is_valid_deletion(struct cgraph *cg, struct ges_operator op)
{
    return valid_bes_clique(cg, op);
}

/*
 * score_insertion_operator takes the insertion operator op and modifies it by
 * finding the (valid) set T (where T is in the powerset of S) that minimizes
 * the quantity score((py, naxy, T, x), y) - score((py, naxy, T), y).
 * The function also modifies op by setting the score_diff field to the best
 * score difference. If there is no valid T, then op is unmodified.
 */
static void score_insertion_operator(struct cgraph *cg, struct ges_operator *op,
                                                        struct ges_score gs,
                                                        int *cycle_mem)
{
    struct ges_operator o = *op;
    /* allocate enough memory on the stack to store all of Pa(y) U Naxy U S */
    int py_naxy_size = o.naxy_size + o.n_parents;
    int py_naxy_t[py_naxy_size + o.set_size];
    for (int i = 0; i < o.n_parents; ++i)
        py_naxy_t[i] = o.parents[i];
    for (int i = 0; i < o.naxy_size; ++i)
        py_naxy_t[i + o.n_parents] = o.naxy[i];
    /* iterate through the powerset of S via bit operations.  */
    uint64_t powerset_size = 1 << o.set_size; /* |P(S)|  = 2^|S| */
    for (o.t = 0; o.t < powerset_size; ++o.t) {
        if (!is_valid_insertion(cg, o, cycle_mem))
            continue;
        int py_naxy_t_size = py_naxy_size;
        for (int i = 0; i < o.set_size; ++i) {
            /* We now need to score the valid operator. We add t to py_naxy_t */
            if (IS_TAIL_NODE(o.t, i))
                py_naxy_t[py_naxy_t_size++] = o.set[i];
        }
        /* score_diff = score(y, pay_naxy_t_x) - score(y, pay_naxy_t) */
        o.score_diff = gs.gsf(gs.df, o.xp, o.y, py_naxy_t, py_naxy_t_size,
                                     gs.args, gs.gsm);
        if (o.score_diff < op->score_diff)
            *op = o;
    }
}

/*
 * score_deletion_operator takes the deletion operator op and modifies it by
 * finding the (valid) set H (where H is in the powerset of Naxy/x) that
 * minimizes the quantity -(score((py, naxy/H, x), y) - score((py, naxy/H, y)).
 * The function also modifies op by setting the score_diff field to the best
 * score difference. If there is no valid H, then op is unmodified.
 */
void score_deletion_operator(struct cgraph *cg, struct ges_operator *op,
                                                struct ges_score gs)
{
    struct ges_operator o = *op;
    /* allocate enough memory on the stack to store all of Pa(y) U Naxy */
    int py_size = 0;
    int py_naxy_mh[o.n_parents + o.naxy_size];
    /* add pa(y) != x to py_naxy_smh */
    for (int i = 0; i < o.n_parents; ++i) {
        if (o.parents[i] != o.xp)
            py_naxy_mh[py_size++] = o.parents[i];
    }
    /* iterate through the powerset of naxy via bit operations.  */
    uint64_t powerset_size = 1 << o.naxy_size;
    for (o.h = 0; o.h < powerset_size; ++o.h) {
        if (!is_valid_deletion(cg, o))
            continue;
        /* add naxy_smh to py_naxy_smh (naxy minus h ) */
        int py_naxy_mh_size = py_size;
        for (int i = 0; i < o.naxy_size; ++i) {
            if (!IS_HEAD_NODE(o.h, i) && o.naxy[i] != o.xp)
                py_naxy_mh[py_naxy_mh_size++] = o.naxy[i];
        }
        o.score_diff = -gs.gsf(gs.df, o.xp, o.y, py_naxy_mh, py_naxy_mh_size,
                                        gs.args, gs.gsm);
        if (o.score_diff < op->score_diff)
            *op = o;
    }
}

static void apply_optimization1(struct cgraph *cg, int y, int n,
                                                   struct ges_score *gs)
{
    if (gs->gsf == ges_bic_score)
        return ges_bic_optimization1(cg, y, n, gs);
}

static void apply_optimization2(struct cgraph *cg, int x, struct ges_score *gs)
{
    if (gs->gsf == ges_bic_score)
        return ges_bic_optimization2(x, gs);
}

static void update_operator_info(struct cgraph *cg, struct ges_operator *op)
{
    free(op->parents);
    calculate_parents(cg, op);

}

static void update_insertion_operator(struct cgraph *cg, struct ges_operator *op,
                                                         struct ges_score gs)
{
    op->score_diff = DEFAULT_SCORE_DIFF;
    /* preallocate memory for cycle_created */
    int *mem = malloc(cg->n_nodes * 2 * sizeof(int));
    /* precalculate the covariances common to all calculations */
    int y = op->y;
    apply_optimization1(cg, y, cg->n_nodes, &gs);
    for (int x = 0; x < cg->n_nodes; ++x) {
        if (x == y || adjacent_in_cgraph(cg, x, y))
            continue;
        apply_optimization2(cg, x, &gs);
        struct ges_operator o = {x, y, {0}, NULL, NULL, op->parents,
                                    op->n_parents, 0, 0, INSERTION,
                                    DEFAULT_SCORE_DIFF};
        /* Split y's neighbors into set (nonadj to x) and naxy (adj to x) */
        partition_neighbors(cg, &o);
        score_insertion_operator(cg, &o, gs, mem);
        if (o.score_diff < op->score_diff) {
            free(op->set);
            free(op->naxy);
            *op = o;
        }
        else {
            free(o.set);
            free(o.naxy);
        }
    }
    free(mem);
    if (gs.gsf == ges_bic_score)
        free_ges_score_mem(gs.gsm);
}


static void update_deletion_operator(struct cgraph *cg, struct ges_operator *op,
                                                        struct ges_score gs)
{
    op->score_diff = DEFAULT_SCORE_DIFF;
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
    apply_optimization1(cg, y, cg->n_nodes, &gs);
    for (int i = 0; i < n; ++i) {
        apply_optimization2(cg, nodes[i], &gs);
        struct ges_operator o = {nodes[i], y, {0}, NULL, NULL, op->parents,
                                           op->n_parents, 0, 0, INSERTION,
                                           DEFAULT_SCORE_DIFF};
        /* Calculate the neighbors of y that are adjacent to x */
        calculate_naxy(cg, &o);
        score_deletion_operator(cg, &o, gs);
        if (o.score_diff < op->score_diff) {
            free(op->set);
            free(op->naxy);
            *op = o;
        }
        else {
            free(o.set);
            free(o.naxy);
        }
    }
    free(nodes);
    if (gs.gsf == ges_bic_score)
        free_ges_score_mem(gs.gsm);
}

/*
 * apply_insertion_operator takes the ges_operator and adds the edge x --> y, and
 * then for all nodes in s, orients node --> y.
 */
static void apply_insertion_operator(struct cgraph *cg, struct ges_operator op)
{
    add_edge_to_cgraph(cg, op.xp, op.y, DIRECTED);
    for (int i = 0; i < op.set_size; ++i) {
        if (IS_TAIL_NODE(op.t, i))
            orient_undirected_edge(cg, op.set[i], op.y);
    }
}

/*
 * apply_deletion_operator takes the ges_operator and deletes edge between x and y,
 * and then for all node in s, orients y -- > node, and x --> node if x --- y.
 * Since we do not know whether or not the edges involving x are directed or
 * undirected, we must check them.
 */
static void apply_deletion_operator(struct cgraph *cg, struct ges_operator op)
{
    if (edge_directed_in_cgraph(cg, op.xp, op.y))
        delete_edge_from_cgraph(cg, op.xp, op.y, DIRECTED);
    else
        delete_edge_from_cgraph(cg, op.xp, op.y, UNDIRECTED);
    /* orient uncovered edges */
    for (int i = 0; i < op.naxy_size; ++i) {
        /*
         * If naxy[i] is in h orient the edge between x and naxy[i] if it is
         * not already. Then, orient the undirected edge y --- naxy[i]
         */
        if (IS_HEAD_NODE(op.h, i)) {
            if (edge_undirected_in_cgraph(cg, op.xp, op.naxy[i]))
                orient_undirected_edge(cg, op.xp , op.naxy[i]);
            orient_undirected_edge(cg, op.y, op.naxy[i]);
        }
    }
}

/* TODO */
double ccf_ges(struct ges_score sc, struct cgraph *cg)
{
    /*
    * The number of processors ges is going to use. Right now it is 1,
    * but this will eventually be passed in as an argument to ges
    */
    int nprocs = 1;
    int    nvar        = cg->n_nodes;
    double graph_score = 0.0f;
    /* TODO */
    struct ges_operator *ops = calloc(nvar, sizeof(struct ges_operator));
    struct heap   *heap    = create_heap(nvar, ops, sizeof(struct ges_operator));
    double        *dscores = heap->keys;
    void         **records = heap->data;
    int           *indices = heap->indices;
    for (int i = 0; i < nvar; ++i) {
        records[i] = ops + i;
        heap->indices[i] = i;
    }
    /* FES STEP 0: For all x,y score x --> y */
    for (int y = 0; y < nvar; ++y) {
        double min_score = DEFAULT_SCORE_DIFF;
        int    x         = -1;
        apply_optimization1(cg, y, y, &sc);
        for (int i = 0; i < y; ++i) {
            double score_diff = sc.gsf(sc.df, i, y, NULL, 0, sc.args, sc.gsm);
            if (score_diff < min_score) {
                min_score = score_diff;
                x         = i;
            }
        }
        if (sc.gsf == ges_bic_score)
            free_ges_score_mem(sc.gsm);
        dscores[y]        = min_score;
        ops[y].xp         = x;
        ops[y].y          = y;
        ops[y].score_diff = min_score;
        ops[y].type       = INSERTION;
    }
    build_heap(heap);
    /* FORWARD EQUIVALENCE SEARCH (FES) */
    struct cgraph *cpy   = copy_cgraph(cg);
    int           *mem   = malloc(nvar * 2 * nprocs * sizeof(int));
    int           *nodes = mem;
    /* extract the operator with the best score from the heap */
    struct ges_operator *op;
    while ((op = peek_heap(heap)) && op->score_diff <= 0.0f) {
        if (!is_valid_insertion(cg, *op, mem)) {
            remove_heap(heap, op->y);
            update_insertion_operator(cg, op, sc);
            insert_heap(heap, op->score_diff, op);
            continue;
        }
        graph_score += op->score_diff;
        apply_insertion_operator(cg, *op);
        int n = 0;
        reorient_and_determine_operators_to_update(cpy, cg, *op, nodes, &n);
        struct ges_operator *new_ops = malloc(n * sizeof(struct ges_operator));
        for (int i = 0; i < n; ++i) {
            new_ops[i] = ops[nodes[i]];
            update_operator_info(cg, &new_ops[i]);
        }
        /* This step (updating) can be paralellized */
        for (int i = 0; i < n; ++i)
            update_insertion_operator(cg, &new_ops[i], sc);
        for (int i = 0; i < n; ++i) {
            ops[nodes[i]] = new_ops[i];
            remove_heap(heap, nodes[i]);
            insert_heap(heap, ops[nodes[i]].score_diff, &ops[nodes[i]]);
        }
        free(new_ops);
    }
    /* BES STEP 0 */
    for (int i = 0; i < nvar; ++i) {
        op       = ops + i;
        op->y    = i;
        op->type = DELETION;
        update_deletion_operator(cg, op, sc);
        records[i] = op;
        indices[i] = i;
        dscores[i] = op->score_diff;
    }
    build_heap(heap);
    /* BACKWARD EQUIVALENCE SEARCH (BES) */
    while ((op = peek_heap(heap)) && (op->score_diff <= 0.0f)) {
        if (!is_valid_deletion(cg, *op)) {
            remove_heap(heap, op->y);
            update_deletion_operator(cg, op, sc);
            insert_heap(heap, op->score_diff, op);
            continue;
        }
        graph_score += op->score_diff;
        apply_deletion_operator(cg, *op);
        int n = 0;
        reorient_and_determine_operators_to_update(cpy, cg, *op, nodes, &n);
        struct ges_operator *new_ops = malloc(n * sizeof(struct ges_operator));
        for (int i = 0; i < n; ++i) {
            new_ops[i] = ops[nodes[i]];
            update_operator_info(cg, &new_ops[i]);
        }
        /* This step (update_operator) can be parallelized */
        for (int i = 0; i < n; ++i)
            update_deletion_operator(cg, &new_ops[i], sc);
        for (int i = 0; i < n; ++i) {
            ops[nodes[i]] = new_ops[i];
            remove_heap(heap, nodes[i]);
            insert_heap(heap, ops[nodes[i]].score_diff, &ops[nodes[i]]);
        }
        free(new_ops);
    }
    /* Barney says its time to clean up */
    free(mem);
    free_heap(heap);
    free_cgraph(cpy);
    for (int i = 0; i < nvar; ++i) {
        free(ops[i].parents);
        free(ops[i].set);
        free(ops[i].naxy);
    }
    free(ops);
    return graph_score;
}
