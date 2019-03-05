/* Author: Alexander Rix
 * Date  : 1/14/2019
 * Description: ges_utils.c contains various utility functions for ges. These
 * functions would clutter up ges.c, so they are defined here instead. There is
 * a small performance hit in not making these static, but it should be (very)
 * small compared to scoring.
 */

#include <stdlib.h>
#include <string.h>

#include <cgraph/cgraph.h>
#include <ges/ges.h>
#include <ges/ges_internal.h>

void free_ges_score_mem(struct ges_score_mem gsm)
{
    free(gsm.lbls);
    free(gsm.cov_xy);
    free(gsm.cov_xx);
    free(gsm.cov_xpx);
}

/*
 * valid_fes_clique checks to see if the set TAIL U nayx constructed from
 * the given insertion operator forms a clique.
 */
int valid_fes_clique(struct cgraph *cg, struct ges_operator *op)
{
    struct ges_operator o = *op;
    for (int i = 0; i < o.nayx_size; ++i) {
        /* check to see if nayx forms a clique */
        for (int j = 0; j < i; ++j) {
            if (!adjacent_in_cgraph(cg, o.nayx[i], o.nayx[j]))
                return 0;
        }
        /* check to see is every nayx is adj to every tail */
        for (int j = 0; j < o.set_size; ++j) {
            /* If not in TAIL skip */
            if (!IS_TAIL_NODE(o.t, j))
                continue;
            if (!adjacent_in_cgraph(cg, o.nayx[i], o.set[j]))
                return 0;
        }
    }
    /* check to see if TAIL forms a clique */
    for (int i = 0; i < o.set_size; ++i) {
        /* If not in TAIL skip */
        if (!IS_TAIL_NODE(o.t, i))
            continue;
        for (int j = 0; j < i; ++j) {
            if (IS_TAIL_NODE(o.t, j)) {
                if (!adjacent_in_cgraph(cg, o.set[i], o.set[j]))
                    return 0;
            }
        }
    }
    return 1;
}
/*
 * valid_fes_clique checks to see if the set nayx/H
 * forms a clique.
 */
int valid_bes_clique(struct cgraph *cg, struct ges_operator *op)
{
    struct ges_operator o = *op;
    for (int i = 0; i < o.nayx_size; ++i) {
        /* if i in H, skip */
        if (IS_HEAD_NODE(o.h, i))
            continue;
        for (int j = 0; j < i; ++j) {
            /* if j in H, skip */
            if (!IS_HEAD_NODE(o.h, j)) {
                if (!adjacent_in_cgraph(cg, o.nayx[i], o.nayx[j]))
                    return 0;
            }
        }
    }
    return 1;
}

/*
 * is_marked and marked use bit operations to save memory,thus making the
 * memset operation in cycle_created faster).
 */
static inline int is_marked(int i, unsigned char *marked)
{
    unsigned char j = 0x01 << (i % 8);
    return (marked[i / 8] & j) == j;
}

static inline void mark(int i, unsigned char *marked)
{
    int q = i / 8;
    unsigned char j = 0x01 << (i % 8);
    marked[q] = marked[q] | j;
}

/*
 * cycle_created returns whether or not the adding the edge x --> y
 * would create a cycle in cg. This is the second validity test for the
 * forward equivalence search of GES. mem is passed in as an optimization.
 */
int cycle_created(struct cgraph *cg, struct ges_operator *op, int *mem)
{
    /*
     * First, we grab memory from mem and use it for recording whether or not
     * a node has been marked. The bytes are intrepeted as chars to save space.
     */
    unsigned char *marked = (unsigned char *) mem;
    memset(marked, 0, cg->n_nodes / 8 + 1);
    /*
     * If a path goes into s_u_nayx, it is ignored. Nodes will only be enqueued
     * if they are unmarked, so marking every node in s_u_nayx accomplishes
     * this (ie ignores the path because it goes into s_u_nayx).
     */
    for (int i = 0; i < op->nayx_size; ++i)
        mark(op->nayx[i], marked);
    for (int i = 0; i < op->set_size; ++i) {
        if (IS_TAIL_NODE(op->t, i))
            mark(op->set[i], marked);
    }
    /* Grab memory from mem to use as a queue. We don't need to zero this. */
    int *queue = mem + cg->n_nodes;
    /* queue_size is bounded by n_nodes, as we only ever add a node once */
    int size = 1;
    queue[0] = op->y;
    for (int i = 0; i < size; ++i) {
        int node = queue[i];
        /* Add the node's unmarked spouses and children to the queue */
        struct edge_list *p = cg->children[node];
        while (p) {
            /* If the next node is x we have found a cycle and can return 1 */
            if (p->node == op->xp)
                return 1;
            if (!is_marked(p->node, marked)) {
                mark(p->node, marked);
                queue[size++] = p->node;
            }
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            if (p->node == op->xp)
                return 1;
            if (!is_marked(p->node, marked)) {
                mark(p->node, marked);
                queue[size++] = p->node;
            }
            p = p->next;
        }
    }
    return 0;
}

/*
 * partition_neighbors partitions the neighbors of op.y into those adjacent
 * to opx (nayx) in cg and those nonadjacent to op.x (set). Used in FES.
 */
void partition_neighbors(struct cgraph *cg, struct ges_operator *op)
{
    struct ges_operator o = *op;
    struct edge_list   *s = cg->spouses[o.y];
    int n = size_edge_list(s);
    o.nayx      = malloc(n * sizeof(int));
    o.nayx_size = 0;
    o.set       = malloc(n * sizeof(int));
    o.set_size  = 0;
    while (s) {
        if (adjacent_in_cgraph(cg, o.xp, s->node))
            o.nayx[o.nayx_size++] = s->node;
        else
            o.set[o.set_size++] = s->node;
        s = s->next;
    }
    *op = o;
}

/*
 * calculate_nayx caculates the neighbors (spouses) of op.y that are adjacent
 * to op.x. Used in BES.
 */
void calculate_nayx(struct cgraph *cg, struct ges_operator *op)
{
    struct ges_operator o = *op;
    struct edge_list *s = cg->spouses[o.y];
    o.nayx_size = 0;
    o.set_size  = 0;
    o.nayx      = malloc(size_edge_list(s) * sizeof(int));
    o.set       = NULL;
    while (s) {
        if (adjacent_in_cgraph(cg, o.xp, s->node))
            o.nayx[o.nayx_size++] = s->node;
        s = s->next;
    }
    *op = o;
}

/*
 * calculate_parents does what it says.
 */
void calculate_parents(struct cgraph *cg, struct ges_operator *op)
{
    struct edge_list *p = cg->parents[op->y];
    op->n_parents = size_edge_list(p);
    op->parents   = malloc(op->n_parents * sizeof(int));
    int i = 0;
    while (p) {
        op->parents[i++] = p->node;
        p = p->next;
    }
}

/*
* reorient_and_determine_operators_to_update reorients the cgraph after
* applying a ges operator (insertion or deletion). It also passes along the
* information from the reorient functions in ges_reorient.c to
* deterimine_nodes_to_recalc to get the nodes and the number of nodes
* we need to recalculate.
*/
int get_insertion_operators_to_update(int *nodes, struct cgraph *cpy,
                                          struct cgraph *cg, struct ges_operator
                                          *op, int *visited)
{
    int n_nodes = cg->n_nodes;
    int n = 0;
    visited[op->y]  = 1;
    visited[op->xp] = 1;
    for (int i = 0; i < op->set_size; ++i) {
        if (IS_TAIL_NODE(op->t, i))
                visited[op->set[i]] = 1;
    }
    for (int i = 0; i < n_nodes; ++i) {
        if (!visited[i])
            continue;
        if (!identical_in_cgraphs(cg, cpy, i)) {
            free_edge_list(cpy->parents[i]);
            cpy->parents[i] = copy_edge_list(cg->parents[i]);
            free_edge_list(cpy->spouses[i]);
            cpy->spouses[i] = copy_edge_list(cg->spouses[i]);
            nodes[n++] = i;
        }
    }
    return n;
}

int get_deletion_operators_to_update(int *nodes, struct cgraph *cpy,
                                         struct cgraph *cg, struct ges_operator
                                         *op, int *visited)
{
    int  n_nodes = cg->n_nodes;
    int  n       = 0;
    visited[op->y]  = 1;
    visited[op->xp] = 1;
    for (int i = 0; i < op->nayx_size; ++i) {
        if (IS_HEAD_NODE(op->h, i))
            visited[op->nayx[i]] = 1;
    }
    for (int i = 0; i < n_nodes; ++i) {
        if (!visited[i])
            continue;
        if (!identical_in_cgraphs(cg, cpy, i)) {
            size_edge_list(cpy->parents[i]);
            cpy->parents[i] = copy_edge_list(cg->parents[i]);
            size_edge_list(cpy->spouses[i]);
            cpy->spouses[i] = copy_edge_list(cg->spouses[i]);
            n++;
        }
        else
            visited[i] = 0;
    }
    int j = 0;
    for (int i = 0; i < n_nodes; ++i) {
        if (visited[i])
            nodes[j++] = i;
    }
    return n;
}
