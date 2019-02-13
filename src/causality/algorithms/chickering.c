/* Author: Alexander Rix
 * Date  : 11/30/18
 * Description:
 * chickering.c implements a function to convert directed acyclic graphs into
 * patterns. The algorithm is described in Chickering's paper
 * "A Transformational Characterization of Equivalent Bayesian Network
 * Structures", avaliable on the arxiv: https://arxiv.org/abs/1302.4938
 */

#include <stdlib.h>

#include <causality.h>
#include <cgraph/cgraph.h>
#include <cgraph/int_linked_list.h>

#define UNKNOWN   -1
#define COMPELLED  DIRECTED
#define REVERSABLE UNDIRECTED

static void order_edges(struct cgraph *cg, int *sort);
static void insertion_sort(struct ill *list);
static void find_compelled(struct cgraph *cg, int *sort);

void ccf_chickering(struct cgraph *cg)
{
    int *sort = ccf_sort(cg);
    order_edges(cg, sort);
    find_compelled(cg, sort);
    free(sort);
}

/*
 * order_edges orders the parents of cg such that the nodes are in descending
 * order according to the sort.
 */
static void order_edges(struct cgraph *cg, int *sort)
{
    int  n_nodes  = cg->n_nodes;
    int *inv_sort = malloc(n_nodes * sizeof(int));
    if (inv_sort == NULL) {
        CAUSALITY_ERROR("Failed to allocate memory in order edges\n");
    }
    for (int i = 0; i < n_nodes; ++i)
        inv_sort[sort[i]] = i;
    /*
     * Replace the value at each edge with the parent's location in the sort.
     * Then, sort (in place) so that the edges are in descending order.
     * This isn't a problem because in force compelled all the values will be
     * declared unknown.
     */
    struct ill **parents = cg->parents;
    for (int i = 0; i < n_nodes; ++i) {
        struct ill *p = parents[i];
        while (p) {
            p->value = inv_sort[p->key];
            p        = p->next;
        }
        insertion_sort(parents[i]);
    }
    free(inv_sort);
}

/*
 * We need a sorting routine so we can order the edges. Typically, we would use
 * a mergesort routine for linked lists, but I suspect insertion sort will be
 * faster because the average degree of causal graphs is 2-5, and insertion sort
 * is faster than merge sort until we hit 10-50 elements.
 */
static void insertion_sort(struct ill *list)
{
    while (list) {
        struct ill *top = list;
        struct ill *max = list;
        while (top) {
            if (top->value > max->value)
                max = top;
            top = top->next;
        }
        int list_key   = list->key;
        int list_value = list->value;
        list->key      = max->key;
        list->value    = max->value;
        max->key       = list_key;
        max->value     = list_value;
        list           = list->next;
    }
}

static void find_compelled(struct cgraph *cg, int *sort)
{
    struct ill **parents = cg->parents;
    int          n_nodes = cg->n_nodes;
    /*
     * order edges sets the value parameter for each edge, so we need to
     * change the value for everything to UNKNOWN
     */
    for (int i = 0; i < n_nodes; ++i) {
        struct ill *p = parents[i];
        while (p) {
            p->value = UNKNOWN;
            p        = p->next;
        }
    }
    /*
     * we iterate through the sort to satisfy the max min condition
     * necessary to run this part of the algorithm
     */
    for (int i = 0; i < n_nodes; ++i) {
        /* by lemma 5 in Chickering, all the incident edges on y are unknown
         * so we don't need to check to see its unordered */
        int         y  = sort[i];
        struct ill *yp = parents[y];
        /* if y has no incident edges, go to the next node in the order */
        if (!yp)
            continue;
        /* Since y has parents, run stepts 5-8 */
        int         x  = yp->key;
        struct ill *xp = parents[x];
        /*
         * for each parent of x, w, where w -> x is compelled
         * check to see if w forms a chain (w -> x -> y)
         * or shielded collider (w -> x -> y and w -> x)
         */
        while (xp) { /* STEP 5 */
            if (xp->value != COMPELLED)
                goto NEXT;
            int w = xp->key;
            /* if true , w --> y , x;  x--> y form a shielded collider */
            if (edge_directed_in_cgraph(cg, w, y)) {
                struct ill* p = ill_search(parents[y], w);
                p->value = COMPELLED;
            }
            /* otherwise it is a chain and parents of y are compelled */
            else {
                struct ill *p = parents[y];
                while (p) {
                    p->value = COMPELLED;
                    p        = p->next;
                }
                goto EOFL; /* goto end of for loop */
            }
            NEXT: ;
            xp = xp->next;
        }
        /*
         * now, we need to search for z, where z -> y, x != z, and z is not a
         * parent of x. That is, an unshielded collider.
         */
        int unshielded_collider = 0;
        struct ill *p = parents[y];
        while (p) {
            int z = p->key;
            if (z != x && !adjacent_in_cgraph(cg, z, x)) {
                unshielded_collider = 1;
                break;
            }
            p = p->next;
        }
        /* if there is an unshielded collider, label all parents compelled */
        if (unshielded_collider) {
            p = parents[y];
            while (p) {
                p->value = COMPELLED;
                p        = p->next;
            }
        }
        /* otherwise, label all unknown edges reversable */
        else {
            /*
             * we need to create cpy because unorient_directed_edge operates in
             * place, which would mess the parents[y] pointer
             */
            struct ill *cpy = copy_ill(parents[y]);
            p = cpy;
            while (p) {
                if (p->value == UNKNOWN)
                    unorient_directed_edge(cg, p->key, y);
                p = p->next;
            }
            free(cpy);
        }
        EOFL: ;
    }
}
