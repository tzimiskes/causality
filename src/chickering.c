#include <causality.h>
#include <cgraph.h>
#include <int_linked_list.h>
#include <edgetypes.h>
#include <sort.h>
#include <chickering.h>

#define UNKNOWN   -1
#define COMPELLED  1 /* This means directed */
#define REVERSABLE 2 /* This means undirected */

static void order_edges(struct cgraph *cg, int *sort);
static void insertion_sort(struct ill *list);
static void find_compelled(struct cgraph *cg, int *sort);

SEXP ccf_chickering_wrapper(SEXP Graph) {
    int *edges         = calculate_edges_ptr(Graph);
    int  n_nodes       = length(VECTOR_ELT(Graph, NODES));
    int  n_edges       = nrows(VECTOR_ELT(Graph, EDGES));
    struct cgraph *cg = create_cgraph(n_nodes);
    fill_in_cgraph(cg, n_edges, edges);
    free(edges);
    ccf_chickering(cg);
    SEXP Pattern = PROTECT(duplicate(Graph));
    recalculate_edges_from_cgraph(cg, Pattern);
    free_cgraph(cg);
    UNPROTECT(1);
    return Pattern;
}

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
    struct ill **parents = cg->parents;
    int          n_nodes = cg->n_nodes;
    /* can be parallelized */
    for (int i = 0; i < n_nodes; ++i) {
        ill_ptr tmp = parents[i];
        while (tmp) {
            tmp->value = sort[tmp->key];
            tmp        = tmp->next;
        }
        insertion_sort(parents[i]);
    }
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
        int         y         = sort[i];
        struct ill *y_parents = parents[y];
        /* if y has no incident edges, go to the next node in the order */
        if (y_parents == NULL)
            continue;
        /* Since y has parents, run stepts 5-8 */
        int         x         = y_parents->key;
        struct ill *x_parents =  parents[x];
        /*
         * for each parent of x, w, where w -> x is compelled
         * check to see if w forms a chain (w -> x -> y)
         * or shielded collider (w -> x -> y and w -> x)
         */
        while (x_parents) { /* STEP 5 */
            if (x_parents->value != COMPELLED)
                goto NEXT;
            int w = x_parents->key;
            /* if true , w --> y , x;  x--> y form a shielded collider */
            if (edge_directed_in_cgraph(cg, w, y)) {
                x_parents->value = COMPELLED;
            }
            /* otherwise it is a chain and parents of y are compelled */
            else {
                struct ill *p = y_parents;
                while (p) {
                    p->value = COMPELLED;
                    p        = p->next;
                }
                goto EOFL; /* goto end of for loop */
            }
            /* if step 7 is executed or w isn't compelled, goto the next parent of x */
            NEXT: ;
            x_parents = x_parents->next;
        }
        /* now, we need to search for z, where z -> y, x != z,
        * and z is not a parent of x. That is, an unshielded collider
        * by starting at the second parent (might not exist),
        * we avoid the need to check to see if z = x
        * STEP 7.5: look for an unshielded collider */
        int unshielded_collider = 0;
        struct ill *p = parents[y];
        while (p) {
            int z = p->key;
            if (z != x && !edge_directed_in_cgraph(cg, z, x)) {
                unshielded_collider = 1;
                goto STEP_89;
            }
            p = p->next;
        }
        STEP_89: {};
        /* STEP 8: if there is an unshielded collider,
        * label all incident edges compelled */
        p = parents[y];
        if (unshielded_collider) {
            while (p) {
                p->value = COMPELLED;
                p = p->next;
            }
        }
        /* STEP 9, label all unknown edges reversable */
        else {
            while (p) {
                if (p->value == UNKNOWN) {
                    unorient_directed_edge(cg, p->key, y);
                    p = parents[y];
                }
                else
                    p = p->next;
            }
        }
        EOFL:{}; /* End Of For Loop */
    }
}
