#include <causality.h>
#include <edgetypes.h>
#include <int_linked_list.h>
#include <cgraph.h>
#include <meek.h>

#ifndef DEBUG
#define DEBUG 0
#endif

/*
 * these are the four meek rules as described by meek(1995). A better
 * discussion is found in pearl(2009)
 */

int _meek1(struct cgraph *cg, int x, int y);
int _meek2(struct cgraph *cg, int x, int y);
int _meek3(struct cgraph *cg, int x, int y);
int _meek4(struct cgraph *cg, int x, int y);

void ccf_meek(struct cgraph *cg);
void apply_rule(struct cgraph *cg, int x, struct ill **stack,
                                   meek_rule _meek_rule);

SEXP ccf_meek_wrapper(SEXP Graph)
{
     int           *edges   = calculateEdgesPtr(Graph);
     int            n_nodes = length(VECTOR_ELT(Graph, NODES));
     int            n_edges = nrows(VECTOR_ELT(Graph, EDGES));
     struct cgraph *cg      = create_cgraph(n_nodes);
     fill_in_cgraph(cg, n_edges, edges);
     free(edges);
     ccf_meek(cg);
     SEXP Pattern = PROTECT(duplicate(Graph));
     calcluateEdgesFromCgraph(cg, Pattern);
     free_cgraph(cg);
     UNPROTECT(1);
     return Pattern;
}

/*
 * meek_rules take in a PDAG and maximially orients it by repeatedly applying
 * the four meek rules
 * it currently returns an updated edge matrix
 */
void ccf_meek(struct cgraph *cg)
{
    if (DEBUG)
        Rprintf("Creating stack\n");
    struct ill *stack = NULL;
    for (int i = 0; i < cg->n_nodes; ++i) {
        if (cg->spouses[i])
            stack = ill_insert_front(stack, i, 0);
    }
    struct ill *p;
    while ((p = stack)) {
        int node = p->key;
        if (DEBUG)
            Rprintf("pop %i\n", node);
        apply_rule(cg, node, &stack, _meek1);
        apply_rule(cg, node, &stack, _meek2);
        apply_rule(cg, node, &stack, _meek3);
        apply_rule(cg, node, &stack, _meek4);
        stack = stack->next;
        free(p);
    }
}

/*
 * apply_rule applied the selected meek rule (passed in by function pointer)
 * it returns 1 if the rule was applied, and 0 if not
 */
void apply_rule(struct cgraph *cg, int x, struct ill **stack,
                                   meek_rule _meek_rule)
{
    /*
     * we need to create a copy of spouses incase an edge is
     * oriented -- orientation occurs "in place."
     */
    struct ill *cpy = copy_ill(cg->spouses[x]);
    struct ill *p   = cpy;
    if(!p)
        return;
    while(p) {
        int y = p->key;
        if (_meek_rule(cg, x, y)) {
            orient_undirected_edge(cg, x, y);
            *stack = ill_insert(*stack, y, 0);
            if (DEBUG)
                Rprintf("push %i\n", y);
        }
        else if (_meek_rule(cg, y, x)) {
            orient_undirected_edge(cg, y, x);
            *stack = ill_insert(*stack, x, 0);
            if (DEBUG)
                Rprintf("push %i\n", x);
        }
        p = p->next;
    }
    ill_free(cpy);
}

/*
 * meek rule one:
 * look for chain node3 --> node1 --- node2, where !adj(node3, node2). If so,
 * orient node1 --> node2
 */
 int _meek1(struct cgraph *cg, int x, int y)
 {
     struct ill *xp = cg->parents[x];
     while (xp) {
         int z = xp->key;
         if (!adjacent_in_cgraph(cg, y, z)) {
             return 1;
         }
         xp = xp->next;
     }
     return 0;
 }

/*
 * meek rule 2: look for z such that, z --> x, y --> z. If
 * so, orient y --> x to prevent a cycle.
 */
 int _meek2(struct cgraph *cg, int x, int y)
 {
     struct ill  *xc = cg->children[x];
     while (xc) {
         int z = xc->key;
         if (edge_directed_in_cgraph(cg, z, y))
             return 1;
         xc = xc->next;
     }
     return 0;
 }

/*
 * meek rule 3: orient node1 --- node2 into node2 --> node1 when there exists
 * chains node2 --- node3 --> node1 and node2 --- node4 --> node1, with
 * !adj(node3, node4)
 */
 int _meek3(struct cgraph *cg, int x, int y)
 {
     struct ill *yp = cg->parents[y];
     while (yp) {
         int z = yp->key;
         if (!edge_undirected_in_cgraph(cg, z, x))
             goto NEXT_Y_PARENT;
         struct ill *yp_cpy = cg->parents[y];
         while (yp_cpy) {
             int w = yp_cpy->key;
             if (w != z && edge_undirected_in_cgraph(cg, w, x) && !adjacent_in_cgraph(cg, z, w)) {
                 return 1;
             }
             yp_cpy = yp_cpy->next;
         }
         NEXT_Y_PARENT: ;
         yp = yp->next;
     }
     return 0;
 }

/*
 *  meek4 rule4: orient node1 --> node2 if there is a chain
 * node4 --> node3 --> node2, node1 --- node4, with adj(node3, node1) and
 * !adj(node2, node4)
 */
 int _meek4(struct cgraph *cg, int x, int y)
 {
     struct ill *yp = cg->parents[y];
     while(yp) {
         int z = yp->key;
         if(adjacent_in_cgraph(cg, x, z)) {
             struct ill *zp = cg->parents[z];
             while(zp) {
                 int w = zp->key;
                 if(edge_undirected_in_cgraph(cg, w, x) && !adjacent_in_cgraph(cg, y, w)) {
                     return 1;
                 }
                 zp = zp->next;
             }
         }
         yp = yp->next;
     }
     return 0;
 }
