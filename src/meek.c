#include <causality.h>
#include <edgetypes.h>
#include <int_linked_list.h>
#include <cgraph.h>

#define RULE_APPLIED 1
#define NO_RULE_APPLIED 0
void ccf_meek(struct cgraph *cg);

SEXP ccf_meek_wrapper(SEXP Graph)
{
     int * edges_ptr   = calculate_edges_ptr(Graph);
     int n_nodes       = length(VECTOR_ELT(Graph, NODES));
     int n_edges       = nrows(VECTOR_ELT(Graph, EDGES));
     /* create an empty cgraph and fill it in */
     struct cgraph *cg = create_cgraph(n_nodes);
     fill_in_cgraph(cg, n_edges, edges_ptr);
     free(edges_ptr);
     ccf_meek(cg);
     SEXP Pattern = PROTECT(duplicate(Graph));
     recalculate_edges_from_cgraph(cg, Pattern);
     free_cgraph(cg);
     UNPROTECT(1);
     return Pattern;
}

/* these are the four meek rules as described by meek(1995). A better discussion
 *  is found in pearl(2009)
 * each rule is described where it is implemented
 * these functions either return ORIENT, FLIP, or UNORIENTABLE
 */
static int meek1(struct cgraph *cg, int x, int y);
static int meek2(struct cgraph *cg, int x, int y);
static int meek3(struct cgraph *cg, int x, int y);
static int meek4(struct cgraph *cg, int x, int y);

/*
 * apply_rule applied the selected meek rule (passed in by function pointer)
 * it returns 1 if the rule was applied, and 0 if not
 */

/*
 * meek_rules take in a PDAG and maximially orients it by repeatedly applying
 * the four meek rules
 * it currently returns an updated edge matrix
 */
void ccf_meek(struct cgraph *cg)
{
    int n_nodes       = cg->n_nodes;
    struct ill ** spouses = cg->spouses;
    int rule_applied;
    do {
        rule_applied = 0;
        for(int i = 0; i < n_nodes; ++i) {
            TOP: {}
            struct ill *spouse = spouses[i];
            while(spouse) {
                if(meek1(cg, i, spouse->key)) {
                    rule_applied++;
                    goto TOP;
                }

                else if(meek2(cg, i, spouse->key)) {
                    rule_applied++;
                    goto TOP;
                }
                else if(meek3(cg, i, spouse->key)) {
                    rule_applied++;
                    goto TOP;
                }
                else if(meek4(cg, i, spouse->key)) {
                    rule_applied++;
                    goto TOP;
                }
                else {
                    spouse = spouse->next;
                }
            }
        }
        R_CheckUserInterrupt();
    } while(rule_applied);
}

/*
 * meek rule one:
 * look for chain node3 --> node1 --- node2, where !adj(node3, node2). If so,
 * orient node1 --> node2
 *
 * Reverse case: node3 --> node1, ! adj(node3, node1); orient node2 --> node1
 */
static int meek1(struct cgraph *cg, int x, int y) {
    struct ill **parents = cg->parents;
    struct ill  *x_par   = parents[x];
    while(x_par) {
        if(!adjacent_in_cgraph(cg, x_par->key, y)) {
            orient_undirected_edge(cg, x, y);
            return RULE_APPLIED;
        }
        x_par = x_par->next;
    }
    /* if we are here, we now look at the parents of node2 instead of node1 */
    struct ill *y_par = parents[y];
    while(y_par) {
        if(!adjacent_in_cgraph(cg, x, y_par->key)) {
            orient_undirected_edge(cg, y, x);
            return RULE_APPLIED;
        }
        y_par = y_par->next;
    }
    return NO_RULE_APPLIED;
}

/*
 * meek rule 2: look for z such that, z --> x, y --> z. If
 * so, orient y --> x to prevent a cycle.
 *
 * In the reverse case, look for node3 --> node2, node1 --> node3, so that we
 * orient node1 --> node2
 */
static int meek2(struct cgraph *cg, int x, int y) {
    struct ill **parents = cg->parents;
    struct ill  *x_par     = parents[x];
    // look for node3 --> node1
    while(x_par) {
        int z         = x_par->key;
        struct ill *z_par = parents[z];
        // look for node2 --> node3
        while(z_par) {
            if(z_par->key == y) {
                orient_undirected_edge(cg, y, x);
                return RULE_APPLIED;
            }
            z_par = z_par->next;
        }
        x_par = x_par->next;
    }
    // if we are here, we now look at the parents of node2 instead of node1
    struct ill *y_par = parents[y];
    while(y_par) {
        int z = y_par->key; /* found it */
        struct ill *z_par = parents[z];
        while(z_par) {
            if(z_par->key == x) {
                orient_undirected_edge(cg, x, y);
                return RULE_APPLIED;
            }
            z_par = z_par->next;
        }
        y_par = y_par->next;
    }
    return NO_RULE_APPLIED;
}

/*
 * meek rule 3: orient node1 --- node2 into node2 --> node1 when there exists
 * chains node2 --- node3 --> node1 and node2 --- node4 --> node1, with
 * !adj(node3, node4)
 *
 * reverse case: chains node1 --- node3 --> node2 and node1 --- node4 --> node2,
 * with !adj(node3, node4)
 */
static int meek3(struct cgraph *cg, int x, int y) {
    struct ill ** parents = cg->parents;
    // look for node3 --> node1
    struct ill *x_par    = parents[x];
    while(x_par) {
        int z = x_par->key; /* found node3 */
        // look for node4 --> node1
        struct ill *x_par_cpy = parents[x];
        while(x_par_cpy) {
            if(x_par_cpy->key != z) {
                int w = x_par_cpy->key;
                // check to see if they are adjacent
                if(!adjacent_in_cgraph(cg, w, z)) {
                    // if they are not, look for node2 -- node3, node2 -- node4
                    if(edge_undirected_in_cgraph(cg, z, y) && edge_undirected_in_cgraph(cg, w, y)) {
                        orient_undirected_edge(cg, y, x);
                        return RULE_APPLIED;
                    }
                }
            }
            x_par_cpy = x_par_cpy->next;
            }
        x_par = x_par->next;
        }
    // now we look through the parents of node2 instead of node1
    struct ill *y_par = parents[y];
    while(y_par) {
        int z = y_par->key;
        // look for node4 --> node1
        struct ill *y_par_cpy = parents[y];
        while(y_par_cpy) {
            if(y_par_cpy->key != z) {
                int w = y_par_cpy->key;
                // check to see if they are adjacent
                if(!adjacent_in_cgraph(cg, z, w)) {
                    // if they are not, look for node1 -- node3, node1 -- node4
                    if(edge_undirected_in_cgraph(cg, z, x) && edge_undirected_in_cgraph(cg, w, x)) {
                        orient_undirected_edge(cg, x , y);
                        return RULE_APPLIED;
                    }
                }
            }
            y_par_cpy = y_par_cpy->next;
            }
        y_par = y_par->next;
        }
    return NO_RULE_APPLIED;
}

/*
 *  meek4 rule4: orient node1 --> node2 if there is a chain
 * node4 --> node3 --> node2, node1 --- node4, with adj(node3, node1) and
 * !adj(node2, node4)
 */
static int meek4(struct cgraph *cg, int x, int y) {
    struct ill ** parents = cg->parents;
    struct ill *x_par     = parents[x];
    while(x_par) {
        int z = x_par->key;
        if(adjacent_in_cgraph(cg, y, z)) {
            struct ill *z_par = parents[z];
            while(z_par) {
                int w = z_par->key;
                if(edge_undirected_in_cgraph(cg, w, y) && !adjacent_in_cgraph(cg, x, w)) {
                    orient_undirected_edge(cg, y, x);
                    return RULE_APPLIED;
                }
                z_par = z_par->next;
            }
        }
        x_par = x_par->next;
    }
    // now we look through the parents of node2 instead of node1
    struct ill *y_par = parents[y];
    while(y_par) {
        int z = y_par->key;
        if(adjacent_in_cgraph(cg, x, z)) {
            struct ill *z_par = parents[z];
            while(z_par) {
                int w = z_par->key;
                if(edge_undirected_in_cgraph(cg, w, x) && !adjacent_in_cgraph(cg, y, w)) {
                    orient_undirected_edge(cg, x , y);
                    return RULE_APPLIED;
                }
                z_par = z_par->next;
            }
        }
        y_par = y_par->next;
    }
    return NO_RULE_APPLIED;
}
