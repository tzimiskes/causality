/* Author: Alexander Rix
 * Date  : 2/19/2019
 * Description: aggregate_graphs.c implements an algorithm to add multiple
 * graphs together to create a single object that can be used to analyse the
 * stability of graph learning algorithms, and also be used to implement
 * bootstraping etc.
 */

#include <stdlib.h>

#include <causality.h>
#include <aggregate/tree.h>

static int reverse(int edge);

/*
* causality_aggregate_graphs takes in a list of cgraphs and weights and turns
* them into a tree structure that can be converted into a graph or a matrix.
* each edge in each graph is added to the tree. some edges will be reversed
* (eg --> might become <--) to keep the table from becoming too long. This also
* makes it easier to see if there's a preferred direction to edges. The reversal
* is achieved by checking if x < y in the edge x --> y. Undirected edges are
* only added when x < y to prevent double counting.
*/
struct tree ** causality_aggregate_graphs(struct cgraph **cgs, double *weights,
                                                               int n_graphs)
{
    int n_nodes = cgs[0]->n_nodes;
    struct tree **trees = calloc(n_nodes, sizeof(struct tree *));
    for (int i = 0; i < n_graphs; ++i) {
        struct cgraph *cg = cgs[i];
        double weight     = weights[i];
        for (int x = 0; x < n_nodes; ++x) {
            struct edge_list *p = cg->parents[x];
            while (p) {
                int y    = p->node;
                int edge = p->edge;
                if (x < y)
                    insert_tree(&trees[y], x, edge, weight);
                else
                    insert_tree(&trees[x], y, reverse(edge), weight);
                p = p->next;
            }
            p = cg->spouses[x];
            while (p) {
                int y    = p->node;
                int edge = p->edge;
                if (x < y)
                    insert_tree(&trees[x], y, edge, weight);
                p = p->next;
            }
        }
        free_cgraph(cg);
    }
    free(cgs);
    return trees;
}

static int reverse(int edge)
{
    switch (edge) {
    case DIRECTED:
        return DIRECTED_REV;
    case PLUSPLUSARROW:
        return PLUSPLUSARROW_REV;
    case SQUIGGLEARROW:
        return SQUIGGLEARROW_REV;
    case CIRCLEARROW:
        return CIRCLEARROW_REV;
    }
    CAUSALITY_ERROR("Unrecognized edgetype in causality_aggregate_graphs!\n");
    return edge; /* hopefully will cause a crash! */
}
