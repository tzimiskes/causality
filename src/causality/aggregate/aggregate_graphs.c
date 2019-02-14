
#include <stdlib.h>

#include <causality.h>
#include <aggregate/tree.h>

static int reverse(int x, int y, int edge);

struct tree ** causality_aggregate_graphs(struct cgraph **cgs, int n_graphs,
                                          double *weights)
{
    int n_nodes = cgs[0]->n_nodes;
    struct tree **trees = calloc(n_nodes, sizeof(struct tree *));

    for (int i = 0; i < n_graphs; ++i) {
        struct cgraph *cg = cgs[i];
        double weight = weights[i];
        for (int x = 0; x < n_nodes; ++x) {
            struct ill *p = cg->parents[x];
            while (p) {
                int y    = p->node;
                int edge = p->edge;
                insert_tree(&trees[x], y, reverse(x, y, edge), weight);
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

static int reverse(int x, int y, int edge)
{
    switch (edge) {
    case DIRECTED:
        return x < y ? DIRECTED : DIRECTED_REV;
    case UNDIRECTED:
        return UNDIRECTED;
    case PLUSPLUSARROW:
        return x < y ? PLUSPLUSARROW : PLUSPLUSARROW_REV;
    case SQUIGGLEARROW:
        return x < y ? SQUIGGLEARROW : SQUIGGLEARROW_REV;
    case CIRCLEARROW:
        return x < y ? CIRCLEARROW : CIRCLEARROW_REV;
    }
    CAUSALITY_ERROR("Unrecognized edgetype in causality_aggregate_graphs!\n");
    return -9999; /* hopefully will cause a crash! */
}
