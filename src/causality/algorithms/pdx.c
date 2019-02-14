/* Author: Alexander Rix
 * Date  : 12/7/18
 * Description:
 * pdx.c implements the pdag extension algorithm found in
 * Dor D, Tarsi M. A simple algorithm to construct a consistent extension of a
 * partially oriented graph. Technicial Report R-185, Cognitive Systems
 * Laboratory, UCLA. 1992 Oct 23.
 */

#include <stdlib.h>

#include <causality.h>
#include <cgraph/cgraph.h>
#include <cgraph/int_linked_list.h>

/* create a circular linked list to iterate through */
struct cll {
    struct ill **children;
    struct ill **parents;
    struct ill **spouses;
    struct cll  *next;
};

/* a node is a sink if it has no children */
static int is_sink(struct cll *node)
{
     return *(node->children) == NULL;
}

/*
 * is_clique checks each undirected parent of node to see if that undirected
 * parent forms a clique with all the other parents of node
 */
static int is_clique(struct cll *node, struct cgraph *cg)
{
    struct ill *spouses = *(node->spouses);
    /* grab a spouse (undirected adjacent) */
    while (spouses) {
        int          spouse = spouses->node;
        struct ill *parents = *(node->parents);
        /* make sure spouse is adjacent to the parents of node */
        while (parents) {
            if (!adjacent_in_cgraph(cg, spouse, parents->node))
                return 0;
            parents = parents->next;
        }
        /* make sure spouse is adjacent to the other spouses of node */
        struct ill *p = *(node->spouses);
        while (p) {
            int spouse2 = p->node;
            if (spouse2 != spouse && !adjacent_in_cgraph(cg, spouse, spouse2))
                return 0;
            p = p->next;
        }
        spouses = spouses->next;
    }
    return 1;
}

/*
 * orient_in_cgraph takes all spouses of node and orients the edge bewteen
 * spouse and node towards node.
 */
static void orient_in_cgraph(struct cgraph *cg, int node)
{
    struct ill *cpy = copy_ill(cg->spouses[node]);
    struct ill *p   = cpy;
    while (p) {
        orient_undirected_edge(cg, p->node, node);
        p = p->next;
    }
    free(cpy);
}

/* remove_node deletes the node from the circular linked list, and removes all
 * edges involving node in cg (which is referenced by cll) */
static void remove_node(struct cll *current, struct cll *nodes)
{
    int node = current - nodes; /* ptr arithemtic */
    /* delete all listings of node in its parents and spouses */
    struct ill *parents = *(current->parents);
    while (parents) {
        ill_delete(nodes[parents->node].children, node);
        parents = parents->next;
    }
    struct ill *spouses = *(current->spouses);
    while (spouses) {
        ill_delete(nodes[spouses->node].spouses, node);
        spouses = spouses->next;
    }
}

/*
 * The PDag eXtension algorithm. It takes in cg and wither returns a new cgraph
 * that is the dag extension of cg, or NULL. cg will be deallocated as a apart
 * of the algorithm.
 */
struct cgraph * causality_pdx(struct cgraph *cg)
{
    int            n_nodes = cg->n_nodes;
    struct cgraph *cpy     = copy_cgraph(cg);
    if (cpy == NULL) {
        free_cgraph(cg);
        CAUSALITY_ERROR("Failed to allocate memory for cpy in ccf_pdx\n");
        return NULL;
    }
    struct cll *nodes = calloc(n_nodes, sizeof(struct cll));
    if (nodes == NULL) {
        free_cgraph(cg);
        free_cgraph(cpy);
        CAUSALITY_ERROR("Failed to allocate memory for nodes in ccf_pdx\n");
        return NULL;
    }
    /* set up circular linked list. The entries of each node refernce cg */
    struct ill **parents  = cg->parents;
    struct ill **spouses  = cg->spouses;
    struct ill **children = cg->children;
    for (int i = 0; i < n_nodes; ++i) {
        nodes[i].parents  = parents  + i;
        nodes[i].children = children + i;
        nodes[i].spouses  = spouses  + i;
        nodes[i].next     = nodes + (i + 1) % n_nodes;
    }
    struct cll *current   = nodes;
    struct cll *prev      = nodes + (n_nodes - 1);
    int         n_checked = 0;
    int         cll_size  = n_nodes;
    /* We iterate through the cll until it is empty, or we cycle through all
     * the nodes. If a node satisifies is_clique and is_sink, the node is
     * removed in cg and from the cll, n_checked is reset, and cll_size is
     * decremented. Otherwise, we increment n_checked and goto the next node.
     * After the loop condition fails we are done and either cpy is the dag
     * extension of cg, or cg does not have a dag extension.
     */
    while (cll_size > 0 && n_checked <= cll_size) {
        if (is_sink(current) && is_clique(current, cg)) {
            orient_in_cgraph(cpy, current - nodes);
            remove_node(current, nodes);
            prev->next = current->next;
            cll_size--;
            n_checked = 0;
        }
        else {
            n_checked++;
            prev = prev->next;
        }
        current = current->next;
    }
    free_cgraph(cg);
    free(nodes);
    /*
     * check to see if pdx could not generate an extension. If no extension
     * exists, free cpy and return NULL.
     */
    int extension_exists = 0;
    if (cll_size == 0)
        extension_exists = 1;
    if (!extension_exists) {
        free_cgraph(cpy);
        cpy = NULL;
    }
    return cpy;
}
