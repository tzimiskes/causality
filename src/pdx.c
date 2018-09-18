#include <causality.h>
#include <cgraph.h>
#include <int_linked_list.h>
#include <edgetypes.h>
#include <pdx.h>

struct cll {
    struct ill **children;
    struct ill **parents;
    struct ill **spouses;
    struct cll  *next;
};

static int is_sink(struct cll *node)
 {
     return *(node->children) == NULL;
}

/*
 * clique checks each undirected parent of current if that undirected
 * parent forms a clique with all the other parents of current */
static int is_clique(struct cll *node, struct cgraph *cg)
{
    struct ill *spouses = *(node->spouses);
    /* grab a spouse (undirected adjacent) */
    while (spouses) {
        int          spouse = spouses->key;
        struct ill *parents = *(node->parents);
        /* make sure spouse is adjacent to the parents of node */
        while (parents) {
            if (!adjacent_in_cgraph(cg, spouse, parents->key))
                return 0;
            parents = parents->next;
        }
        /* make sure spouse is adjacent to the other spouses of node */
        struct ill *p = *(node->spouses);
        while (p) {
            int spouse2 = p->key;
            if (spouse2 != spouse && !adjacent_in_cgraph(cg, spouse, spouse2))
                return 0;
            p = p->next;
        }
        spouses = spouses->next;
    }
    return 1;
}

static inline void orient_in_cgraph(struct cgraph *cg, int node) {
  struct ill *spouse = cg->spouses[node];
  while (spouse) {
    orient_undirected_edge(cg, spouse->key, node);
    spouse = cg->spouses[node];
  }
}

void remove_node(struct cll *current, struct cll *nodes)
{
    int node = current - nodes; /* ptr arithemtic */
    /* delete all listings of node in its parents and spouses */
    struct ill *parents = *(current->parents);
    while (parents) {
        ill_delete(nodes[parents->key].children, node);
        parents = parents->next;
    }
    struct ill *spouses = *(current->spouses);
    while (spouses) {
        ill_delete(nodes[spouses->key].spouses, node);
        spouses = spouses->next;
    }
}

SEXP ccf_pdx_wrapper(SEXP Pdag)
{
    int           *edges_ptr = calculate_edges_ptr(Pdag);
    int            n_nodes   = length(VECTOR_ELT(Pdag,NODES));
    int            n_edges   = nrows(VECTOR_ELT(Pdag, EDGES));
    struct cgraph *cg        = create_cgraph(n_nodes);
    fill_in_cgraph(cg, n_edges, edges_ptr);
    free(edges_ptr);
    cg = ccf_pdx(cg);
    if (cg == NULL) {
        return R_NilValue;
    }
    SEXP Dag = PROTECT(duplicate(Pdag));
    recalculate_edges_from_cgraph(cg, Dag);
    free_cgraph(cg);
    UNPROTECT(1);
    return Dag;
}

struct cgraph * ccf_pdx(struct cgraph *cg)
{
    int            n_nodes = cg->n_nodes;
    struct cgraph *copy    = copy_cgraph(cg);
    struct cll    *nodes   = calloc(n_nodes, sizeof(struct cll));
    if (nodes == NULL)
        error("Failed to allocate memory for nodes in cf_extend_pdag\n");
    // set up circular linked list
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
    int         ll_size   = n_nodes;
    /* Comment needed */
    while (ll_size > 0 && n_checked <= ll_size) {
        if (is_sink(current) && is_clique(current, cg)) {
            orient_in_cgraph(copy, current - nodes);
            remove_node(current, nodes);
            prev->next = current->next;
            ll_size--;
            n_checked = 0;
        }
        else {
            n_checked++;
            prev = prev->next;
        }
        current = current->next;
    }
    free(nodes);
    free_cgraph(cg);
    /* check to see if pdx failed to generate an extension. If there is a
    * failure, free the copy_ptr and set it to NULL. */
    int failure = ll_size  > 0 ? 1 : 0;
    if (failure) {
        free_cgraph(copy);
        copy = NULL;
    }
    return copy;
}
