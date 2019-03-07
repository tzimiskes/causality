#include <stdlib.h>

#include <causality.h>
#include <cgraph/cgraph.h>
#include <cgraph/edge_list.h>

struct cgraph *create_cgraph(int n_nodes)
{
    struct cgraph *cg = malloc(sizeof(struct cgraph));
    if (!cg) {
        CAUSALITY_ERROR("Failed to allocate memory for cgraph!\n");
        return NULL;
    }
    cg->n_edges  = 0;
    cg->n_nodes  = n_nodes;
    cg->parents  = calloc(n_nodes, sizeof(struct edge_list *));
    cg->children = calloc(n_nodes, sizeof(struct edge_list *));
    cg->spouses  = calloc(n_nodes, sizeof(struct edge_list *));
    if (!cg->parents || !cg->children || !cg->spouses)
        CAUSALITY_ERROR("Failed to allocate memory for cgraph!\n");
    return cg;
}

struct cgraph * copy_cgraph(struct cgraph *cg)
{
    struct cgraph  *copy          = create_cgraph(cg->n_nodes);
    for (int i = 0; i < cg->n_nodes; ++i) {
        copy->parents[i]  = copy_edge_list(cg->parents[i]);
        copy->children[i] = copy_edge_list(cg->children[i]);
        copy->spouses[i]  = copy_edge_list(cg->spouses[i]);
    }
    copy->n_edges = cg->n_edges;
    return copy;
}

void add_edge_to_cgraph(struct cgraph *cg, int x, int y, short edge)
{
    if (IS_DIRECTED(edge)) {
        insert_edge(&cg->children[x], y, edge, 0);
        insert_edge(&cg->parents[y], x, edge, 0);
    }
    else {
        insert_edge(&cg->spouses[x], y, edge, 0);
        insert_edge(&cg->spouses[y], x, edge, 0);
    }
    cg->n_edges += 1;
}

void delete_edge_from_cgraph(struct cgraph *cg, int x, int y, short edge)
{
    if (IS_DIRECTED(edge)) {
        remove_edge(&cg->parents[y], x);
        remove_edge(&cg->children[x], y);
    }
    else {
        remove_edge(&cg->spouses[y], x);
        remove_edge(&cg->spouses[x], y);
    }
    cg->n_edges -= 1;
}

void free_cgraph(struct cgraph *cg)
{
    struct edge_list **parents  = cg->parents;
    struct edge_list **children = cg->children;
    struct edge_list **spouses  = cg->spouses;
    for (int i = 0; i < cg->n_nodes; ++i) {
        free_edge_list(parents[i]);
        free_edge_list(children[i]);
        free_edge_list(spouses[i]);
    }
    free(parents);
    free(children);
    free(spouses);
    free(cg);
}

int adjacent_in_cgraph(struct cgraph *cg, int x, int y)
{
    struct edge_list *e = cg->parents[x];
    while (e) {
        if (e->node == y)
            return 1;
        e = e->next;
    }
    e = cg->children[x];
    while (e) {
        if (e->node == y)
            return 1;
        e = e->next;
    }
    e = cg->spouses[x];
    while (e) {
        if (e->node == y)
            return 1;
        e = e->next;
    }
    return 0;
}

int edge_undirected_in_cgraph(struct cgraph *cg, int x, int y)
{
    struct edge_list *e = cg->spouses[x];
    while (e) {
        if (e->node == y)
            return 1;
        e = e->next;
    }
    return 0;
}

int edge_directed_in_cgraph(struct cgraph *cg, int x, int y)
{
    struct edge_list *e = cg->children[x];
    while (e) {
        if (e->node == y)
            return 1;
        e = e->next;
    }
    return 0;
}

int identical_in_cgraphs(struct cgraph *cg1, struct cgraph *cg2, int node)
{
    if (!identical_edge_lists(cg1->children[node], cg2->children[node]))
        return 0;
    if (!identical_edge_lists(cg1->parents[node], cg2->parents[node]))
        return 0;
    if (!identical_edge_lists(cg1->spouses[node], cg2->spouses[node]))
        return 0;
    return 1;
}

void orient_undirected_edge(struct cgraph *cg, int x, int y)
{
    delete_edge_from_cgraph(cg, x, y, UNDIRECTED);
    add_edge_to_cgraph(cg, x, y, DIRECTED);
}

void unorient_directed_edge(struct cgraph *cg, int x, int y)
{
    delete_edge_from_cgraph(cg, x, y, DIRECTED);
    add_edge_to_cgraph(cg, x, y, UNDIRECTED);
}

void print_cgraph(struct cgraph *cg)
{
    for (int i = 0; i < cg->n_nodes; ++i) {
        struct edge_list *p = cg->parents[i];
        while (p) {
            CAUSALITY_PRINT("%i --> %i\n", p->node, i);
            p = p->next;
        }
        p = cg->spouses[i];
        while (p) {
            if (p->node < i)
                CAUSALITY_PRINT("%i --- %i\n", p->node, i);
            p = p->next;
        }
    }
}
