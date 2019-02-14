#include <stdlib.h>

#include <causality.h>
#include <cgraph/cgraph.h>
#include <cgraph/int_linked_list.h>

struct cgraph *create_cgraph(int n_nodes)
{
    struct cgraph *cg = calloc(1,sizeof(struct cgraph));
    if (!cg) {
        CAUSALITY_ERROR("Failed to allocate memory for cgraph!\n");
        return NULL;
    }
    cg->n_nodes  = n_nodes;
    cg->parents  = create_ptr_to_ill_ptr(n_nodes);
    cg->children = create_ptr_to_ill_ptr(n_nodes);
    cg->spouses  = create_ptr_to_ill_ptr(n_nodes);
    if (!cg->parents || !cg->children || !cg->spouses)
        CAUSALITY_ERROR("Failed to allocate memory for cgraph!\n");
    return cg;
}

void fill_in_cgraph(struct cgraph *cg, int n_edges, int *edges)
{
    int         *node1     = edges;
    int         *node2     = edges + n_edges;
    int         *edge_type = edges + 2 * n_edges;
    struct ill **parents   = cg->parents;
    struct ill **children  = cg->children;
    struct ill **spouses   = cg->spouses;
    for (int i = 0; i < n_edges; ++i) {
        int x = node1[i];
        int y = node2[i];
        int edge  = edge_type[i];
        if (IS_DIRECTED(edge)) {
            children[x] = ill_insert(children[x], y, edge);
            parents[y]  = ill_insert(parents[y],  x, edge);
        }
        else {
            spouses[x]  = ill_insert(spouses[x],  y, edge);
            spouses[y]  = ill_insert(spouses[y],  x, edge);
        }
    }
    cg->n_edges = n_edges;
}

struct cgraph * copy_cgraph(struct cgraph *cg)
{
    struct cgraph  *copy          = create_cgraph(cg->n_nodes);
    struct ill    **parents       = cg->parents;
    struct ill    **spouses       = cg->spouses;
    struct ill    **children      = cg->children;
    struct ill    **copy_parents  = copy->parents;
    struct ill    **copy_children = copy->children;
    struct ill    **copy_spouses  = copy->spouses;
    for (int i = 0; i < cg->n_nodes; ++i) {
        copy_parents[i]  = copy_ill(parents[i]);
        copy_children[i] = copy_ill(children[i]);
        copy_spouses[i]  = copy_ill(spouses[i]);
    }
    copy->n_edges = cg->n_edges;
    return copy;
}

void add_edge_to_cgraph(struct cgraph *cg, int node1, int node2, int edge)
{
    struct ill **parents   = cg->parents;
    struct ill **children  = cg->children;
    struct ill **spouses   = cg->spouses;
    if (IS_DIRECTED(edge)) {
        children[node1] = ill_insert(children[node1], node2, edge);
        parents[node2]  = ill_insert(parents[node2],  node1, edge);
    }
    else {
        spouses[node1] = ill_insert(spouses[node1], node2, edge);
        spouses[node2] = ill_insert(spouses[node2], node1, edge);
    }
    cg->n_edges++;
}

void delete_edge_from_cgraph(struct cgraph *cg, int node1, int node2, int edge)
{
    struct ill **parents   = cg->parents;
    struct ill **children  = cg->children;
    struct ill **spouses   = cg->spouses;
    if (IS_DIRECTED(edge)) {
        ill_delete(&parents[node2], node1);
        ill_delete(&children[node1], node2);
    }
    else {
        ill_delete(&spouses[node2], node1);
        ill_delete(&spouses[node1], node2);
    }
    cg->n_edges -= 1;
}

void free_cgraph(struct cgraph *cg)
{
    struct ill **parents  = cg->parents;
    struct ill **children = cg->children;
    struct ill **spouses  = cg->spouses;
    for (int i = 0; i < cg->n_nodes; ++i) {
        ill_free(parents[i]);
        ill_free(children[i]);
        ill_free(spouses[i]);
    }
    free(parents);
    free(children);
    free(spouses);
    free(cg);
}

int adjacent_in_cgraph(struct cgraph *cg, int x, int y)
{
    struct ill *p = cg->parents[x];
    while (p) {
        if (p->node == y)
            return 1;
        p = p->next;
    }
    p = cg->children[x];
    while (p) {
        if (p->node == y)
            return 1;
        p = p->next;
    }
    p = cg->spouses[x];
    while (p) {
        if (p->node == y)
            return 1;
        p = p->next;
    }
    return 0;
}

int edge_undirected_in_cgraph(struct cgraph *cg, int x, int y)
{
    struct ill *p = cg->spouses[x];
    while (p) {
        if (p->node == y)
            return 1;
        p = p->next;
    }
    return 0;
}

int edge_directed_in_cgraph(struct cgraph *cg, int parent, int child)
{
    struct ill *p = cg->children[parent];
    while (p) {
        if (p->node == child)
            return 1;
        p = p->next;
    }
    return 0;
}


int identical_in_cgraphs(struct cgraph *cg1, struct cgraph *cg2, int node)
{
    struct ill *s1 = cg1->spouses[node];
    struct ill *s2 = cg2->spouses[node];
    while (s1) {
        struct ill *p = s2;
        while(p) {
            if(s1->node == p->node)
                goto S1_NEXT;
            p = p->next;
        }
        return 0;
        S1_NEXT: ;
        s1 = s1->next;
    }
    s1 = cg1->spouses[node];
    while (s2) {
        struct ill *p = s1;
        while(p) {
            if(s2->node == p->node)
                goto S2_NEXT;
            p = p->next;
        }
        return 0;
        S2_NEXT: ;
        s2 = s2->next;
    }
    struct ill *p1 = cg1->parents[node];
    struct ill *p2 = cg2->parents[node];
    while (p1) {
        struct ill *p = p2;
        while(p) {
            if(p1->node == p->node)
                goto P1_NEXT;
            p = p->next;
        }
        return 0;
        P1_NEXT: ;
        p1 = p1->next;
    }
    p1 = cg1->parents[node];
    while (p2) {
        struct ill *p = p1;
        while(p) {
            if(p2->node == p->node)
                goto P2_NEXT;
            p = p->next;
        }
        return 0;
        P2_NEXT: ;
        p2 = p2->next;
    }
    return 1;
}

void orient_undirected_edge(struct cgraph *cg, int parent, int child)
{
    struct ill *node = NULL;
    if (cg->spouses[child]->node == parent) {
        node = cg->spouses[child];
        cg->spouses[child] = cg->spouses[child]->next;
    }
    else {
        struct ill *spouses = cg->spouses[child];
        while (spouses->next) {
            if (spouses->next->node == parent) {
                node = spouses->next;
                spouses->next = spouses->next->next;
                break;
            }
            spouses = spouses->next;
        }
    }
    node->next         = cg->parents[child];
    node->edge        = DIRECTED;
    cg->parents[child] = node;
    /* now we need to do the oher */
    if (cg->spouses[parent]->node == child) {
        node = cg->spouses[parent];
        cg->spouses[parent] = cg->spouses[parent]->next;
    }
    else {
        struct ill *spouses = cg->spouses[parent];
        while (spouses->next) {
            if (spouses->next->node == child) {
                node = spouses->next;
                spouses->next = spouses->next->next;
                break;
            }
            spouses = spouses->next;
        }
    }
    node->next           = cg->children[parent];
    node->edge          = DIRECTED;
    cg->children[parent] = node;
}

void unorient_directed_edge(struct cgraph *cg, int parent, int child)
{
    struct ill *node = NULL;
    if (cg->parents[child]->node == parent) {
        node = cg->parents[child];
        cg->parents[child] = cg->parents[child]->next;
    }
    else {
        struct ill *parents = cg->parents[child];
        while (parents->next) {
            if (parents->next->node == parent) {
                node          = parents->next;
                parents->next = parents->next->next;
                break;
            }
            parents = parents->next;
        }
    }
    node->next         = cg->spouses[child];
    node->edge        = UNDIRECTED;
    cg->spouses[child] = node;
    /* now we need to do the oher */
    if (cg->children[parent]->node == child) {
        node = cg->children[parent];
        cg->children[parent] = cg->children[parent]->next;
    }
    else {
        struct ill *children = cg->children[parent];
        while (children->next) {
            if (children->next->node == child) {
                node = children->next;
                children->next = children->next->next;
                break;
            }
            children = children->next;
        }
    }
    node->next          = cg->spouses[parent];
    node->edge         = UNDIRECTED;
    cg->spouses[parent] = node;
}

void print_cgraph(struct cgraph *cg)
{
    for (int i = 0; i < cg->n_nodes; ++i) {
        struct ill *p = cg->parents[i];
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
