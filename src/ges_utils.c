#include <causality.h>
#include <cgraph.h>
#include <ges_utils.h>

#define INSERTION 1

void free_gesrec(struct gesrec g)
{
    free(g.naxy);
    free(g.set);
    free(g.parents);
}

/*
 * forms_clique checks to see if s_u_naxy forms a clique. This is the first
 * validity test for the forward equivalence search of GES, and the validity
 * test for the backward equivalence search of GES.
 */
int forms_clique(struct cgraph *cg, struct gesrec g)
{
    if (g.type == INSERTION) {
        for (int i = 0; i < g.naxy_size; ++i) {
            for (int j = 0; j < i; ++j) {
                if (!adjacent_in_cgraph(cg, g.naxy[i], g.naxy[j]))
                    return 0;
            }
            for (int j = 0; j < g.set_size; ++j) {
                if ((g.t & 1 << j) == 0)
                    continue;
                if (!adjacent_in_cgraph(cg, g.naxy[i], g.set[j]))
                    return 0;
            }
        }
        for (int i = 0; i < g.set_size; ++i) {
            if ((g.t & 1 << i) == 0)
                continue;
            for (int j = 0; j < i; ++j) {
                if ((g.t & 1 << j) == 0)
                    continue;
                if (!adjacent_in_cgraph(cg, g.set[i], g.set[j]))
                    return 0;
            }
        }
    }
    else {
        for (int i = 0; i < g.naxy_size; ++i) {
            if ((g.h & 1 << i) == 1 << i)
                continue;
            for (int j = 0; j < i; ++j) {
                if ((g.h & 1 << j) == 1 << j)
                    continue;
                if (!adjacent_in_cgraph(cg, g.naxy[i], g.naxy[j])) {
                    return 0;
                }
            }
        }
    }
    return 1;
}

static inline int is_marked(int i, unsigned char *marked) {
    int q = i / 8;
    int r = i % 8;

    return (marked[q] & 1 << r) == (1 << r);
}

static inline void mark(int i, unsigned char *marked)
{
    int q = i / 8;
    int r = i % 8;

    marked[q] = marked[q] | 1 << r;
}
/*
 * cycle_created returns whether or not the adding the edge x --> y
 * would create a cycle in cg. This is the second validity test for the
 * forward equivalence search of GES.
 */
int cycle_created(struct cgraph *cg, struct gesrec g, int *mem)
{
    /*
     * First, we grab memory from mem and use it for recording whether or not
     * a node has been marked. The bytes are intrepeted as chars to save space.
     */
    unsigned char *marked = (unsigned char *) mem;
    memset(marked, 0, cg->n_nodes/8 + 1);
    /*
     * If a path goes into s_u_naxy, it is ignored. Nodes will only be enqueued
     * if they are unmarked, so marking every node in s_u_naxy accomplishes
     * this (ie ignores the path becuase it goes into s_u_naxy).
     */
    for (int i = 0; i < g.naxy_size; ++i)
        mark(g.naxy[i], marked);
    for (int i = 0; i < g.set_size; ++i) {
        if ((g.t & 1 << i)== 1 << i)
            mark(g.set[i], marked);
    }
    /* Grab memory from mem to use as a queue. We don't need to zero this. */
    int *queue      = mem + cg->n_nodes;
    int  queue_size = 1;
    queue[0] = g.y;
    for (int i = 0; i < queue_size; ++i) {
        int node = queue[i];
        /* Add the node's unmarked spouses and children to the queue */
        struct ill *p = cg->children[node];
        while (p) {
            /* If the next node is x we have found a cycle and can return 1 */
            if (p->key == g.x)
                return 1;
            if (!is_marked(p->key, marked)) {
                mark(p->key, marked);
                queue[queue_size++] = p->key;
            }
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            if (p->key == g.x)
                return 1;
            if (!is_marked(p->key, marked)) {
                mark(p->key, marked);
                queue[queue_size++] = p->key;
            }
            p = p->next;
        }
    }
    return 0;
}

void partition_neighbors(struct cgraph *cg, struct gesrec *g)
{
    struct gesrec t = *g;
    struct ill   *s = cg->spouses[t.y];
    int           n = ill_size(s);
    g->naxy = malloc(n * sizeof(int));
    g->set  = malloc(n * sizeof(int));
    while (s) {
        if (adjacent_in_cgraph(cg, t.x, s->key))
            t.naxy[t.naxy_size++] = s->key;
        else
            t.set[t.set_size++] = s->key;
        s = s->next;
    }
    *g = t;
}

void calculate_naxy(struct cgraph *cg, struct gesrec *g)
{
    struct gesrec t = *g;
    struct ill   *s = cg->spouses[t.y];
    int           n = ill_size(s);
    g->naxy = malloc(n * sizeof(int));
    while (s) {
        if (adjacent_in_cgraph(cg, t.x, s->key))
            t.naxy[t.naxy_size++] = s->key;
        s = s->next;
    }
    *g = t;
}

int * deterimine_nodes_to_recalc(struct cgraph *cpy, struct cgraph *cg,
                                                     struct gesrec g,
                                                     int *visited,
                                                     int n_visited,
                                                     int *n_nodes)
{
    int n = 0;
    // int x = g->x;
    int y = g.y;
    visited[y] = 1;
    if (g.type == INSERTION) {
        for (int i = 0; i < g.set_size; ++i) {
            if ((g.t & 1 << i) == 1 << i)
                visited[g.set[i]] = 1;
        }
    }
    else {
        for (int i = 0; i < g.naxy_size; ++i) {
            if ((g.h & 1 << i) == 1 << i)
                visited[g.naxy[i]] = 1;
        }
    }
    for (int i = 0; i < cg->n_nodes; ++i) {
        if (!visited[i])
            continue;
        if (!identical_in_cgraphs(cg, cpy, i)) {
            ill_free(cpy->parents[i]);
            cpy->parents[i] = copy_ill(cg->parents[i]);
            ill_free(cpy->spouses[i]);
            cpy->spouses[i] = copy_ill(cg->spouses[i]);
            n++;
        }
        else
            visited[i] = 0;
    }
    int *nodes = calloc(n, sizeof(int));
    int  j     = 0;
    for (int i = 0; i < cg->n_nodes; ++i) {
        if (visited[i])
            nodes[j++] = i;
    }
    *n_nodes = n;
    free(visited);
    return nodes;
}
