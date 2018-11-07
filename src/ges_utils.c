#include <causality.h>
#include <cgraph.h>
#include <ges_utils.h>

void free_gesrec(struct gesrec g)
{
    free(g.naxy);
    free(g.set);
}

/*
 * forms_clique checks to see if s_u_naxy forms a clique. This is the first
 * validity test for the forward equivalence search of GES, and the validity
 * test for the backward equivalence search of GES.
 */
int forms_clique(struct cgraph *cg, int *s_u_naxy, int s_u_naxy_size)
{
    for (int i = 0; i < s_u_naxy_size; ++i) {
        /*
         * Checking if nodes are adjacent in a graph is a symmetric operation,
         * so we only need j to iterate up to i - 1. adjacent_in_cgraph returns
         * 0 if the nodes are identical
         */
        for (int j = 0; j < i; ++j) {
            if (!adjacent_in_cgraph(cg, s_u_naxy[i], s_u_naxy[j])) {
                return 0;
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
int cycle_created(struct cgraph *cg, int y, int x, int *s_u_naxy,
                                     int s_u_naxy_size, int *mem)
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
    for (int i = 0; i < s_u_naxy_size; ++i)
        mark(s_u_naxy[i], marked);
    /* Grab memory from mem to use as a queue. We don't need to zero this. */
    int *queue      = mem + cg->n_nodes;
    int  queue_size = 1;
    queue[0] = y;
    for (int i = 0; i < queue_size; ++i) {
        int node = queue[i];
        /* Add the node's unmarked spouses and children to the queue */
        struct ill *p = cg->children[node];
        while (p) {
            /* If the next node is x we have found a cycle and can return 1 */
            if (p->key == x)
                return 1;
            if (!is_marked(p->key, marked)) {
                mark(p->key, marked);
                queue[queue_size++] = p->key;
            }
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            if (p->key == x)
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
    struct ill *l = cg->spouses[g->y];
    while (l) {
        if (adjacent_in_cgraph(cg, g->x, l->key))
            g->naxy_size += 1;
        else
            g->set_size += 1;
        l = l->next;
    }
    g->naxy = malloc(g->naxy_size * sizeof(int));
    g->set  = malloc(g->set_size * sizeof(int));
    /* we have to reiterate through the list */
    l = cg->spouses[g->y];
    int j = 0;
    int k = 0;
    while (l) {
        int z = l->key;
        if (adjacent_in_cgraph(cg, g->x, z))
            g->naxy[j++] = z;
        else
            g->set[k++] = z;
        l = l->next;
    }
}

int * deterimine_nodes_to_recalc(struct cgraph *cpy, struct cgraph *cg,
                                                     struct gesrec *gesrecp,
                                                     int *visited,
                                                     int n_visited,
                                                     int *n_nodes)
{
    int n = 0;
    // int x = gesrecp->x;
    int y = gesrecp->y;
    visited[y] = 1;
    for (int i = 0; i < gesrecp->set_size; ++i)
        visited[gesrecp->set[i]] = 1;
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
        if (visited[i]) {
            nodes[j] = i;
            j++;
        }
    }
    *n_nodes = n;
    free(visited);
    return nodes;
}
