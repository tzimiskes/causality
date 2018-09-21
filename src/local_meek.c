#include <causality.h>
#include <edgetypes.h>
#include <int_linked_list.h>
#include <cgraph.h>

struct pll {
    struct ill *p;
    struct pll *next;
};

typedef int (*meek_rule)(struct cgraph *cg, int x, int y);


static void undirect_reversible_parents(int node, struct cgraph *cg,
                                                struct ill **stack,
                                                struct pll **compelled,
                                                int *visited,  int *n_visited);

static void push_adjacents(int node, struct cgraph *cg, struct ill **stack);

void apply_rule(struct cgraph *cg, int x, struct ill **stack,
                                   struct pll **compelled, int *visited,
                                   int *n_visited, meek_rule _meek_rule);

int _meek1(struct cgraph *cg, int x, int y);
int _meek2(struct cgraph *cg, int x, int y);
int _meek3(struct cgraph *cg, int x, int y);
int _meek4(struct cgraph *cg, int x, int y);

void meek_rules(struct cgraph *cg, int x, struct ill **stack,
                                     struct pll **compelled, int *visited,
                                     int *n_visited);

void orient(struct cgraph *cg, int x, int y, struct ill **stack,
    struct pll **compelled, int *visited,
    int *n_visited);

void insert_pll(struct pll **p, struct ill *l)
{
    struct pll *t = malloc(sizeof(struct pll));
    t->p    = l;
    t->next = NULL;
    if(*p)
        t->next = *p;
    *p = t;
}

int * meek_local(struct cgraph *cg, int *nodes, int n_nodes, int * n_visited)
{
    struct pll *compelled = NULL;
    struct ill *stack     = NULL;
    int        *visited   = calloc(cg->n_nodes, sizeof(int));
    for (int i = 0; i < n_nodes; ++i) {
       undirect_reversible_parents(nodes[i], cg, &stack, &compelled, visited,
                                           n_visited);
       stack = ill_insert_front(stack, nodes[i], 0);
    }
    for (int i = 0; i < n_nodes; ++i) {
        meek_rules(cg, nodes[i], &stack ,&compelled, visited, n_visited);
    }
    while (stack) {
        /* pop the top */
        int node = stack->key;
        struct ill *p = stack;
        stack = stack->next;
        free(p);
        undirect_reversible_parents(node, cg, &stack, &compelled, visited,
                                          n_visited);
        meek_rules(cg, node, &stack, &compelled, visited, n_visited);
    }
    /*
     * now that we've completed the pdag, we need to clean up the mess we made
     * when we set the edge values to negative */
    struct pll *p;
    while(compelled) {
        compelled->p->value = -compelled->p->value;
        p = compelled->next;
        free(compelled);
        compelled = p;
    }
    return visited;
}

/*
 * undirect_reversible_parents will undirected any parents of node that are not
 * compelled. It does two other things. It records unshielded colliders by
 * storing the pointer to the edge in compelled, and also makes the the edge
 * value negative if compelled
 */
static void undirect_reversible_parents(int node, struct cgraph *cg,
                                                  struct ill **stack,
                                                  struct pll **compelled,
                                                  int *visited,  int *n_visited)
{
    struct ill *reversible = NULL;
    struct ill *p1 = cg->parents[node];
    /* find all unshielded colliders on node */
    while (p1) {
        int x = p1->key;
        struct ill *p2 = cg->parents[node];
        while (p2) {
            int z = p2->key;
            /*
             * If there is an unshielded collider, and x is not already
             * compelled add the edge to the compelled edges list and make its
             * value negative.
             */
            if (x != z && !adjacent_in_cgraph(cg, x, z)) {
                if (x > 0) {
                    p1->value = -p1->value;
                    insert_pll(compelled, p1);
                }
                goto NEXT_PARENT;
            }
            p2 = p2->next;
        }
        reversible = ill_insert_front(reversible, x, 0);
        NEXT_PARENT: ;
        p1 = p1->next;
    }
    /*
     * if we modify the node by undirecting any parents, we need to add the
     * node and its adjacents to the stack
     */
    int node_modified = 0;
    struct ill *p;
    while (reversible) {
        int parent = reversible->key;
        p = ill_search(cg->parents[node], parent);
        /* if the value is positive, the edge isn't compelled */
        if (p->value > 0) {
            Rprintf("undirect %i --> %i\n", parent, node);
            node_modified = 1;
            unorient_directed_edge(cg, parent, node);

            /* if parent or node haven't been marked as visited, mark them */
            if (!visited[parent]) {
                visited[parent] = 1;
                *n_visited     += 1;
            }
            if (!visited[node]) {
                visited[node] = 1;
                *n_visited   += 1;
            }
        }
        p = reversible;
        reversible = reversible->next;
        free(p);
    }
    if(node_modified) {
        push_adjacents(node, cg, stack);
        *stack = ill_insert_front(*stack, node, 0);
    }
}

static void push_adjacents(int node, struct cgraph *cg, struct ill **stack)
{
        struct ill *p;
        p = cg->parents[node];
        while (p) {
            *stack = ill_insert_front(*stack, p->key, 0);
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            *stack = ill_insert_front(*stack, p->key, 0);
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            *stack = ill_insert_front(*stack, p->key, 0);
            p = p->next;
        }
}

void orient(struct cgraph *cg, int x, int y, struct ill **stack,
                               struct pll **compelled, int *visited,
                               int *n_visited)
{
    Rprintf("direct %i --> %i\n", x, y);
    orient_undirected_edge(cg, x, y);
    /* get the newly created edge. add it to complled, and mark it compelled */
    struct ill *p = ill_search(cg->parents[y], x);
    p->value = -(p->value);
    insert_pll(compelled, p);
    /* if x or y are unvisited, marked them as visited */
    if (!visited[x]) {
        visited[x] = 1;
        *n_visited     += 1;
    }
    if (!visited[y]) {
        visited[y] = 1;
        *n_visited   += 1;
    }
    /* we need to look at y (again) now that we added a new compelled edge */
    *stack = ill_insert_front(*stack, y, 0);
}

void apply_rule(struct cgraph *cg, int x, struct ill **stack,
                                   struct pll **compelled, int *visited,
                                   int *n_visited, meek_rule _meek_rule)
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
        if(_meek_rule(cg, x, y))
            orient(cg, x, y, stack, compelled, visited, n_visited);
        else if(_meek_rule(cg, y, x))
            orient(cg, y, x, stack, compelled, visited, n_visited);
        p = p->next;
    }
    ill_free(cpy);
}

int _meek1(struct cgraph *cg, int x, int y) {
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

/* look for  z, st,  z --> y --- x, x --> z */
int _meek2(struct cgraph *cg, int x, int y) {
    struct ill  *xc = cg->children[x];
    while (xc) {
        int z = xc->key;
        if (edge_directed_in_cgraph(cg, z, y))
            return 1;
        xc = xc->next;
    }
    return 0;
}

int _meek3(struct cgraph *cg, int x, int y) {
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



void meek_rules(struct cgraph *cg, int x, struct ill **stack,
                                     struct pll **compelled, int *visited,
                                     int *n_visited)
{
    apply_rule(cg, x, stack, compelled, visited, n_visited, _meek1);
    apply_rule(cg, x, stack, compelled, visited, n_visited, _meek2);
    apply_rule(cg, x, stack, compelled, visited, n_visited, _meek3);
    apply_rule(cg, x, stack, compelled, visited, n_visited, _meek4);
}
