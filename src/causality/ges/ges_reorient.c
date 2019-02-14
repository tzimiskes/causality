#include <stdlib.h>
#include <string.h>

#include <causality.h>

#include <cgraph/cgraph.h>
#include <cgraph/int_linked_list.h>
#include <algorithms/meek.h>
#include <ges/ges.h>
#include <ges/ges_internal.h>

#define TAG_COMPELLED 1
#define UNTAGGED      0

/* pointer linked list */
struct pll {
    struct ill *p;
    struct pll *next;
};

static void undirect_reversible_parents(int node, struct cgraph *cg,
                                                  struct ill **stack,
                                                  struct pll **compelled,
                                                  int *visited);

static void push_adjacents(int node, struct cgraph *cg, struct ill **stack);

void apply_rule_local(struct cgraph *cg, int x, struct ill **stack,
                                         struct pll **compelled, int *visited,
                                         meek_rule _meek_rule);


static void meek_rules(struct cgraph *cg, int x, struct ill **stack,
                                          struct pll **compelled, int *visited);

void orient(struct cgraph *cg, int x, int y, struct ill **stack,
                               struct pll **compelled, int *visited);

static void insert_pll(struct pll **p, struct ill *l)
{
    struct pll *t = malloc(sizeof(struct pll));
    t->p    = l;
    t->next = NULL;
    if (*p)
        t->next = *p;
    *p = t;
}

void reorient_fes(struct cgraph *cg, struct ges_operator op, int *visited)
{
    memset(visited, 0, cg->n_nodes * sizeof(int));
    struct pll *compelled = NULL;
    struct ill *stack     = NULL;
    undirect_reversible_parents(op.y, cg, &stack, &compelled, visited);
    stack = ill_insert_front(stack, op.y, 0);
    meek_rules(cg, op.y, &stack, &compelled, visited);
    while (stack) {
        /* pop the top */
        int         node = stack->node;
        struct ill *p    = stack;
        stack = stack->next;
        free(p);
        undirect_reversible_parents(node, cg, &stack, &compelled, visited);
        meek_rules(cg, node, &stack, &compelled, visited);
    }
    /*
     * now that we've completed the pdag, we need to clean up the mess we made
     * when we set the edge values to negative */
    struct pll *p;
    while (compelled) {
        compelled->p->tag = UNTAGGED;
        p = compelled->next;
        free(compelled);
        compelled = p;
    }
}


void reorient_bes(struct cgraph *cg, struct ges_operator op, int *visited)
{
    memset(visited, 0, cg->n_nodes * sizeof(int));
    struct pll *compelled = NULL;
    struct ill *stack     = NULL;
    undirect_reversible_parents(op.y, cg, &stack, &compelled, visited);
    stack = ill_insert_front(stack, op.y, 0);
    for (int i = 0; i < op.naxy_size; ++i) {
        if (IS_HEAD_NODE(op.h, i)) {
            undirect_reversible_parents(op.naxy[i], cg, &stack, &compelled,
                                                        visited);
            stack = ill_insert_front(stack, op.naxy[i], 0);
        }
    }
    meek_rules(cg, op.y, &stack, &compelled, visited);
    for (int i = 0; i < op.naxy_size; ++i) {
        if (IS_HEAD_NODE(op.h, i))
            meek_rules(cg, op.naxy[i], &stack, &compelled, visited);
    }
    while (stack) {
        /* pop the top */
        int node = stack->node;
        struct ill *p = stack;
        stack = stack->next;
        free(p);
        undirect_reversible_parents(node, cg, &stack, &compelled, visited);
        meek_rules(cg, node, &stack, &compelled, visited);
    }
    /*
     * now that we've completed the pdag, we need to clean up the mess we made
     * when we set the edge values to negative
     */
    struct pll *p;
    while (compelled) {
        compelled->p->tag = UNTAGGED;
        p = compelled->next;
        free(compelled);
        compelled = p;
    }
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
                                                  int *visited)
{
    struct ill *reversible = NULL;
    struct ill *p1 = cg->parents[node];
    /* find all unshielded colliders on node */
    while (p1) {
        int x = p1->node;
        struct ill *p2 = cg->parents[node];
        while (p2) {
            int z = p2->node;
            /*
             * If there is an unshielded collider, and x is not already
             * compelled add the edge to the compelled edges list and make its
             * value negative.
             */
            if (x != z && !adjacent_in_cgraph(cg, x, z)) {
                if (x > 0) {
                    p1->tag = TAG_COMPELLED;
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
    while (reversible) {
        int parent = reversible->node;
        struct ill *p = ill_search(cg->parents[node], parent);
        /* if the value is positive, the edge isn't compelled */
        if (p->edge > 0) {
            node_modified = 1;
            unorient_directed_edge(cg, parent, node);
            /* if parent or node haven't been marked as visited, mark them */
            visited[parent] = 1;
            visited[node] = 1;
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
            *stack = ill_insert_front(*stack, p->node, 0);
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            *stack = ill_insert_front(*stack, p->node, 0);
            p = p->next;
        }
        p = cg->spouses[node];
        while (p) {
            *stack = ill_insert_front(*stack, p->node, 0);
            p = p->next;
        }
}

void orient(struct cgraph *cg, int x, int y, struct ill **stack,
                                     struct pll **compelled, int *visited)
{
    orient_undirected_edge(cg, x, y);
    /* get the newly created edge. add it to complled, and mark it compelled */
    struct ill *p = ill_search(cg->parents[y], x);
    p->edge = TAG_COMPELLED;
    insert_pll(compelled, p);
    /* if x or y are unvisited, marked them as visited */
    visited[x] = 1;
    visited[y] = 1;
    /* we need to look at y (again) now that we added a new compelled edge */
    *stack = ill_insert_front(*stack, y, 0);
}

void apply_rule_local(struct cgraph *cg, int x, struct ill **stack,
                                         struct pll **compelled, int *visited,
                                         meek_rule _meek_rule)
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
        int y = p->node;
        if(_meek_rule(cg, x, y))
            orient(cg, x, y, stack, compelled, visited);
        else if(_meek_rule(cg, y, x))
            orient(cg, y, x, stack, compelled, visited);
        p = p->next;
    }
    ill_free(cpy);
}

void meek_rules(struct cgraph *cg, int x, struct ill **stack,
                                     struct pll **compelled, int *visited)
{
    apply_rule_local(cg, x, stack, compelled, visited, meek_rule1);
    apply_rule_local(cg, x, stack, compelled, visited, meek_rule2);
    apply_rule_local(cg, x, stack, compelled, visited, meek_rule3);
    apply_rule_local(cg, x, stack, compelled, visited, meek_rule4);
}
