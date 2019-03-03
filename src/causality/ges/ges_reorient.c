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

/* compelled edge list */
struct cel {
    struct ill *p;
    struct cel *next;
};

static void undirect_reversible_parents(int node, struct cgraph *cg,
                                            struct ill **stack, struct cel
                                            **compelled, int *visited);

static void push_adjacents(int node, struct cgraph *cg, struct ill **stack);

static void apply_rule_locally(struct cgraph *cg, int x, struct ill **stack,
                                   struct cel **compelled, int *visited,
                                   meek_rule meek_rule);

static void meek_rules(struct cgraph *cg, int x, struct ill **stack, struct cel
                           **compelled, int *visited);

static void orient(struct cgraph *cg, int x, int y, struct ill **stack,
                       struct cel **compelled, int *visited);

static void make_compelled(struct cel **compelled, struct ill *l)
{
    l->tag = TAG_COMPELLED;
    struct cel *t = malloc(sizeof(struct cel));
    t->p    = l;
    t->next = NULL;
    if (*compelled)
        t->next = *compelled;
    *compelled = t;
}

static void free_compelled(struct cel *compelled)
{
    struct cel *next;
    while (compelled) {
        compelled->p->tag = UNTAGGED;
        next = compelled->next;
        free(compelled);
        compelled = next;
    }
}

void reorient_fes(struct cgraph *cg, struct ges_operator op, int *visited)
{
    memset(visited, 0, cg->n_nodes * sizeof(int));
    struct cel *compelled = NULL;
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
    free_compelled(compelled);
}


void reorient_bes(struct cgraph *cg, struct ges_operator op, int *visited)
{
    memset(visited, 0, cg->n_nodes * sizeof(int));
    struct cel *compelled = NULL;
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
    free_compelled(compelled);
}

/*
 * undirect_reversible_parents will undirected any parents of node that are not
 * compelled. It does two other things. It records unshielded colliders by
 * storing the pointer to the edge in compelled, and also makes the the edge
 * value negative if compelled
 */
static void undirect_reversible_parents(int node, struct cgraph *cg,
                                                  struct ill **stack,
                                                  struct cel **compelled,
                                                  int *visited)
{
    struct ill *reversible = NULL;
    struct ill *p1 = cg->parents[node];
    /* find all unshielded colliders on node */
    while (p1) {
        if (p1->tag == TAG_COMPELLED)
            goto NEXT_PARENT;
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
                make_compelled(compelled, p1);
                if (p2->tag == UNTAGGED)
                    make_compelled(compelled, p2);
                goto NEXT_PARENT;
            }
            p2 = p2->next;
        }
        reversible = ill_insert_front(reversible, x, 0);
        NEXT_PARENT:
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
        if (p->tag == UNTAGGED) {
            node_modified = 1;
            unorient_directed_edge(cg, parent, node);
            visited[parent] = 1;
            visited[node]   = 1;
        }
        p = reversible;
        reversible = reversible->next;
        free(p);
    }
    if (node_modified) {
        push_adjacents(node, cg, stack);
        *stack = ill_insert_front(*stack, node, 0);
    }
}

static inline void push_list(struct ill **stack, struct ill *p)
{
    while (p) {
        *stack = ill_insert_front(*stack, p->node, 0);
        p = p->next;
    }
}

static void push_adjacents(int node, struct cgraph *cg, struct ill **stack)
{
    push_list(stack, cg->parents[node]);
    push_list(stack, cg->spouses[node]);
    push_list(stack, cg->children[node]);
}

/*
 * orient takes the undirected edge x --- y and orients it x --> y. The edge is
 * tagged compelled and added to the compelled list. We have made a change to x
 * and y so we mark them down as visited. y is added to the stack to see what
 * effect the newly oriented edge has.
 */
void orient(struct cgraph *cg, int x, int y, struct ill **stack,
                                     struct cel **compelled, int *visited)
{
    orient_undirected_edge(cg, x, y);
    /* get the newly created edge and make it compelled */
    make_compelled(compelled, ill_search(cg->parents[y], x));
    visited[x] = 1;
    visited[y] = 1;
    *stack = ill_insert_front(*stack, y, 0);
    *stack = ill_insert_front(*stack, x, 0);
}

static void apply_rule_locally(struct cgraph *cg, int x, struct ill **stack,
                                   struct cel **compelled, int *visited,
                                   meek_rule meek_rule)
{
    /*
     * we need to create a copy of spouses in case an edge is
     * oriented -- orientation occurs "in place."
     */
    struct ill *cpy = copy_ill(cg->spouses[x]);
    struct ill *p   = cpy;
    while (p) {
        int y = p->node;
        if (meek_rule(cg, x, y))
            orient(cg, x, y, stack, compelled, visited);
        else if (meek_rule(cg, y, x))
            orient(cg, y, x, stack, compelled, visited);
        p = p->next;
    }
    ill_free(cpy);
}

static void meek_rules(struct cgraph *cg, int x, struct ill **stack,
                           struct cel **compelled, int *visited)
{
    apply_rule_locally(cg, x, stack, compelled, visited, meek_rule1);
    apply_rule_locally(cg, x, stack, compelled, visited, meek_rule2);
    apply_rule_locally(cg, x, stack, compelled, visited, meek_rule3);
    apply_rule_locally(cg, x, stack, compelled, visited, meek_rule4);
}
