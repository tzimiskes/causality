#include <stdlib.h>
#include <string.h>

#include <causality.h>

#include <cgraph/cgraph.h>
#include <cgraph/edge_list.h>
#include <algorithms/meek.h>
#include <ges/ges.h>
#include <ges/ges_internal.h>

#define TAG_COMPELLED  1
#define TAG_REVERSABLE UNTAGGED

/* compelled edge list */
struct cel {
    struct edge_list *p;
    struct cel *next;
};

static void undirect_reversible_parents(int node, struct cgraph *cg,
                                            struct stack **s, struct cel
                                            **compelled, int *visited);

static void push_adjacents(int node, struct cgraph *cg, struct stack **s);

static void apply_rule_locally(struct cgraph *cg, int x, struct stack **s,
                                   struct cel **compelled, int *visited,
                                   meek_rule meek_rule);

static void meek_rules(struct cgraph *cg, int x, struct stack **s, struct cel
                           **compelled, int *visited);

static void orient(struct cgraph *cg, int x, int y, struct stack **s,
                       struct cel **compelled, int *visited);

static void make_compelled(struct cel **compelled, struct edge_list *l)
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
    struct stack *s       = NULL;
    undirect_reversible_parents(op.y, cg, &s, &compelled, visited);
    push(&s, op.y);
    meek_rules(cg, op.y, &s, &compelled, visited);
    int node;
    while ((node = pop(&s)) >= 0) {
        undirect_reversible_parents(node, cg, &s, &compelled, visited);
        meek_rules(cg, node, &s, &compelled, visited);
    }
    free_compelled(compelled);
}

void reorient_bes(struct cgraph *cg, struct ges_operator op, int *visited)
{
    memset(visited, 0, cg->n_nodes * sizeof(int));
    struct cel *compelled = NULL;
    struct stack *s       = NULL;
    undirect_reversible_parents(op.y, cg, &s, &compelled, visited);
    push(&s, op.y);
    for (int i = 0; i < op.nayx_size; ++i) {
        if (IS_HEAD_NODE(op.h, i)) {
            undirect_reversible_parents(op.nayx[i], cg, &s, &compelled,
                                                        visited);
            push(&s, op.nayx[i]);
        }
    }
    meek_rules(cg, op.y, &s, &compelled, visited);
    for (int i = 0; i < op.nayx_size; ++i) {
        if (IS_HEAD_NODE(op.h, i))
            meek_rules(cg, op.nayx[i], &s, &compelled, visited);
    }
    int node;
    while ((node = pop(&s)) >= 0) {
        undirect_reversible_parents(node, cg, &s, &compelled, visited);
        meek_rules(cg, node, &s, &compelled, visited);
    }
    free_compelled(compelled);
}

/*
 * undirect_reversible_parents will undirected any parents of node that are not
 * compelled. It does two other things. It records unshielded colliders by
 * storing the pointer to the edge in compelled, and also makes the the edge
 * value negative if compelled
 */
static void undirect_reversible_parents(int y, struct cgraph *cg, struct stack
                                            **s, struct cel **compelled,
                                            int *visited)
{
    struct edge_list *p1 = cg->parents[y];
    /* find all unshielded colliders on y */
    while (p1) {
        if (p1->tag == TAG_COMPELLED)
            goto NEXT_PARENT;
        int x = p1->node;
        struct edge_list *p2 = cg->parents[y];
        while (p2) {
            /* If there is an unshielded collider make the edge compelled. */
            if (x != p2->node && !adjacent_in_cgraph(cg, x, p2->node)) {
                make_compelled(compelled, p1);
                if (p2->tag == UNTAGGED)
                    make_compelled(compelled, p2);
                goto NEXT_PARENT;
            }
            p2 = p2->next;
        }
        NEXT_PARENT:
        p1 = p1->next;
    }
    struct edge_list *cpy = copy_edge_list(cg->parents[y]);
    struct edge_list *p   = cpy;
    int node_modified = 0;
    while (p) {
        int x = p->node;
        if (p->tag == TAG_REVERSABLE) {
            node_modified = 1;
            unorient_directed_edge(cg, x, y);
            visited[x] = 1;
            visited[y] = 1;
        }
        p = p->next;
    }
    free_edge_list(cpy);
    if (node_modified) {
        push(s, y);
        push_adjacents(y, cg, s);
    }
}

static void push_list(struct stack **s, struct edge_list *p)
{
    while (p) {
        push(s, p->node);
        p = p->next;
    }
}

static void push_adjacents(int node, struct cgraph *cg, struct stack **s)
{
    push_list(s, cg->parents[node]);
    push_list(s, cg->spouses[node]);
    push_list(s, cg->children[node]);
}

/*
 * orient takes the undirected edge x --- y and orients it x --> y. The edge is
 * tagged compelled and added to the compelled list. We have made a change to x
 * and y so we mark them down as visited. y is added to the stack to see what
 * effect the newly oriented edge has.
 */
static void orient(struct cgraph *cg, int x, int y, struct stack **s,
                       struct cel **compelled, int *visited)
{
    orient_undirected_edge(cg, x, y);
    /* get the newly created edge and make it compelled */
    make_compelled(compelled, search_edge_list(cg->parents[y], x));
    visited[x] = 1;
    visited[y] = 1;
    push(s, x);
    push(s, y);
}

static void apply_rule_locally(struct cgraph *cg, int x, struct stack **s,
                                   struct cel **compelled, int *visited,
                                   meek_rule meek_rule)
{
    /*
     * we need to create a copy of spouses in case an edge is
     * oriented -- orientation occurs "in place."
     */
    struct edge_list *cpy = copy_edge_list(cg->spouses[x]);
    struct edge_list *p   = cpy;
    while (p) {
        int y = p->node;
        if (meek_rule(cg, x, y))
            orient(cg, x, y, s, compelled, visited);
        else if (meek_rule(cg, y, x))
            orient(cg, y, x, s, compelled, visited);
        p = p->next;
    }
    free_edge_list(cpy);
}

static void meek_rules(struct cgraph *cg, int x, struct stack **s,
                           struct cel **compelled, int *visited)
{
    apply_rule_locally(cg, x, s, compelled, visited, meek_rule1);
    apply_rule_locally(cg, x, s, compelled, visited, meek_rule2);
    apply_rule_locally(cg, x, s, compelled, visited, meek_rule3);
    apply_rule_locally(cg, x, s, compelled, visited, meek_rule4);
}
