#include <stdlib.h>

#include "headers/causality.h"
#include "headers/int_linked_list.h"
#include "headers/cgraph.h"
#include "headers/meek.h"

#define DEBUG 0

#ifndef DEBUG
#define DEBUG 0
#endif

struct pll {
    struct ill *p;
    struct pll *next;
};

static void undirect_reversible_parents(int node, struct cgraph *cg,
                                                  struct ill **stack,
                                                  struct pll **compelled,
                                                  int *visited, int *n_visited);

static void push_adjacents(int node, struct cgraph *cg, struct ill **stack);

void apply_rule_local(struct cgraph *cg, int x, struct ill **stack,
                                   struct pll **compelled, int *visited,
                                   int *n_visited, meek_rule _meek_rule);


static void meek_rules(struct cgraph *cg, int x, struct ill **stack,
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

int * reorient(struct cgraph *cg, struct ges_op op, int *n_visited)
{
    struct pll *compelled = NULL;
    struct ill *stack     = NULL;
    int        *visited   = calloc(cg->n_nodes, sizeof(int));
    undirect_reversible_parents(op.y, cg, &stack, &compelled, visited, n_visited);
    stack = ill_insert_front(stack, op.y, 0);
    if (op.type == DELETION) {
        for (int i = 0; i < op.naxy_size; ++i) {
            if ((op.h & 1 << i) == 1 << i) {
                undirect_reversible_parents(op.naxy[i], cg, &stack, &compelled,
                                                        visited, n_visited);
                stack = ill_insert_front(stack, op.naxy[i], 0);
            }
        }
        meek_rules(cg, op.y, &stack, &compelled, visited, n_visited);
        for (int i = 0; i < op.naxy_size; ++i) {
            if ((op.h & 1 << i) == 1 << i)
                meek_rules(cg, op.naxy[i], &stack, &compelled, visited, n_visited);
        }
    }
    else if (op.type == INSERTION)
        meek_rules(cg, op.y, &stack, &compelled, visited, n_visited);
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

void apply_rule_local(struct cgraph *cg, int x, struct ill **stack,
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

void meek_rules(struct cgraph *cg, int x, struct ill **stack,
                                     struct pll **compelled, int *visited,
                                     int *n_visited)
{
    apply_rule_local(cg, x, stack, compelled, visited, n_visited, meek1);
    apply_rule_local(cg, x, stack, compelled, visited, n_visited, meek2);
    apply_rule_local(cg, x, stack, compelled, visited, n_visited, meek3);
    apply_rule_local(cg, x, stack, compelled, visited, n_visited, meek4);
}
