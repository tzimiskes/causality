#ifndef MEEK_H
#define MEEK_H

#include <stdlib.h>
#include <cgraph/cgraph.h>

struct stack {
    int size;
    int node;
    struct stack *next;
};

typedef int (*meek_rule)(struct cgraph *cg, int x, int y);
/* node stack operations */
void push(struct stack **s, int node);
int pop(struct stack **s);
/* meek rules */
void apply_rule(struct cgraph *cg, int x, struct stack **s, meek_rule rule);
int meek_rule1(struct cgraph *cg, int x, int y);
int meek_rule2(struct cgraph *cg, int x, int y);
int meek_rule3(struct cgraph *cg, int x, int y);
int meek_rule4(struct cgraph *cg, int x, int y);
#endif
