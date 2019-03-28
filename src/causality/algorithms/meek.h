#ifndef MEEK_H
#define MEEK_H

#include <stdlib.h>
#include <cgraph/cgraph.h>

struct stack {
    int node;
    struct stack *next;
};

inline void push(struct stack **s, int node)
{
    struct stack *tmp = malloc(sizeof (struct stack));
    tmp->next = *s;
    tmp->node = node;
    *s = tmp;
}

inline int pop(struct stack **s)
{
    if (*s == NULL)
        return -1;
    struct stack *tmp = *s;
    int node = tmp->node;
    *s = tmp->next;
    free(tmp);
    return node;
}


typedef int (*meek_rule)(struct cgraph *cg, int x, int y);

int meek_rule1(struct cgraph *cg, int x, int y);
int meek_rule2(struct cgraph *cg, int x, int y);
int meek_rule3(struct cgraph *cg, int x, int y);
int meek_rule4(struct cgraph *cg, int x, int y);
#endif
