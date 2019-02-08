#include <cgraph/cgraph.h>

#ifndef MEEK_H
#define MEEK_H

typedef int (*meek_rule)(struct cgraph *cg, int x, int y);

int meek_rule1(struct cgraph *cg, int x, int y);
int meek_rule2(struct cgraph *cg, int x, int y);
int meek_rule3(struct cgraph *cg, int x, int y);
int meek_rule4(struct cgraph *cg, int x, int y);
#endif
