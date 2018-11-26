#include <cgraph.h>
#ifndef _MEEK_H
#define _MEEK_H
typedef int (*meek_rule)(struct cgraph *cg, int x, int y);

int meek1(struct cgraph *cg, int x, int y);
int meek2(struct cgraph *cg, int x, int y);
int meek3(struct cgraph *cg, int x, int y);
int meek4(struct cgraph *cg, int x, int y);
#endif
