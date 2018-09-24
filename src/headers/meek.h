#include <cgraph.h>
#ifndef _MEEK_H
#define _MEEK_H
typedef int (*meek_rule)(struct cgraph *cg, int x, int y);

int _meek1(struct cgraph *cg, int x, int y);
int _meek2(struct cgraph *cg, int x, int y);
int _meek3(struct cgraph *cg, int x, int y);
int _meek4(struct cgraph *cg, int x, int y);
#endif
