#include <time.h>
#include <R.h>
#ifndef _BENCHMARKR_H
#define _BENCHMARKR_H
#ifndef BENCHMARK
#define CREATE_TIMER(NAME)
#define TIME_FUNC(TIMER, FUNC) FUNC;
#define PRINT_TIMER(TIMER)
#else
static clock_t c1 = 0;
static clock_t c2 = 0;
#define CREATE_TIMER(NAME) static double NAME = 0.0f;
#define TIME_FUNC(TIMER, FUNC) c1 = clock(); \
                               FUNC; \
                               c2 = clock(); \
                               TIMER += ((double) (c2-c1))/(CLOCKS_PER_SEC *1.0e3);
#define PRINT_TIMER(TIMER) Rprintf(#TIMER); Rprintf(": %f (ms)\n", TIMER);
#endif
#endif /* benchmarkr.h */
