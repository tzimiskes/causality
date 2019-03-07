#include <stdint.h>
#include <stdio.h>

static inline int IS_TAIL_NODE(uint64_t t, int node)
{
    uint64_t i = 0x1 << node;
    return (t & i) == i;
}

int main ()
{
    uint64_t y = 0x1;
    uint64_t n = 0x1234;
    for (int i = 63; i >= 0; --i)
        printf("%i", (n & (y << i)) == (y << i));
    printf("\n");
    for (int i = 63; i >= 0; --i)
        printf("%i", IS_TAIL_NODE(n, i));
    printf("\n");
    return 0;
}
