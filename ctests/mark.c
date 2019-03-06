#include <stdio.h>

static inline int is_marked(int i, unsigned char *marked)
{
    unsigned char j = 0x01 << (i % 8);
    return (marked[i / 8] & j) == j;
}

static inline void mark(int i, unsigned char *marked)
{
    int q = i / 8;
    unsigned char j = 0x01 << (i % 8);
    marked[q] = marked[q] | j;
}

int main () {
    unsigned char x[2] = {0, 0};
    mark(0, x);
    mark(2, x);
    mark(4, x);
    mark(15, x);
    mark(8, x);
    for (int i = 0; i < 16; ++i)
        printf("%i", is_marked(i, x));
    printf("\n");
    return 0;
}
