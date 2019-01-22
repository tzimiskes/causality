#include "causality.h"

#ifndef IRBT_H
#define IRBT_H

struct irbt {
    struct irbt * child[2];
    int           key;
    float         values [NUM_EDGES_STORED];
};


struct irbt * irbt_insert(struct irbt *root, int key, float *values, float weight);
void irbt_print_tree(struct irbt *root);
void irbt_free(struct irbt *root);
struct irbt * irbt_merge_trees(struct irbt *dst, struct irbt *src);
int irbt_size(struct irbt *root);
int irbt_key(struct irbt *root);
float * irbt_values_ptr(struct irbt * root);
struct irbt * irbt_left_child(struct irbt *root);
struct irbt * irbt_right_child(struct irbt *root);
#endif
