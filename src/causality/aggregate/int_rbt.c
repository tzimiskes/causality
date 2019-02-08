// This RBT implementation is adapted from the Eternally Confuzzled tutorial
// http://www.eternallyconfuzzled.com/tuts/datastructures/jsw_tut_rbtree.aspx

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>

#include <causality.h>
#include <aggregate/int_redblacktree.h>

#define LEFT 0
#define RIGHT 1

static inline int IS_RED(struct irbt *node)
{
    if (node == NULL)
        return 0;
    else if (node->key > 0)
        return 1;
    else
        return 0;
}

static inline int ABS(int x) {
    if (x < 0)
        return -x;
    else
        return x;
}

static inline void SET_TO_RED(struct irbt *node)
{
    node->key = ABS(node->key);
}

static inline void SET_TO_BLACK(struct irbt *node)
{
    node->key = -ABS(node->key);
}

int irbt_key(struct irbt *node)
{
    return ABS(node->key) - 1;
}

static struct irbt * irbt_instantiate_node(int key, float *values, float weight)
{
    struct irbt *tmp = malloc(sizeof(struct irbt));
    if (tmp == NULL)
        CAUSALITY_ERROR("failed to allocate memory for rbt pointer\n");
    for (int i = 0; i < NUM_EDGES_STORED; ++i)
        tmp->values[i] = values[i] * weight;
    tmp->key          = key + 1;
    tmp->child[LEFT]  = NULL;
    tmp->child[RIGHT] = NULL;
    return tmp;
}

static inline struct irbt * single_rotation(struct irbt  *root, int dir)
{
  struct irbt *tmp  = root->child[!dir];
  root->child[!dir] = tmp->child[dir];
  tmp->child[dir]   = root;
  SET_TO_RED(root);
  SET_TO_BLACK(tmp);
  return tmp;
}

static inline struct irbt * double_rotation(struct irbt *root, int dir)
{
    root->child[!dir] = single_rotation(root->child[!dir], !dir);
    return single_rotation(root, dir);
}

static struct irbt * irbt_insert_recursive(struct irbt *root, int key,
                                                              float *values,
                                                              float weight)
{
    if (root == NULL)
        root = irbt_instantiate_node(key, values, weight);
    else if (key == irbt_key(root)) {
        for (int i = 0; i < NUM_EDGES_STORED; ++i)
            root->values[i] += values[i];
    }
    else {
        int direction = key < irbt_key(root) ? LEFT : RIGHT;
        root->child[direction] = irbt_insert_recursive(root->child[direction],
                                                  key, values, weight);
        if (IS_RED(root->child[direction])) {
            if (IS_RED(root->child[!direction])) {
                SET_TO_RED(root);
                SET_TO_BLACK(root->child[LEFT]);
                SET_TO_BLACK(root->child[RIGHT]);
            }
            else {
                if (IS_RED(root->child[direction]->child[direction]))
                    root = single_rotation(root, !direction);
                else if(IS_RED(root->child[direction]->child[!direction]))
                    root = double_rotation(root, !direction);
            }
        }
    }
    return root;
}

struct irbt * irbt_insert(struct irbt *root, int key, float *values,
                                             float weight)
{
    root = irbt_insert_recursive(root, key, values, weight);
    SET_TO_BLACK(root);
    return root;
}

void irbt_print_tree(struct irbt * root)
{
    if(root != NULL) {
        CAUSALITY_PRINT("Key: %i Value(s):", irbt_key(root));
        for(int i = 0; i < NUM_EDGES_STORED; ++i)
            CAUSALITY_PRINT("%f" , root->values[i]);
        CAUSALITY_PRINT("\n");
        irbt_print_tree(root->child[LEFT]);
        irbt_print_tree(root->child[RIGHT]);
    }
}

void irbt_free(struct irbt * root)
{
    if (root != NULL) {
        irbt_free(root->child[LEFT]);
        irbt_free(root->child[RIGHT]);
        free(root);
  }
}

struct irbt * irbt_merge_trees(struct irbt * dst, struct irbt * src)
{
    if (src != NULL) {
        dst = irbt_merge_trees(dst, src->child[LEFT]);
        dst = irbt_merge_trees(dst, src->child[RIGHT]);
        return(irbt_insert(dst, irbt_key(src), src->values, 1));
    }
    else
        return dst;
}

int irbt_size(struct irbt *root)
{
    if (root != NULL)
        return 1 + irbt_size(root->child[LEFT]) + irbt_size(root->child[RIGHT]);
    else
        return 0;
}

float * irbt_values_ptr(struct irbt *root)
{
    return root->values;
}

struct irbt * irbt_left_child(struct irbt *root)
{
    return root->child[LEFT];
}

struct irbt * irbt_right_child(struct irbt *root)
{
    return root->child[RIGHT];
}
