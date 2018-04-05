// This RBT implementation is adapted from the Eternally Confuzzled tutorial
// http://www.eternallyconfuzzled.com/tuts/datastructures/jsw_tut_rbtree.aspx
/*
#ifdef _MSC_VER
#define ALIGNED_ALLOCATE(ptr, align, size) ptr = _aligned_malloc(size, align)
#define FREE(ptr) _aligned_free(ptr)
#else
#define _XOPEN_SOURCE 600
#define ALIGNED_ALLOCATE(ptr, align, size) posix_memalign((void**) &(ptr), \
                                                        (align), (size))
#define FREE(ptr) free(ptr)
#endif
*/
#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include"headers/causality.h"

#define LEFT 0
#define RIGHT 1

typedef struct irbt* irbt_ptr;
typedef struct irbt {
  irbt_ptr child[2];
  int key;
  int values [11]; /* NUM_EDGES_STORED */
}irbt;

static inline int IS_RED(irbt_ptr node) {
  if(node == NULL) {
    return 0;
  }
  else if(node->key > 0)
    return 1;
  else
    return 0;
}

static inline int ABS(int x) {
  if(x < 0)
    return(-x);
  else
    return(x);
}

static inline void SET_TO_RED(irbt_ptr node) {
  node->key = ABS(node->key);
}

static inline void SET_TO_BLACK(irbt_ptr node) {
  node->key = -ABS(node->key);
}

int irbt_key(irbt_ptr root) {
  return(ABS(root->key) - 1);
}

inline irbt_ptr irbt_instantiate_node(const int key, const int n,
                                                   const int * const values)
  {
  irbt_ptr tmp = malloc(sizeof(irbt));
  if(tmp == NULL) {
    error("failed to allocate memory for rbt pointer\n");
  }
  memcpy(tmp->values, values, n*sizeof(int));
  tmp->key          = key + 1;
  tmp->child[LEFT]  = NULL;
  tmp->child[RIGHT] = NULL;
  return(tmp);
}

static inline irbt_ptr single_rotation(irbt_ptr root, int dir) {

  irbt_ptr tmp = root->child[!dir];

  root->child[!dir] = tmp->child[dir];
  tmp->child[dir]   = root;

  SET_TO_RED(root);
  SET_TO_BLACK(tmp);
  return tmp;
}

static inline irbt_ptr double_rotation(irbt_ptr root, int dir) {

  root->child[!dir] = single_rotation(root->child[!dir], !dir);

  return single_rotation(root, dir);
}

static irbt_ptr irbt_insert_recurvise(irbt_ptr root,
                                                   const int key,
                                                   const int n,
                                                   const int* const values)
{
  if(root == NULL) {
    root = irbt_instantiate_node(key, n, values);
  }
  else if (key == irbt_key(root)) {
    for(int i = 0; i < n; ++i)
      root->values[i] += values[i];
  }
  else {
    int direction = key < irbt_key(root) ? LEFT : RIGHT;
    root->child[direction] = irbt_insert_recurvise(root->child[direction],
                                                      key, n, values);

    if(IS_RED(root->child[direction])) {
      if(IS_RED(root->child[!direction])) {

        SET_TO_RED(root);
        SET_TO_BLACK(root->child[LEFT]);
        SET_TO_BLACK(root->child[RIGHT]);
      }
      else {
        if(IS_RED(root->child[direction]->child[direction]))
          root = single_rotation(root, !direction);
        else if(IS_RED(root->child[direction]->child[!direction]))
          root = double_rotation(root, !direction);
      }
    }
  }
  return root;
}

irbt_ptr irbt_insert(irbt_ptr root, const int key, const int n,
                           const int * const values)
{
  root = irbt_insert_recurvise(root, key , n, values);
  SET_TO_BLACK(root);
  return(root);
}

void irbt_print_tree(irbt_ptr root, const int n) {
  if(root != NULL) {
    Rprintf("Key: %i Value(s):", irbt_key(root));
    for(int i = 0; i < n; ++i)
      Rprintf(" %i" , root->values[i]);
    Rprintf("\n");
    irbt_print_tree(root->child[LEFT], n);
    irbt_print_tree(root->child[RIGHT], n);
  }
}

void irbt_free(irbt_ptr root) {
  if(root != NULL) {
    irbt_free(root->child[LEFT]);
    irbt_free(root->child[RIGHT]);
    free(root);
  }
}

irbt_ptr* make_ptr_to_irbt(const int n) {
  irbt_ptr* hash_table = malloc(n*sizeof(irbt));
  if(hash_table == NULL)
    error("Failed to allocate pointer for hash_table.\n");
  for(int i = 0; i < n; ++i)
    hash_table[i] = NULL;
  return(hash_table);
}


irbt_ptr irbt_merge_trees(irbt_ptr dst, irbt_ptr src, const int n)
{
  if(src != NULL) {
    dst = irbt_merge_trees(dst, src->child[LEFT], n);
    dst = irbt_merge_trees(dst, src->child[RIGHT], n);
    return(irbt_insert(dst, irbt_key(src), n , src->values));
  }
  else
    return dst;
}

int irbt_size(irbt_ptr root) {
  if(root != NULL)
    return(1 + irbt_size(root->child[LEFT]) +
            irbt_size(root->child[RIGHT]));
  else
    return 0;
}

int* irbt_values_ptr(irbt_ptr root) {
  return (root->values);
}

irbt_ptr irbt_left_child(irbt_ptr root) {
  return (root->child[LEFT]);
}

irbt_ptr irbt_right_child(irbt_ptr root) {
  return (root->child[RIGHT]);
}
