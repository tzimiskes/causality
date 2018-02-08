#include"headers/rapi.h"

#include<stdlib.h>
#include<string.h>
#include <stdio.h>

#define IS_RED(node) ((node) == NULL ? 0 : ((node)->key > 0 ? 1: 0))
#define ABS(x) ((x) > 0 ? (x): -(x))

#define SET_TO_RED(node) ((node)->key = ABS((node)->key))
#define SET_TO_BLACK(node) ((node)->key = -ABS((node)->key))

#define LEFT 0
#define RIGHT 1

typedef struct int_rbt_node* int_rbt_node_ptr;
typedef struct int_rbt_node {
  int key;
  int_rbt_node_ptr child[2];
  int values [];
}int_rbt_node;

int int_rbt_node_ptr_key(int_rbt_node_ptr node) {
  return(ABS(node->key) - 1);
}

inline int_rbt_node_ptr int_rbt_instantiate_node(const int key, const int n,
                                          const int * const values) {
  int_rbt_node_ptr tmp = malloc(sizeof(int_rbt_node) + n*sizeof(int));
  if(tmp == NULL) {
    exit(1);
  }
  printf("1\n");
  memcpy(tmp->values, values, n*sizeof(int));
  printf("2\n");
  tmp->key          = key + 1;
  tmp->child[LEFT]  = NULL;
  tmp->child[RIGHT] = NULL;
  return(tmp);
}

inline int_rbt_node_ptr single_rotation(int_rbt_node_ptr root, int direction) {
  int_rbt_node_ptr tmp = root->child[!direction];

  root->child[!direction] = tmp->child[direction];
  tmp->child[direction]   = root;

  SET_TO_RED(root);
  SET_TO_BLACK(tmp);
  printf("single rot ok\n");
  return tmp;
}

inline int_rbt_node_ptr double_rotation(int_rbt_node_ptr root, int direction) {

  root->child[!direction] = single_rotation(root->child[!direction],
                                            !direction);
  return single_rotation(root, direction);
}

int_rbt_node_ptr int_rbt_insert_recurvise(int_rbt_node_ptr root,
                                                 const int key,
                                                 const int n,
                                                 const int* const values)
{
  if(root == NULL) {
    printf("make node\n");
    root = int_rbt_instantiate_node(key, n, values);
  }
  else if (key == int_rbt_node_ptr_key(root)) {
    for(int i = 0; i < n; ++i)
      root->values[i] += values[i];
  }
  else {
    int direction = key < int_rbt_node_ptr_key(root) ? LEFT : RIGHT;
    printf("key: %i rootkey: %i dir: %i\n", key, int_rbt_node_ptr_key(root), direction);
    root->child[direction] = int_rbt_insert_recurvise(root->child[direction],
                                                      key, n, values);

    printf("Done Recursing\n");

    printf("%i\n",IS_RED(root->child[direction]) );
    printf("%p\n",root->child[direction]);

    if(IS_RED(root->child[direction])) {
      printf("maybe her\n");
      printf("%i\n",IS_RED(root->child[!direction]) );
      printf("%p\n",root->child[!direction]);

      if(IS_RED(root->child[!direction])) {
        printf("ok\n");
        SET_TO_RED(root);
        SET_TO_BLACK(root->child[LEFT]);
        SET_TO_BLACK(root->child[RIGHT]);
        printf("ok\n");
      }
      else {
        printf("ok we here\n");
        if(IS_RED(root->child[direction]->child[direction]))
          root = single_rotation(root, !direction);
        else if(IS_RED(root->child[direction]->child[!direction]))
          root = double_rotation(root, !direction);
        printf("no\n");
      }
    }
  }
  printf("herp\n");
  return root;
}

int_rbt_node_ptr int_rbt_insert(int_rbt_node_ptr root, const int key, const int n,
                                const int * const values)
{
  int_rbt_node_ptr tmp = int_rbt_insert_recurvise(root, key ,n, values);
  SET_TO_BLACK(tmp);
  return tmp;
}

void int_rbt_print_tree(int_rbt_node_ptr root, const int n) {
  if(root != NULL) {
    printf("Key: %i Value(s):", int_rbt_node_ptr_key(root));
    for(int i = 0; i < n; ++i)
      printf(" %i" , root->values[i]);
    printf("\n");
    int_rbt_print_tree(root->child[LEFT], n);
    int_rbt_print_tree(root->child[RIGHT], n);
  }
}
