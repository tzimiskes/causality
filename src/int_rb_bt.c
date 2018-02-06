#include"headers/rapi.h"

#include<stdlib.h>

typedef struct int_rbt_node* int_rbt_node_ptr;
typedef struct int_rbt_node {
  int key;
  int_rbt_node_ptr parent;
  int_rbt_node_ptr left;
  int_rbt_node_ptr right;
  int values[];
}int_rbt_node;

int_rbt_node_ptr instantiate_int_rbt_tree(const int key, const int n,
                                          const int* const values) {
  int_rbt_node_ptr tmp = malloc(sizeof(tmp) + n*sizeof(tmp->values[0]));
  if(tmp == NULL) {
    error("Failed to allocate memeory for rbt_node_ptr!");
  }
  for (int i = 0; i < n; ++i)
    tmp->values[i] = values[i];
  tmp->key    = key;
  tmp->parent = NULL;
  tmp->left   = NULL;
  tmp->right  = NULL;
  return(tmp);
}

int_rbt_node SENTINEL = struct int_rbt_node {
 int key = 0;
 int_rbt_node_ptr parent = NULL;
 int_rbt_node_ptr left = NULL;
 int_rbt_node_ptr right = NULL ;
 int values[] = NULL;
};