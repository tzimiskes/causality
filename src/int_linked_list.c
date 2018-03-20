#include"headers/causality.h"

typedef struct int_ll* int_ll_ptr;
// definition of each node in a linked list
typedef struct int_ll {
  int_ll_ptr child;
  int key;
  int value;
} int_ll;

static inline int_ll_ptr int_ll_instantiate(int key, int value) {
  int_ll_ptr tmp = malloc(sizeof(int_ll));
  if(tmp == NULL)
    error("Failed to instaniate linked list!\n");
  tmp->key =  key;
  tmp->value = value;
  tmp->child = NULL;
  return(tmp);
}

int_ll_ptr int_ll_insert(int_ll_ptr root, int key, int value) {
  if(root == NULL)
    return(int_ll_instantiate(key, value));
  else {
    int_ll_ptr tmp = root;
    while(tmp->child != NULL)
      tmp = tmp->child;
    tmp->child = int_ll_instantiate(key, value);
    return(root);
  }
}

// this function inserts nodes into the linked list by descending value
// I guess its strange that I don't do this by key; I might rewrite it
// if a node goes before root, it changes the values of root to the new node,
// and the makes a new int_ll and sets it to root,
int_ll_ptr int_ll_insert_by_value(int_ll_ptr root, int key, int value) {
  // if root is null, instantiate
  if(root == NULL)
    return(int_ll_instantiate(key, value));
  // if new node should be before root, make new node root
  if(root->value < value) {
    int_ll_ptr new_root = int_ll_instantiate(key, value);
    new_root->child = root;
    return(new_root);
  }
  // else loop through the nodes
  int_ll_ptr tmp = root;
  while(tmp->child != NULL) {
    if(tmp->child->value < value) {
      int_ll_ptr new_node = int_ll_instantiate(key, value);
      new_node->child = tmp->child;
      tmp->child = new_node;
      return(root);
    }
    tmp = tmp->child;
  }
  // if child is NULL instantiate it
  tmp->child = int_ll_instantiate(key, value);
  return(root);
}

int_ll_ptr int_ll_next(int_ll_ptr root) {
  return(root->child);
}

int int_ll_key(int_ll_ptr root) {
  return(root->key);
}

int int_ll_value(int_ll_ptr root) {
  return(root->value);
}

void int_ll_free(int_ll_ptr root) {
  while(root != NULL) {
    int_ll_ptr next = root->child;
    free(root);
    root = next;
  }
}

int_ll_ptr int_ll_delete(int_ll_ptr root, const int key) {
  if(root != NULL) {
    int_ll_ptr tmp = root;
    if(root->key == key) {
      tmp = root->child;
      free(root);
      return(tmp);
    }
    else {
      while(tmp->child != NULL) {
        if(tmp->child->key == key) {
          int_ll_ptr new_child = tmp->child->child;
          free(tmp->child);
          tmp->child = new_child;
          return root;
        }
      }
      error("key not found in linked list!\n");
    }
  }
  else {
    return root; /* aka NULL */
  }
}

int int_ll_size(int_ll_ptr root) {
  int n = 1;
  if(root == NULL)
    return(0);
  else {
    while((root = root->child) != NULL)
      n++;
  }
  return(n);
}

void int_ll_set_value(int_ll_ptr root, int new_value) {
  if(root != NULL)
    root->value = new_value;
  else
    error("Cannot assign value to a NULL pointer!\n");
}

int_ll_ptr int_ll_search(int_ll_ptr root, const int key) {
  while(root != NULL) {
    if(root-> key == key)
      return root;
    else
      root = root->child;
  }
  return root; /* root is NULL */
}