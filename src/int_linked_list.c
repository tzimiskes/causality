#include<stdlib.h>
#include<R.h>
#include<Rinternals.h>

typedef struct int_ll* int_ll_ptr;
// definition of each node in a linked list
typedef struct int_ll {
  int key;
  int value;
  int_ll_ptr child;
} int_ll;

int_ll_ptr int_ll_instantiate(int key, int value) {
  int_ll_ptr tmp = malloc(sizeof(int_ll));
  if(tmp == NULL)
    error("Failed to instaniate linked list!\n");
  tmp->key =  key;
  tmp->value = value;
  tmp->child = NULL;
  return(tmp);
}

void int_ll_insert(int_ll_ptr root, int key, int value) {
  if(root != NULL) {
    while(root->child != NULL)
      root = root->child;
    root->child = int_ll_instantiate(key, value);
  }
}

// this function inserts nodes into the linked list by descending value
// I guess its strange that I don't do this by key; I might rewrite it
// if a node goes before root, it changes the values of root to the new node,
// and the makes a new int_ll and sets it to root,
void int_ll_insert_by_value(int_ll_ptr root, int key, int value) {
  // declare these variables up here in an attempt to save some cycle by
  // encouraging the compliler to store them in register
  // not sure if this works though
  int root_value;
  int_ll_ptr child;
  while(root != NULL) {
    child = root->child;
    if ((root_value = root->value) < value) {
      int root_key = root->key;
      int_ll_ptr tmp = int_ll_instantiate(root_key, root_value);
      tmp->child = child;
      root->key = key;
      root->value = value;
      root->child = tmp;
      return;
    } else if(child == NULL) {
      root->child = int_ll_instantiate(key, value);
      return;
    } else
      root = child;
  }
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
    if(root->key == key) {
      int_ll_ptr tmp = root->child;
      free(root);
      return(tmp);
    } else
      return(int_ll_delete(root->child, key));
  }
  error("cannot find key to delete in linked list!\n");
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