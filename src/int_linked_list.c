#include<stdlib.h>
#include<R.h>
#include<Rinternals.h>


typedef struct int_ll* int_ll_ptr;
// definition of each node in a linked list
typedef struct int_ll {
  R_xlen_t key;
  int value;
  int_ll_ptr child;
} int_ll;

int_ll_ptr int_ll_instantiate(R_xlen_t key, int value) {
  int_ll_ptr tmp = malloc(sizeof(int_ll));
  if(tmp == NULL)
    error("Failed to instaniate linked list!\n");
  tmp->key = key;
  tmp->value = value;
  tmp->child = NULL;
  return(tmp);
}

void int_ll_insert(int_ll_ptr root, R_xlen_t key, int value) {
  while(root->child != NULL)
    root = root->child;
  root->child = int_ll_instantiate(key, value);
}

int_ll_ptr int_ll_next(int_ll_ptr root) {
  return(root->child);
}

R_xlen_t int_ll_key(int_ll_ptr root) {
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

R_xlen_t int_ll_size(int_ll_ptr root) {
  R_xlen_t n = 1;
  if(root == NULL)
    return(0);
  else {
    while((root = root->child) != NULL)
      ++n;
  }
  return(n);
}

void int_ll_set_value(int_ll_ptr root, int value) {
  if(root != NULL)
    root->value = value;
  else
    error("Cannot assign value to a NULL pointer!\n");
}

