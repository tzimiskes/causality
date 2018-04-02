#include"headers/causality.h"
#include"headers/int_linked_list.h"


static inline ill_ptr ill_instantiate(int key, int value) {
  ill_ptr tmp = malloc(sizeof(ill));
  if(tmp == NULL)
    error("Failed to instaniate linked list!\n");
  tmp->key =  key;
  tmp->value = value;
  tmp->child = NULL;
  return(tmp);
}

ill_ptr ill_insert(ill_ptr root, int key, int value) {
  if(root == NULL)
    return(ill_instantiate(key, value));
  else {
    ill_ptr tmp = root;
    while(tmp->child != NULL)
      tmp = tmp->child;
    tmp->child = ill_instantiate(key, value);
    return(root);
  }
}

void ill_insert2(ill_ptr* root, int key, int value, int i, ill_ptr nodes) {
    nodes[i].child = *root;
    nodes[i].key   = key;
    nodes[i].value = value;
    *root          = &nodes[i];
}

/*
 * TODO
 */
void ill_insert_by_value(ill_ptr* root, int key, int value,
                          int i, ill_ptr nodes)
{
  nodes[i].key   = key;
  nodes[i].value = value;
  // if root is null, instantiate
  if(*root == NULL) {
    *root          = &nodes[i];
    return;
  }
  if((*root)->value < value) {
    nodes[i].child = *root;
    *root          = &nodes[i];
    return;
  }
  // else loop through the nodes
  ill_ptr tmp = *root;
  while(tmp->child != NULL) {
    if(tmp->child->value < value) {
      nodes[i].child = tmp->child;
      tmp->child     = &nodes[i];
      return;
    }
    tmp = tmp->child;
  }
  // if child is NULL instantiate it
  tmp->child = &nodes[i];
}

void ill_set_next(ill_ptr root, ill_ptr next) {
  root->child = next;
}

ill_ptr ill_next(ill_ptr root) {
  return(root->child);
}

int ill_key(ill_ptr root) {
  return(root->key);
}

int ill_value(ill_ptr root) {
  return(root->value);
}

void ill_free(ill_ptr root) {
  while(root != NULL) {
    ill_ptr next = root->child;
    free(root);
    root = next;
  }
}

void ill_set_value(ill_ptr root, int new_value) {
  if(root != NULL)
    root->value = new_value;
  else
    error("Cannot assign value to a NULL pointer!\n");
}

ill_ptr ill_search(ill_ptr root, const int key) {
  while(root != NULL) {
    if(root-> key == key)
      return root;
    else
      root = root->child;
  }
  return root; /* root is NULL */
}

ill_ptr* create_ptr_to_ill_ptr(const int n) {
  ill_ptr* hash_table = malloc(n*sizeof(ill_ptr));
  if(hash_table == NULL)
    error("Failed to allocate pointer for ill_ptr*\n");
  for(int i = 0; i < n; ++i)
    hash_table[i] = NULL;
  return(hash_table);
}

ill_ptr create_ill_ptr(const int n) {
  ill_ptr ptr = calloc(n, sizeof(ill));
  if(ptr == NULL)
    error("Failed to allocate pointer for ill_ptr\n");
  return(ptr);
}

void ill_print(ill_ptr root) {
  while(root != NULL) {
    Rprintf("Key: %i Value: %i\n", root->key, root->value);
    root = root->child;
  }
}
